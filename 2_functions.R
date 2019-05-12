require(RMySQL)
require(dplyr)
require(lubridate)
require(ggplot2)

updateTables <- function(tablesToUpdate = c("sched_stops", "trainID", "enroute_trains","stops")){
  load("dbConstants.RData")
  drv<-dbDriver("MySQL")
  
  names(tablesToUpdate)<-tablesToUpdate 
  
  con<-dbConnect(drv,
                 username = USERNAME,
                 password = PASSWORD,
                 host= HOST,
                 port = PORT,
                 dbname = DBNAME)
  
  
  output <- lapply(as.list(tablesToUpdate), FUN = function(x){
    tbl_df(dbReadTable(con, x))
  })
  
  dbDisconnect(con)
  
  output

}

cleanSchedStops <-function(sched_stops){
  sched_stops_clean <- sched_stops[,2:7] %>% 
    mutate(arrival = ymd_hms(arrival), departure = ymd_hms(departure),
           timeFeed = ymd_hms(timeFeed), stop_id = substr(stop_id,1,3))
  
  sched_stops_clean$departure[sched_stops_clean$departure<0] <-NA
  sched_stops_clean$arrival[sched_stops_clean$arrival<0] <-NA
  
  sched_stops_clean
  
}

cleanTrainId <-function(trainID){
  trainID %>% 
    mutate(start_date  = ymd(start_date) )
}

cleanStops <-function(df){
  df %>% filter(nchar(stop_id)==3)
}

filterTrainIds <-function(clean_trainid, route, dir_interest){
  matched_train_ids <- clean_trainid %>% 
    filter(route_id %in% route, direction == dir_interest) %>% 
    select(full_id)
  
  matched_train_ids$full_id
}

getTrainTravelTime <-function(clean_sched_stops, clean_trainid, route, dir, start_station){
  # Outputs data frame where start_station is the beginning of a trip
  # Negative Travel time indicates previous stops
  # Output includes unconfirmed routes (enroute_conf == 0, filter these out
  # for correct historical data)
  
  matched_train_ids <- clean_trainid %>% 
    filter(route_id %in% route, direction == dir) %>% 
    select(full_id)
  
  hist_stops <- clean_sched_stops %>% 
    filter(full_id %in% matched_train_ids$full_id)
  
  trip_begin <- hist_stops %>% 
    filter(stop_id == start_station) %>% 
    select(full_id, station_leave_time = departure) 

  
  hist_stops %>% 
    inner_join(trip_begin, by='full_id') %>% 
    mutate(time_in_transit = as.numeric(arrival - station_leave_time)) %>% 
    group_by(full_id) %>% 
    arrange(timeFeed, enroute_conf)
  
}



getNextTrainsatStop <-function(enroute_df, sched_stop, clean_trainid, stop_interest, 
                               route, dir_interest, num_trains, 
                               time_start = force_tz(now(),tzone = "UTC")){
  
  
  
  matched_train_ids <-  filterTrainIds(clean_trainid, route, dir_interest)
    
  
  train_ids<- matched_train_ids[matched_train_ids %in% enroute_df$full_id]
  
  sched_stop %>% 
    filter(stop_id == stop_interest,
           full_id %in% train_ids,
           departure > time_start) %>%
    top_n(num_trains,departure)
}
# getNextTrainsatStop(output$enroute_trains, clean_sched_stop, clean_trainid, "120S", c("1","2"), "S", 5, time_of_interest)

# TODO route ids are not available to the output.
  # should there be a merge or should they be calculated from the id?
getTimeBetweenTrains <-function(sched_stop, clean_trainid, stop_interest, 
                                route, dir_interest){
  
  matched_train_ids <- filterTrainIds(clean_trainid, route, dir_interest)
  
  sched_stop %>% 
    filter(enroute_conf != 0,
        stop_id == stop_interest,
           full_id %in% matched_train_ids) %>%
    arrange(departure) %>% 
    mutate(time_between = lead(departure)-departure)
}





