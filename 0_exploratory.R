require(readr)
require(dplyr)
require(lubridate)
require(ggplot2)
require(lubridate)

safeDateConvert <- function(df, columns){
require(lubridate)
  df[columns] <- 
    lapply(df[columns], parse_date_time2, orders = "Ymd HMS")
  
  df
}



sched_stops <- read_csv('dumps/sept2_scheduled.csv',col_types = "cccccc") %>%
  safeDateConvert(c('arrival', 'departure', 'timeFeed'))
sched_stops <- cbind(sched_stops, getTrainDetails(sched_stops$full_id)) %>% tbl_df
na_departure <- is.na(sched_stops$departure)
sched_stops$departure[na_departure] <- sched_stops$arrival[na_departure]

 
enroute_trains <-read_csv('dumps/sept2_enroute.csv', col_types ="iiccccc") %>% 
  safeDateConvert(c('timeFeed','last_ping'))
enroute_trains <- cbind(enroute_trains, getTrainDetails(enroute_trains$full_id)) %>% tbl_df


enroute_trains %>% filter(full_id==trains[450], stop_id=="120S")
sched_stops %>% filter(full_id ==trains[450],stop_id=="120S") %>% tail(5)

getTrainDetails <-function(full_id){
  nice<- str_split_fixed(full_id,pattern="[/.]{2}|[_]", n = 4)
  nice <- cbind(nice, substr(nice[,4],1,1),substr(nice[,4],2,30))
  out<- as.data.frame(x = nice[,c(1:3,5,6)], stringsAsFactors=F)
  colnames(out) <- c("start_date","start_time","line","direction","route_id")
  
  out
}




sched_stops %>% 
  filter(full_id ==trains[450]) %>% 
  group_by(stop_id) %>%
  top_n(1,timeFeed) %>% 
  ungroup() %>% 
  mutate(stop_num = row_number(timeFeed)) %>% 
  #left_join(enroute_trains,by = c('full_id','timeFeed','stop_id','stop_name')) %>% 
  ggplot(data = ., aes(x = reorder(stop_name,order(stop_num)), y = departure))+
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


enroute_trains %>% 
  filter(full_id ==trains[450]) %>% 
  group_by(stop_id) %>%
  top_n(1,last_ping) %>% 
  top_n(1, timeFeed) %>% 
  ungroup %>% 
  mutate(stop_num = row_number(last_ping)) %>% 
  ggplot(data = ., aes(x = reorder(stop_name,order(stop_num)), y = last_ping))+
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


sched_stops %>% 
  filter(line =="1", direction == "S") %>% 
  group_by(full_id,stop_id) %>%
  top_n(1,timeFeed) %>% 
  ungroup() %>% 
  mutate(stop_num = row_number(timeFeed)) %>% 
  #left_join(enroute_trains,by = c('full_id','timeFeed','stop_id','stop_name')) %>% 
  ggplot(data = ., aes(x = reorder(stop_name,order(stop_num)), y = departure))+
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plotLine <- function(sched_stops, start_station, in_line, in_direction){
  specific_trains <- sched_stops %>% 
    filter(line ==in_line, direction == in_direction)
  
  trains_firststop <-
    specific_trains %>% 
    filter(stop_name==start_station) %>% 
    select(full_id) %>% distinct
  
  time_0 <-  
    specific_trains %>% 
    filter(full_id %in% trains_firststop$full_id, stop_name ==start_station) %>% 
    group_by(full_id) %>%
    top_n(1,timeFeed) %>% 
    distinct(full_id) %>% 
    select(full_id, time0_arrival=arrival,time0_departure=departure)

  
  specific_stops <- 
    specific_trains %>% 
    filter(full_id %in% trains_firststop$full_id) %>% 
    group_by(full_id,stop_id) %>%
    top_n(1,timeFeed) %>% 
    distinct(full_id, stop_id) %>% 
    left_join(time_0, by ='full_id') %>% 
    ungroup() %>% 
    #group_by(full_id) %>% 
    # how can we make this normalized to a specific station
    # have a table with full_id and departure
    mutate(timeDiff = departure-time0_departure) 

  station_summary <- specific_stops %>% 
    group_by(stop_name) %>% 
    summarise(mean_travel = mean(timeDiff), count = n()) %>% 
    arrange(mean_travel)
  
  specific_stops %>% 
    mutate(stop_num = row_number(timeFeed)) %>% 
    ggplot(data = ., aes(x = stop_name, y = as.numeric(timeDiff)))+
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_x_discrete(limits = station_summary$stop_name)
  
}










  
## How is there a cortlandt st on the 1 line?
enroute_trains %>% 
  filter(line=="1", direction =="S", stop_name=="Cortlandt St")





