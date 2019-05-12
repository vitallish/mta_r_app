require(dplyr)
require(lubridate)
require(ggplot2)
library(leaflet)
library(htmltools)
library(tidyr)
library(forcats)
# library(chron)
library(splines)

# Assume database is defined in odbcini file

getPathData <- function(train = "1", direction_name = "S", start_time = "2019-04-01"){
  
  con<-DBI::dbConnect(odbc::odbc(), "mta_gtfs")
  
  
  sched_stops <- tbl(con, "sched_stops")
  trainID <- tbl(con, "trainID")
  enroute_trains <- tbl(con, "enroute_trains")
  stops <- tbl(con, "stops")
  
  
  
  test_query <- sched_stops %>% 
    left_join(trainID, by = "full_id") %>% 
    left_join(stops, by = "stop_id") %>% 
    filter(route_id == train, direction == direction_name, enroute_conf > 0) %>% 
    select(-full_stop_id) %>% 
    filter(arrival >=start_time)
  
  test_query_df <- test_query %>% 
    collect()
  
  DBI::dbDisconnect(con)
  
  d <- test_query_df %>% 
    mutate_at(vars(arrival, departure, timeFeed), ymd_hms) %>% 
    group_by(full_id) %>% 
    arrange(full_id, desc(arrival)) %>% 
    mutate(time_to_arrive = arrival-lag(arrival, order_by = (arrival))) %>% 
    mutate(stop_start = lag(stop_id, order_by = arrival)) %>% 
    mutate(time_to_arrive = as.numeric(time_to_arrive, unit = "secs")) %>% 
    mutate(path = paste("S", stop_start, stop_id, sep = "_")) %>% 
    ungroup() %>% 
    filter(!is.na(time_to_arrive))
  
  d_paths_valid <- d %>% 
    count(path)
  
  d_paths_valid$cluster <- kmeans(d_paths_valid$n, centers = c(1, 1e3))$cluster
  
  d <- d %>% 
    semi_join(d_paths_valid %>% filter(cluster ==2), by = "path")
  
  d %>% 
    mutate(path_order = fct_reorder(path, enroute_conf)) %>% 
    mutate(WEEKEND = as.numeric(chron::is.weekend(arrival))) %>% 
    mutate(TIME_DAY = hour(arrival) + minute(arrival)/60) %>% 
    mutate(WEEKDAY = weekdays(arrival))
}

createModel <-function(x){

  time_forcast <- x %>% select(path_order, arrival, time_to_arrive, WEEKEND, TIME_DAY, WEEKDAY)
  time_forcast <- time_forcast %>% 
    filter(time_to_arrive>0)
  
  glm(time_to_arrive ~ path_order + WEEKEND*ns(TIME_DAY, df = 10), data = time_forcast, family = "poisson")
  
  
}
createPath <- function(start_id, end_id, order){
  
  end_val <- grep(paste0(end_id,"$"), order)
  start_val <- grep(paste0("^S_",start_id), order)
  
  order[start_val:end_val]
  
}

