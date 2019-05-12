require(RMySQL)
require(dplyr)
require(lubridate)
require(ggplot2)
library(leaflet)
library(htmltools)
library(tidyr)




con<-DBI::dbConnect(odbc::odbc(), "mta_gtfs")
dbListTables(con)

sched_stops <- tbl(con, "sched_stops")
trainID <- tbl(con, "trainID")
enroute_trains <- tbl(con, "enroute_trains")
stops <- tbl(con, "stops")


## Live information ----
for(i in 1:1){
cur_time <- Sys.time()
live_query <- sched_stops %>% 
  left_join(trainID, by = "full_id") %>% 
  left_join(stops, by = "stop_id") %>% 
  inner_join(enroute_trains, by = "full_id", suffix = c("", "_enroute")) %>% 
  # left_join(enroute_trains, by = c("full_id", "stop_id")) %>% 
  filter(route_id == "1", direction == "S") %>% 
  select(-full_stop_id)


live_df <- live_query %>%
  collect() 

live_clean <- live_df %>% 
  mutate_at(vars(arrival, departure, timeFeed, last_ping), ymd_hms, tz =  Sys.timezone()) %>% 
  mutate(cur_time = Sys.time())

current_train_pos <- live_clean %>% 
  group_by(full_id) %>% 
  mutate(time_pinged = cur_time -last_ping) %>% 
  mutate(time_since_depart = as.numeric(cur_time - lag(departure, order_by = stop_sequence), unit = "mins")) %>% 
  mutate(arriving_in = as.numeric(arrival - cur_time, unit = "mins")) %>% 
  mutate(perc_there = case_when(current_status == 'STOPPED_AT' ~ 1,
                                TRUE ~ time_since_depart/(time_since_depart + arriving_in))) %>% 
  mutate(guess_lat = lag(stop_lat, order_by = stop_sequence) + (stop_lat - lag(stop_lat, order_by = stop_sequence))*(perc_there)) %>% 
  mutate(guess_lon = lag(stop_lon, order_by = stop_sequence) + (stop_lon - lag(stop_lon, order_by = stop_sequence))*(perc_there)) %>% 
  
  filter(stop_id_enroute == stop_id) %>% 
  mutate(PINGED = paste0("PINGED: ", round(as.numeric(cur_time-last_ping, unit = "secs")))) %>% 
  mutate(ARRIVAL = paste0("Arrival: ", round(arriving_in, 1), " min")) %>% 
  mutate(LEFT = paste0("Left: ", round(time_since_depart,1), " min")) %>% 
  mutate(station_color = case_when(current_status == "STOPPED_AT" ~ "green",
                                   current_status == "INCOMING_AT" ~ "yellow",
                                   TRUE ~ "blue"))

all_stations <- live_clean %>% 
  distinct(stop_name, stop_id, stop_lat, stop_lon)




output <- leaflet() %>% 
  addCircleMarkers(~stop_lon, ~stop_lat, data = all_stations, radius = 2, color = "black", popup = ~htmlEscape(stop_name)) %>%  
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(~guess_lon, ~guess_lat, data = current_train_pos, color = ~station_color, popup = ~(
    paste(PINGED, 
          paste0(current_status, ": ", stop_name),
          ARRIVAL,
          LEFT,
          
          sep = "<br>")
  ))

print(output)
Sys.sleep(15)

}

## Historic Information ----
test_query <- sched_stops %>% 
  left_join(trainID, by = "full_id") %>% 
  left_join(stops, by = "stop_id") %>% 
  filter(route_id == "1", direction == "S", enroute_conf > 0) %>% 
  select(-full_stop_id) %>% 
  filter(arrival >="2019-01-01")
  

test_query_df <- test_query %>% 
    collect()

library(forcats)

d <- test_query_df %>% 
  mutate_at(vars(arrival, departure, timeFeed), ymd_hms) %>% 
  group_by(full_id) %>% 
  arrange(full_id, desc(arrival)) %>% 
  mutate(time_to_arrive = arrival-lag(departure, order_by = (arrival))) %>% 
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

d <- d %>% 
  mutate(path_order = fct_reorder(path, enroute_conf))
# Brute force it bro ----
full_order <- levels(d$path_order)

only_clean <- d %>% select(full_id, path_order, time_to_arrive) %>% 
  spread(path_order, time_to_arrive) %>% 
  filter(complete.cases(.))

# create forumals
all_mods <- list()
for(i in 2:length(full_order)){
  output_var <- full_order[i]
  predictors <- paste(full_order[1:(i-1)], collapse = " + ")
  full_form <- as.formula(paste(output_var, predictors, sep = "~"))
  
  full_mod <- glm(full_form, data = only_clean)
  all_mods[[output_var]] <- step(full_mod, trace = 0)
  
}


library(broom)
library(purrr)
map_df(all_mods, tidy, .id = "output") %>% 
  filter(term != '(Intercept)') %>% 
  count(term) %>% 
  View()





# Only control for day and time


time_forcast <- d %>% select(path_order, arrival, time_to_arrive)

library(chron)
library(splines)
time_forcast <- time_forcast %>% 
  mutate(WEEKEND = as.numeric(is.weekend(arrival))) %>% 
  mutate(TIME_DAY = hour(arrival) + minute(arrival)/60) %>% 
  mutate(WEEKDAY = weekdays(arrival)) %>% 
  filter(time_to_arrive>0)

full_time_mod <- glm(time_to_arrive ~ path_order + WEEKEND*ns(TIME_DAY, df = 10), data = time_forcast, family = "poisson")
# full_time_mod <- glm(time_to_arrive ~ path_order + WEEKEND*((sin(TIME_DAY/24*2*pi)^2+ cos(TIME_DAY/24*2*pi)^2), degree = 2 ), 
#                      data = time_forcast, family = "gaussian")

expand.grid(
  path_order = full_order[],
  WEEKEND = c(1,0),
  TIME_DAY = seq(0,24, length.out = 100)
) %>% 
  mutate(modeled_time = predict(full_time_mod, ., type = "response")) %>% 
  mutate(WEEKEND = as.factor(WEEKEND)) %>% 
  ggplot(aes(x = TIME_DAY, y = modeled_time, color = WEEKEND)) +
  geom_line()


for(i_path_order in full_order){
p <- time_forcast %>% 
  mutate(preds = fitted.values(full_time_mod)) %>% 
  filter(path_order == i_path_order) %>% 
  ggplot(aes(x = TIME_DAY, y = preds - time_to_arrive, color = path_order)) +
  geom_line() +
  facet_wrap(~floor_date(arrival, unit = "days"), scales = "free") +
  coord_cartesian(ylim = c(-50,50)) +
  labs(main = i_path_order)  

ggsave(paste0(i_path_order,".png"), p, device = "png", path = "output")
}


# Let's try to define correlation structure
d_mod <- d %>% 
  select(full_id, path_order, time_to_arrive) %>% 
  group_by(path_order) %>% 
  filter(n()>200) %>% 
  ungroup() %>% 
  filter(!is.na(time_to_arrive)) %>% 
  spread(path_order, time_to_arrive) %>% 
  filter(!is.na(S_103S_104S)) %>% 
  mutate_if(is.numeric, ~as.numeric(scale(., scale = FALSE)))

d_mod2 <- d_mod %>% 
  select(S_103S_104S, S_104S_106S,S_106S_107S,S_107S_108S) %>% 
  filter(complete.cases(.))

lm(S_104S_106S ~ S_103S_104S, data = d_mod2)
lm(S_106S_107S ~ S_103S_104S + S_104S_106S, data = d_mod2)
lm(S_107S_108S ~ S_106S_107S + S_103S_104S + S_104S_106S, data = d_mod2)


d_cor_test <- d %>% 
  # filter(full_id == '20190402_099300_1..S03R') %>% 
  select(full_id, arrival, stop_id)

d_full_mod <- d_cor_test %>% 
  inner_join(d_cor_test, by = "full_id", suffix = c("_start", "_end")) %>% 
  mutate(travel_time = arrival_end - arrival_start,
         travel_time = as.numeric(travel_time, unit = "secs")) %>% 
  select(full_id, stop_id_start,stop_id_end, travel_time ) %>% 
  filter(travel_time >0)

d_full_mod_simple <- d_full_mod %>% 
  group_by(stop_id_end, stop_id_start) %>% 
  tally()

d_full_mod_simple$clusters <- (d_full_mod_simple %>% pull(n) %>% kmeans(c(1,1e3)))$cluster

d_full_mod_filtered <- d_full_mod %>% 
  semi_join(d_full_mod_simple %>% filter(clusters == 2))


full_mod <- glm(travel_time ~ stop_id_start:stop_id_end -1, data = d_full_mod_filtered)



d %>% 
  filter(stop_id %in% c('114S', '127S')) %>% 
  mutate(time_to_arrive = arrival-lag(departure, order_by = (arrival))) %>% 
  filter(!is.na(time_to_arrive)) %>% 
  filter(year(arrival) >=2019) %>% 
  filter(time_to_arrive < 5e3) %>% 
  ggplot(aes(x = hour(arrival), y = as.numeric(time_to_arrive)/60)) +
  geom_boxplot(aes(group = hour(arrival)))



sched_stops_clean <- sched_stops %>% 
  mutate(arrival = ymd_hms(arrival), departure = ymd_hms(departure),
         timeFeed = ymd_hms(timeFeed))

sched_stops_clean$departure[sched_stops_clean$departure<0] <-NA
sched_stops_clean$arrival[sched_stops_clean$arrival<0] <-NA

trainID_clean <- trainID %>% 
  mutate(start_date  = ymd(start_date) )

full_df <- sched_stops_clean %>% 
  left_join(stops, by ="stop_id") %>% 
  left_join(trainID_clean, by = 'full_id') %>% 
  filter(enroute_conf != 0)

# filter_options needs to be loaded into a db and updated periodically
filter_options <- full_df %>% 
  select(route_id, direction, stop_id, stop_name) %>%
  distinct(route_id, direction, stop_id, stop_name)
  

full_df_all<- sched_stops_clean %>% 
  left_join(stops, by ="stop_id") %>% 
  left_join(trainID_clean, by = 'full_id')

trip_in_question <- full_df %>% 
  filter(route_id == "1", direction == "S")

trip_begin <- trip_in_question %>% 
  filter(stop_name == "96 St") %>% 
  select(full_id, station_leave_time = departure) %>% 
  mutate(station_leave_hour = hour(station_leave_time)) %>% 
  mutate(station_leave_cut = cut(station_leave_hour,c(-.1,7,9,17,19,24)))
  
full_travel_time <- trip_in_question %>% 
  inner_join(trip_begin, by='full_id') %>% 
  mutate(time_in_transit = arrival - station_leave_time) %>% 
  group_by(full_id) %>% 
  arrange(timeFeed, enroute_conf)

stop_med <- full_travel_time %>% group_by(stop_name) %>% 
  summarise(med_travel = median(time_in_transit,na.rm = T)) %>%
  arrange(as.numeric(med_travel)) %>% 
  mutate(stop_num = row_number(stop_name))
  
full_travel_time %>%  
  ggplot(data = ., aes(x = stop_name, y = as.numeric(time_in_transit), 
                       color = station_leave_cut))+
  geom_point(position = "jitter", alpha = 0.2) +
  stat_summary(fun.y=mean, geom = "point", size = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  scale_x_discrete(limits = stop_med$stop_name)
 
full_travel_time %>%
  filter(stop_name == "Chambers St") %>% 
  group_by(station_leave_cut) %>% 
  select(full_id, arrival, departure, time_in_transit) %>% 
  summarise(avg_travel = mean(as.numeric(time_in_transit))/60, n())

full_travel_time %>%
  filter(stop_name == "Chambers St") %>% 
  ggplot(data = ., aes(x = as.numeric(time_in_transit))) +
  geom_density(alpha = 0.2)


getNextTrainsatStop <-function(enroute_df, full_df_all, stop_interest, 
                               train_lines, dir_interest, num_trains, 
                               time_start = force_tz(now(),tzone = "UTC")){
  train_ids<- enroute_df$full_id
  full_df_all %>% 
    filter(stop_name == stop_interest,
           full_id %in% train_ids,
           route_id %in% train_lines, 
           departure > time_start,
           direction == dir_interest) %>%
    top_n(num_trains,departure)
}

# figure out how long between trains

getTimeBetweenTrains <-function(full_df, stop_interest, 
                                train_lines, dir_interest){
  
  full_df %>% 
    filter(stop_name == stop_interest,
           route_id %in% train_lines, 
           direction == dir_interest) %>%
    arrange(departure) %>% 
    mutate(time_between = lead(departure)-departure)
}

#density by hour, train time wait
OK <- getTimeBetweenTrains(full_df, "96 St", c("1"),"S") %>% filter(time_between<5000) %>% mutate(time_between = as.numeric(time_between))
OK %>% mutate(station_leave_hour = hour(departure)) %>% mutate(station_leave_cut = cut(station_leave_hour,c(-.1,7,9,17,19,24))) %>% qplot(time_between, data = ., color = station_leave_cut,geom = "density")

qplot(data = OK, x = arrival, y = time_between, color = hour(arrival))
#time difference between different trains, for example: time difference between 1 train arrival and 2/3 train departure

getTimeTransfer <- function(full_df, arriving_trains = "1", transfer_trains = c("2","3"), dir = "S", 
                            transfer_stop = "96 St", minimum_time  = seconds(0)){
  trains_of_interest <- full_df %>% 
    filter(route_id %in% c(arriving_trains, transfer_trains), 
           stop_name == transfer_stop, direction == dir) %>% 
    mutate(toi =ifelse(route_id %in% arriving_trains,arrival,departure)) %>%
    mutate(arriving_bool = ifelse(route_id %in% arriving_trains, T, F)) %>% 
    arrange(toi) %>% 
    mutate(time_wait = NA)
  
  
  
  
  for(row in 1:nrow(trains_of_interest)){
    spec_row <- trains_of_interest[row,]
    if(spec_row$arriving_bool){
      temp_row_num <- row
      while(trains_of_interest$arriving_bool[temp_row_num]){
        temp_row_num = temp_row_num + 1
      }
      trains_of_interest$time_wait[row] <- 
        trains_of_interest$toi[temp_row_num] - spec_row$toi
      
    }
    
  }
  
  trains_of_interest
  
}

getTimeTransfer(full_df) %>% filter(!is.na(time_wait)) %>% qplot(data = ., x = arrival, y = time_wait, color = hour(arrival))


