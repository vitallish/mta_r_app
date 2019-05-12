source('2_functions2.R')
d <- getPathData() 



current_time <- Sys.time()
hours_check <- 24

look_back_time <- current_time-lubridate::hours(hours_check)


model <- createModel(d %>% filter(arrival < (current_time-lubridate::hours(hours_check))))

recent_data <- d %>% filter(time_to_arrive > 0,
             arrival >= (current_time-lubridate::hours(hours_check)),
             arrival <= current_time)


path_of_interest <- createPath(  order = levels(d$path_order),
                                 start_id = "114S",
                                 end_id = "127S"
)



data_to_eval <- recent_data %>% 
  mutate(EXP = predict(model, recent_data, type = "response")) %>% 
  select(full_id, path_order, time_to_arrive, EXP, arrival)

# 
# data_to_eval %>% 
#   filter(path_order %in% path_of_interest) %>% 
#   group_by(full_id) %>% 
#   filter(n()==length(path_of_interest)) %>% # only look at trains with all stops
#   summarise(resid = sum(time_to_arrive - EXP), arrival = min(arrival)) %>% 
#   ggplot(aes(x = arrival, y = resid)) +
#   geom_point() +
#   geom_smooth()


data_to_eval %>% 
  filter(path_order %in% path_of_interest) %>% 
  group_by(full_id) %>% 
  filter(n()==length(path_of_interest)) %>% # only look at trains with all stops
  summarise(resid = sum(time_to_arrive - EXP), 
            actual = sum(time_to_arrive),
            pred = sum(EXP),
            arrival = min(arrival)) %>% 
  gather("type", "time", resid, pred, actual) %>% 
  mutate(time = time/60) %>% 
  mutate(scale_type = ifelse(type %in% c("resid"), "resid", "full")) %>% 
  ggplot(aes(x = arrival, y = time, color = type)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~scale_type, scales = "free") +
  geom_hline(data = data.frame(scale_type = "resid", yint = 0), aes(yintercept = yint))
  
## Time between trains ----
start_station = "114S"

d %>% filter(stop_id == start_station) %>% 
  mutate(time_to_prev_train = arrival - lag(arrival, order_by = arrival),
         time_to_prev_train = as.numeric(time_to_prev_train, unit = "secs")) %>% 
  filter(time_to_prev_train <quantile(time_to_prev_train, .99, na.rm = T)) %>% 
  filter(arrival >= look_back_time -lubridate::hours(24)) %>% 
  ggplot(aes(x = arrival, y = time_to_prev_train/60)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ ns(x, df = 10))

