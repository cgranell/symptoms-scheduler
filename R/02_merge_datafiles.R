
# install.packages(c("here", "tidyverse", "lubridate", "stringr"))
library(here)
library(tidyverse)
library(lubridate)
library(stringr)


schedulers <- c("AD" = "Advanced", 
                "BA" = "Basic")

devices <- c("BQ" = "BQ Aquaris V",
             "NV" = "Nvidia Shield Tablet",
             "A1" = "Xiaomi Mi A1",
             "H9" = "Honor 9",
             "MO" = "Motorola Moto G")

baseline_delay <- 60


data_path <- here::here("data-raw")
file_names <- list.files(path  = data_path)
file_paths <- list.files(path = data_path, full.names = TRUE)

data_merged <- data.frame()

for (f in 1:length(file_names)) {
  filename <- stringr::str_sub(file_names[f], 1, 5)
  
  parts <- stringr::str_split(filename, "_", simplify = TRUE)
  
  scheduler_id = parts[1]
  scheduler_name <- schedulers[[scheduler_id]]
  device_id <- parts[2]
  device_name <- devices[[device_id]]
  device_desc <- paste0(scheduler_name, " - ", device_name)
  
  data_temp <- read_csv(file_paths[f], col_names = TRUE, 
                        cols(
                          battery = col_double(),
                          exec_timestamp = col_double(),
                          planning_timestamp = col_double(),
                          task = col_character()
                        ))


  data_temp <- 
    data_temp %>%
    mutate(scheduler = scheduler_name,
           device_id = device_id,
           device_name = device_name,
           device_desc = device_desc) 
  
  data_merged <- rbind(data_merged, data_temp)
}       


data_merged <- 
  data_merged %>%
  mutate(#exec_date = as_datetime(exec_timestamp/1000),
         #plan_date = as_datetime(planning_timestamp/1000),
         exec_date = as_datetime(exec_timestamp/1000, tz="Europe/Madrid"),
         plan_date = as_datetime(planning_timestamp/1000, tz="Europe/Madrid"),
         plan_hour = lubridate::hour(plan_date),
         plan_day = lubridate::day(plan_date),
         plan_month = lubridate::month(plan_date),
         time_period = ifelse(between(plan_hour,0, 7), "nighttime", "daytime"))


# Delay time is in seconds
data_merged <-
  data_merged %>%
  group_by(device_id) %>%
  arrange(plan_date) %>%
  mutate(step = row_number(),
         delay = (as.duration(interval(plan_date, exec_date)) - baseline_delay) / dseconds(1))


# Outliers
means <- 
  data_merged %>%
  group_by(device_id) %>%
  summarise(mean = round(mean(delay), 3),
            sd = round(sd(delay), 3),
            lo = round(mean - 2*sd, 3),
            hi = round(mean + 2*sd, 3))

data_complete <- 
  left_join(data_merged, means, by="device_id") %>%
  mutate(outlier = ifelse((delay < lo | delay > hi), "yes", "no")) %>%
  arrange(device_id, plan_date)
  

data_path <- here::here("data", "data.csv")
write_csv(data_complete, data_path)
data_path <- here::here("data", "data.rds")
saveRDS(data_complete, data_path)

data_path <- here::here("webapp","data", "data.csv")
write_csv(data_complete, data_path)
data_path <- here::here("webapp","data", "data.rds")
saveRDS(data_complete, data_path)


# data_path <- here::here("data", "data.rda")
# saveRDS(data_complete, data_path)
