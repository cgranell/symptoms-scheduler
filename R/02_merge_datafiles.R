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

files <- tibble(
  experiment = c(rep("#1", 5), rep("#2", 5)),
  gsheets_name = c("AD_BQ.csv", "AD_NV.csv", "AD_A1.csv", "BA_H9.csv", "BA_MO.csv",
                   "BA_BQ.csv", "BA_NV.csv", "BA_A1.csv", "AD_H9.csv", "AD_MO.csv"),
  gsheets_link = c("https://drive.google.com/open?id=1fP9r0S8ORa689yHRtfCtYQothLuGcCw3",
                   "https://drive.google.com/open?id=1l-vnaT-Smy0SegArIz5A108-IQ2_jmVV",
                   "https://drive.google.com/open?id=1gzk1ezN5t5yBv2RnbznUqvalDx97eu3o",
                   "https://drive.google.com/open?id=1BLXO4Rvz6ppWFJe5oN9Y0hKh__ypofxk",
                   "https://drive.google.com/open?id=1RizGjKR8QLOdJna7qGPP7dYX9WWAysO4",
                   "https://drive.google.com/open?id=1rfaooLZ0Up0gNx1SCA92TV1JFxcLU0sk",
                   "https://drive.google.com/open?id=1_KIXX5FGXv7MO37QbUDnd_KGqiGihiMI",
                   "https://drive.google.com/open?id=1LezrtyB9i4K7cQsmI78E0v_KaOyhZcXI",
                   "https://drive.google.com/open?id=1ISjsfSLZt6n9miryQtDr-tsZV9bfbCDT",
                   "https://drive.google.com/open?id=1bx2RVNz05qVKGr-32GI-ZIOKQWlqKBPz")
)

baseline_delay <- 60

data_path <- here::here("data-raw")
file_names <- list.files(path  = data_path)
file_paths <- list.files(path = data_path, full.names = TRUE)

data_merged <- data.frame()

get_experiment_id <- function(filename) {
  fullname <- paste0(filename, ".csv")
  files %>%
    filter(gsheets_name == fullname) %>%
    select(experiment) %>%
    pull
}

for (f in 1:length(file_names)) {
  filename <- stringr::str_sub(file_names[f], 1, 5)
  exp_id <- get_experiment_id(filename)
  
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
    mutate(exp_id = exp_id,
           scheduler = scheduler_name,
           device_id = device_id,
           device_name = device_name,
           device_desc = device_desc) 
  
  data_merged <- rbind(data_merged, data_temp)
}       


data_merged <- 
  data_merged %>%
  mutate(exec_date = as_datetime(exec_timestamp/1000, tz="Europe/Madrid"),
         plan_date = as_datetime(planning_timestamp/1000, tz="Europe/Madrid"),
         plan_second = lubridate::second(plan_date),
         plan_minute = lubridate::minute(plan_date),
         plan_hour = lubridate::hour(plan_date),
         plan_day = lubridate::day(plan_date),
         plan_month = lubridate::month(plan_date),
         exec_second = lubridate::second(exec_date),
         exec_minute = lubridate::minute(exec_date),
         exec_hour = lubridate::hour(exec_date),
         exec_day = lubridate::day(exec_date),
         exec_month = lubridate::month(exec_date),
         time_period = ifelse(between(plan_hour,0, 7), "nighttime", "daytime"))


# Delay time is in seconds
data_merged <-
  data_merged %>%
  group_by(device_id) %>%
  arrange(plan_date) %>%
  mutate(step = row_number(),
         delay = (as.duration(interval(plan_date, exec_date)) - baseline_delay) / dseconds(1),
         delay = round(delay, 3))

#TODO: Outliers are handeled in notebooks. To update shiny app

# Outliers
# means <- 
#   data_merged %>%
#   group_by(exp_id, device_id) %>%
#   summarise(mean = round(mean(delay), 3),
#             median= round(median(delay), 3),
#             sd = round(sd(delay), 3),
#             # lo = round(mean - 2*sd, 3),
#             # hi = round(mean + 2*sd, 3),
#             lo = round(mean - 2*sd, 3),
#             hi = round(mean + 2*sd, 3)
#             )
# 
# data_complete <- 
#   left_join(data_merged, means, by="device_id") %>%
#   mutate(outlier = ifelse((delay < lo | delay > hi), "yes", "no")) %>%
#   arrange(device_id, plan_date)
data_complete <- data_merged  

data_path <- here::here("data", "data.csv")
write_csv(data_complete, data_path)
data_path <- here::here("data", "data.rds")
saveRDS(data_complete, data_path)

## SO FAR; comment and shiny app is no longer up-to date
# data_path <- here::here("webapp","data", "data.csv")
# write_csv(data_complete, data_path)
# data_path <- here::here("webapp","data", "data.rds")
# saveRDS(data_complete, data_path)

