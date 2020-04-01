
# install.packages(c("here", "tidyverse", "lubridate", "stringr"))
library(here)
library(tidyverse)
library(lubridate)
library(stringr)


schedulers <- c("AV" = "advanced", 
                "BA" = "basic")

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
  filename <- stringr::str_sub(file_names[f], 1, 8)

  parts <- stringr::str_split(filename, "_", simplify = TRUE)
  
  scheduler_type <- parts[1]
  day_exp <- as.integer(parts[2])
  device_id <- parts[3]

  device_name <- devices[[device_id]]
  data_temp <- read_csv(file_paths[f], col_names = TRUE)


  data_temp <- 
    data_temp %>%
    mutate(scheduler = scheduler_type,
           device_id = device_id,
           device_name = device_name,
           day = day_exp) 
  
  data_merged <- rbind(data_merged, data_temp)
}       


data_merged <- 
  data_merged %>%
  mutate(exec_date = as_datetime(exec_timestamp/1000),
         plan_date = as_datetime(planning_timestamp/1000))


# Delay time is in seconds
data_merged <-
  data_merged %>%
  group_by(device_id) %>%
  arrange(plan_date) %>%
  mutate(step = row_number(),
         delay = as.duration(interval(plan_date, exec_date)) - baseline_delay)


# Outliers
means <- 
  data_merged %>%
  group_by(device_id) %>%
  summarise(mean = mean(delay),
            sd = sd(delay),
            lo = mean - 2*sd,
            hi = mean + 2*sd)

data_complete <- 
  left_join(data_merged, means, by="device_id") %>%
  mutate(outlier = ifelse((delay < lo | delay > hi), "yes", "no")) %>%
  arrange(device_id, plan_date)
  

data_path <- here::here("data", "data.csv")
write_csv(data_complete, data_path)
data_path <- here::here("webapp","data", "data.csv")
write_csv(data_complete, data_path)

# data_path <- here::here("data", "data.rda")
# saveRDS(data_complete, data_path)
