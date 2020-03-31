
# install.packages(c("here", "tidyverse", "lubridate"))
library(here)
library(tidyverse)
library(lubridate)

file_path <- here::here("data-raw", "Dummy1minA1_30-03-2020T12h23.csv")

scheduler_type <- "advanced"
day_exp <- 1
device_id <- "A1"
baseline_delay <- 60

data <- read_csv(file_path, col_names = TRUE)

data <- 
  data %>%
  mutate(scheduler = scheduler_type,
         device = device_id,
         day = day_exp,
         exec_date = as_datetime(exec_timestamp/1000),
         plan_date = as_datetime(planning_timestamp/1000))


# Delay time is in seconds
data_formatted <-
  data %>%
  group_by(device) %>%
  arrange(plan_date) %>%
  mutate(step = row_number(),
         delay = as.duration(interval(plan_date, exec_date)) - baseline_delay)


# Outliers
# TODO:to optimise 

mean <- mean(data_formatted$delay)
sd <- sd(data_formatted$delay)
interval <- c(mean - 2*sd, mean + 2*sd) 

data_formatted <-
  data_formatted %>%
  mutate(outlier = ifelse((delay < interval[1] | delay > interval[2]), "yes", "no"))


data_path <- here::here("data", "Dummy1minA1_30-03-2020T12h23.csv")
write_csv(data_formatted, data_path)
data_path <- here::here("data", "Dummy1minA1_30-03-2020T12h23.rda")
saveRDS(data_formatted, data_path)
