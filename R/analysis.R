library(here)
library(tidyverse)
library(lubridate)

file_path <- here::here("data-raw", "TestExperiment_24-03-2020T13h15.csv")

data <- read_csv(file_path, col_names = TRUE)

data <- 
  data %>%
  mutate(exec_date = as_datetime(exec_timestamp/1000),
         plan_date = as_datetime(planning_timestamp/1000))

# plan_date is the driver field to interpet the date. The same date is twice

time_start <- min(data$plan_date)
time_end <- max(data$plan_date)
# time_elapsed <- ...

# Delay time is in seconds
data_formatted <-
  data %>%
  group_by(task) %>%
  arrange(plan_date) %>%
  mutate(step = row_number(),
         delay = interval(plan_date, exec_date))


p <- 
  filter(data_formatted, delay >= 60 & delay <= 61) %>%
  ggplot(aes(x = step, y=delay, group=task, colour = factor(task))) +
  geom_line() +
  labs(x = "time", y = "delay") + 
  theme_bw()
  
p

