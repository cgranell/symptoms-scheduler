library(here)
library(tidyverse)
library(lubridate)

file_path <- here("test.csv")

data <- read_csv(file_path, col_names = TRUE)


data <- 
  data %>%
  mutate(exec_date = as_datetime(exec_timestamp/1000),
         plan_date = as_datetime(planning_timestamp/1000))

all <-
  data %>%
  group_by(task) %>%
  arrange(plan_date) %>%
  mutate(step = row_number(),
         delay = interval(plan_date, exec_date))


p_all <- 
  filter(all, delay >= 60) %>%
  ggplot(aes(x = step, y=delay, group=task, colour = factor(task))) +
  geom_line() +
  labs(x = "time", y = "delay") + 
  theme_bw()
  
p_all

