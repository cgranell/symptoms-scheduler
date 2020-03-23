library(here)
library(tidyverse)
library(lubridate)

file_path <- here("test.csv")

data <- read_csv(file_path, col_names = TRUE)


gps <-
  data %>%
  filter(task == "GPS") %>%
  mutate(delay = abs(exec_timestamp - planning_timestamp),
         step = n() - row_number())


dummy <-
  data %>%
  filter(task == "dummy") %>%
  mutate(delay = abs(exec_timestamp - planning_timestamp),
         step = n() - row_number())


all <- bind_rows(gps, dummy) %>% arrange(step)


p_all <- all %>%
  ggplot(aes(x = step, y=delay, group=task, colour = factor(task))) +
  geom_line() +
  labs(x = "time", y = "delay") + 
  theme_bw()
  
p_all
  