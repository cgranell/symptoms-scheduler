library(here)
library(tidyverse)
library(lubridate)
library(manipulate)

file_path <- here::here("data-raw", "PreExperiment_26-03-2020T15h49.csv")

data <- read_csv(file_path, col_names = TRUE)

data <- 
  data %>%
  mutate(exec_date = as_datetime(exec_timestamp/1000),
         plan_date = as_datetime(planning_timestamp/1000))

# plan_date is the driver field to interpet the date. The same date is twice

time_start <- min(data$plan_date)
time_end <- max(data$plan_date)
time_elapsed <- interval(time_start, time_end)
duration <- round(as.duration(time_elapsed) / dminutes(1), 2) # dhours(1), ddays(1)


baseline_delay <- 60

# Delay time is in seconds
data_formatted <-
  data %>%
  group_by(task) %>%
  arrange(plan_date) %>%
  mutate(step = row_number(),
         delay = as.duration(interval(plan_date, exec_date)) - baseline_delay)


data_formatted <- filter(data_formatted, delay <= 10)

ylim <- c(min(data_formatted$delay), max(data_formatted$delay))
ybks <- seq(ylim[1], ylim[2], 0.05)
xlim <- c(min(data_formatted$step), max(data_formatted$step))
xbks <- seq(xlim[1], xlim[2], 4)

ggplot(data_formatted, aes(x = step, y=delay)) +
  geom_line(alpha = 0.6, size = 0.5) +
  labs(title=paste0("Start: ", time_start, " - End: ", time_end),
       x = "time steps [minutes]", 
       y = "delay [seconds]") + 
  scale_x_continuous(breaks=xbks, limits=xlim) +
  scale_y_continuous(breaks=ybks, limits=ylim) +
  theme_bw()



manipulate(
  filter(data_formatted, task == task_selected) %>%
  ggplot(aes(x = step, y=delay)) +
    geom_line(alpha = 0.6, size = 0.5) +
    labs(x = "time steps", y = "delay [seconds]") + 
    scale_x_continuous(breaks=xbks, limits=xlim) +
    scale_y_continuous(breaks=ybks, limits=ylim) +
    theme_bw(),
  task_selected = picker("GPS", "dummy", label="Type of task") 
  )
  
p

