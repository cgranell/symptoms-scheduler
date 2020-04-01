library(here)
library(tidyverse)
library(lubridate)
library(manipulate)


file_path <- here::here("data", "data.csv")

data <- read_csv(file_path, col_names = TRUE)

# plan_date is the driver field to interpet the date. The same date is twice

sel_device <- "H9"

selection <- 
  data %>%
  filter(device_id == sel_device)

title_plot <- sel_device
time_start <- min(selection$plan_date)
time_end <- max(selection$plan_date)
time_elapsed <- interval(time_start, time_end)
duration <- round(as.duration(time_elapsed) / dhours(1), 2) # dhours(1), ddays(1)

# 
# # get rid of outlier
# n_records <- max(data$step)
# n_outliers <- nrow(filter(data, outlier == "yes"))
# 
# cat(paste0("outliers: ", round(n_outliers / n_records, 2), "%"))




ylim_delay <- c(min(selection$delay), max(selection$delay))
ybks_delay <- seq(ylim_delay[1], ylim_delay[2], (ylim_delay[2] - ylim_delay[1])/ 10)
xlim <- c(min(selection$step), max(selection$step))
xbks <- seq(xlim[1], xlim[2], 100)

ylim_battery <- c(min(selection$battery), max(selection$battery))
ybks_battery <- seq(0, 100, 10)

# Dual-scale plot
# https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
scalefactor <- ylim_delay[2] / ylim_battery[2]  


# width = 1600
selection %>%
  ggplot(aes(x = step)) +
    geom_line(aes(y = delay), alpha = 0.6, size = 0.5) +
    geom_line(aes(y = battery * scalefactor), color="red") +
    # geom_smooth(aes(y=delay), method = "lm") +
    geom_smooth(aes(y=delay), method="loess") +
    labs(title=title,
         subtitle=paste0("Start: ", time_start, " - End: ", time_end),
         x = "time steps [minutes]") + 
    scale_x_continuous(breaks=xbks, limits=xlim) +
    scale_y_continuous(name="delay [seconds]", breaks=ybks_delay, limits=ylim_delay, 
                       sec.axis=sec_axis(~./scalefactor, breaks=ybks_battery, name="battery [%]")) +
    theme_bw() +
    theme(
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.text.y.right = element_text(color = "red"), 
      axis.title.y.right = element_text(color = "red")
      # axis.title.y.left=element_text(color="blue"),
      # axis.text.y.left=element_text(color="blue"),
    )

plot_path <- here::here("figs", "advanced-1A.png")
ggsave(filename = plot_path, width = 18, height = 16, units = "cm")

# 
# manipulate(
#   filter(data_formatted, task == task_selected) %>%
#   ggplot(aes(x = step, y=delay)) +
#     geom_line(alpha = 0.6, size = 0.5) +
#     labs(x = "time steps", y = "delay [seconds]") + 
#     scale_x_continuous(breaks=xbks, limits=xlim) +
#     scale_y_continuous(breaks=ybks, limits=ylim) +
#     theme_bw(),
#   outlier_flag = picker("yes", "no", label="Outlier") 
#   )
#   

