library(here)
library(tidyverse)
library(lubridate)


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
duration <- ceiling(as.duration(time_elapsed) / ddays(1)) # dhours(1), ddays(1)

# # get rid of outlier
# n_records <- max(data$step)
# n_outliers <- nrow(filter(data, outlier == "yes"))
# 
# cat(paste0("outliers: ", round(n_outliers / n_records, 2), "%"))

ylim_delay <- c(min(selection$delay), max(selection$delay))
ystep <- ceiling((ylim_delay[2] - ylim_delay[1]) / 10) 
ybks_delay <- round(seq(ylim_delay[1], ylim_delay[2], ystep), 3)

xlim <- c(min(selection$step), max(selection$step))
xstep <- ceiling((xlim[2] - xlim[1])/duration)
xstep <- 360
xbks <- seq(xlim[1], xlim[2], xstep)

ylim_battery <- c(min(selection$battery), max(selection$battery))
ybks_battery <- seq(0, 100, 10)

# Dual-scale plot based on time step
# https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
scalefactor <- ylim_delay[2] / ylim_battery[2]  


palette <- c("daytime"="#66CC99", "nighttime"="#9999CC") #  #CC6666


## line chart, grouped by time_peridd
# width = 1600
selection %>%
  ggplot(aes(x = step, color=factor(time_period))) +
    geom_point(aes(y = delay), alpha = 0.6, size = 0.5) +
    geom_line(aes(y = battery * scalefactor), color="red") +
    # geom_smooth(aes(y=delay), method = "lm") +
    geom_smooth(aes(y=delay), method="loess") +
    labs(title=title_plot,
         subtitle=paste0("Start: ", time_start, " - End: ", time_end),
         x = "time steps [minutes]") + 
    scale_x_continuous(breaks=xbks, limits=xlim) +
    scale_y_continuous(name="delay [seconds]", breaks=ybks_delay, limits=ylim_delay, 
                       sec.axis=sec_axis(~./scalefactor, breaks=ybks_battery, name="battery [%]")) +
    scale_color_manual(values = palette,
                       breaks = names(palette),
                       labels = c("day time \n(8-23 hr)", "night time \n(0-7 hr)")) +
    guides(color=guide_legend(title="Time period", override.aes=list(fill=NA), nrow=2)) + # modify legend title
    theme(legend.title = element_text(size=8), 
        legend.position="bottom",
        legend.direction = "horizontal") +
    theme_bw() +
    theme(
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.text.y.right = element_text(color = "red"), 
      axis.title.y.right = element_text(color = "red")
      # axis.title.y.left=element_text(color="blue"),
      # axis.text.y.left=element_text(color="blue"),
    )




plot_path <- here::here("figs", "timestep.png")
ggsave(filename = plot_path, width = 20, height = 16, units = "cm")

legend.nrow <- length(unique(selection$plan_day))

## line chart, grouped by plan_day
# width = 1600
selection %>%
  ggplot(aes(x = step, color=factor(plan_day))) +
  geom_point(aes(y = delay), alpha = 0.6, size = 0.5) +
  geom_line(aes(y = battery * scalefactor), color="red") +
  # geom_smooth(aes(y=delay), method = "lm") +
  geom_smooth(aes(y=delay), method="loess") +
  labs(title=title_plot,
       subtitle=paste0("Start: ", time_start, " - End: ", time_end),
       x = "time steps [minutes]") + 
  scale_x_continuous(breaks=xbks, limits=xlim) +
  scale_y_continuous(name="delay [seconds]", breaks=ybks_delay, limits=ylim_delay, 
                     sec.axis=sec_axis(~./scalefactor, breaks=ybks_battery, name="battery [%]")) +
  # scale_color_discrete()values = palette,
  #                    breaks = names(palette),
  #                    labels = c("day time \n(8-23 hr)", "night time \n(0-7 hr)")) +
  guides(color=guide_legend(title="Day", override.aes=list(fill=NA), nrow=7)) + # modify legend title
  theme(legend.title = element_text(size=8), 
        legend.position="bottom",
        legend.direction = "horizontal") +
  theme_bw() +
  theme(
    axis.line.y.right = element_line(color = "red"), 
    axis.ticks.y.right = element_line(color = "red"),
    axis.text.y.right = element_text(color = "red"), 
    axis.title.y.right = element_text(color = "red")
    # axis.title.y.left=element_text(color="blue"),
    # axis.text.y.left=element_text(color="blue"),
  )


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

