

dual_scale_plot <- function(selection, plot_title) {
  
  time_start <- min(selection$plan_date)
  time_end <- max(selection$plan_date)
  time_elapsed <- interval(time_start, time_end)
  duration <- ceiling(as.duration(time_elapsed) / ddays(1))
  
  ylim_delay <- c(min(selection$delay), max(selection$delay))
  ystep <- round((ylim_delay[2] - ylim_delay[1]) / 10, 3) 
  ybks_delay <- round(seq(ylim_delay[1], ylim_delay[2], ystep), 3)
  
  ylim_battery <- c(min(selection$battery), max(selection$battery))
  ybks_battery <- seq(0, 100, 10)
  
  # Dual-scale plot: https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
  scalefactor <- ylim_delay[2] / ylim_battery[2]  
  
  selection %>%
    ggplot(aes(x = plan_date, color=factor(scheduler))) +
    geom_point(aes(y = delay), alpha = 0.6, size = 0.5) +
    geom_line(aes(y = battery * scalefactor), color="red") +
    # geom_smooth(aes(y=delay), method="loess") +
    labs(title=plot_title,
         subtitle=paste0("Start: ", time_start, " - End: ", time_end),
         x = "Date [2 breaks per day]") + 
    scale_x_datetime(breaks = scales::date_breaks("12 hours"),
                     labels = scales::date_format("%d-%m %H:%M", tz="CET"),
                     expand = c(0,0)) +
    scale_y_continuous(name="delay [seconds]", breaks=ybks_delay, limits=ylim_delay, 
                       sec.axis=sec_axis(~./scalefactor, breaks=ybks_battery, name="battery [%]")) +
    
    guides(color=guide_legend(title="Scheduler",
                              # override.aes=list(fill=NA),
                              nrow=2)) + 
    
    theme(legend.title = element_text(size=8), 
          legend.position="bottom",
          legend.direction = "horizontal") +
    theme_bw() +
    theme(
      axis.line.y.right = element_line(color = "red"), 
      axis.ticks.y.right = element_line(color = "red"),
      axis.text.y.right = element_text(color = "red"), 
      axis.title.y.right = element_text(color = "red"),
      axis.text.x = element_text(size=7, angle=45)
      # axis.title.y.left=element_text(color="blue"),
      # axis.text.y.left=element_text(color="blue"),
    ) -> p
  
    return(p)
}