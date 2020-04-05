#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(tidyverse)
library(lubridate)

source("load_data.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Symptoms - scheduler experiment"),
    
    
    helpText("Create a line plot per device."),
    
    sidebarLayout(
        sidebarPanel(
        
            selectInput(inputId = "selected_device", 
                        label = "Scheduler - Devices:", 
                        choices = devices_lbl),
            
            
            checkboxInput("selected_outlier", 
                           label = "Show outliers", 
                           FALSE),
            
            dateRangeInput(inputId =  "selected_dates",
                           label = "Time interval:",
                           min    = min_date,
                           max    = max_date,
                           start  = start_date,
                           end    = max_date,
                           format = "dd/MM",
                           separator = " to "),

            width = 3
            
        ),
        
        mainPanel(
            textOutput("id_label_summary"),
            tableOutput("id_table"), 
            plotOutput("id_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$id_label_summary <- renderText({ 
        "Summary of delay ranges (Y-axis). Outlier = value out of the range [lower, upper]."
    })
    
    output$id_table <- renderTable({means},
                                   striped = TRUE)
    
    # Filter data series to current selection
    selection <- reactive({
        selected_interval <- interval(input$selected_dates[1], input$selected_dates[2])
        
        if (input$selected_outlier) {
            data %>%
                filter(device_id == input$selected_device,
                       plan_date %within% selected_interval)
            
        } else {
            data %>%
                filter(device_id == input$selected_device,
                       outlier == "no",
                       plan_date %within% selected_interval)
            
        }
    })
        
    output$id_plot <- renderPlot({
        time_start <- min(selection()$plan_date)
        time_end <- max(selection()$plan_date)
        time_elapsed <- interval(time_start, time_end)
        duration <- ceiling(as.duration(time_elapsed) / dhours(1))
        
        step_start <- min(selection()$step)
        step_end <- max(selection()$step)
        
        title <- paste0(selection()$device_desc)
                      
        subtitle <- paste("[Selected time interval]",
                           "Start:", time_start, 
                           "(step", step_start, ")", 
                           " - ",
                           "End:", time_end,
                           "(step", step_end, ")")
        
        ylim_delay <- c(min(selection()$delay), max(selection()$delay))
        ystep <- ceiling((ylim_delay[2] - ylim_delay[1]) / 10) 
        ybks_delay <- round(seq(ylim_delay[1], ylim_delay[2], (ylim_delay[2] - ylim_delay[1])/ 10),3)
        
        xlim <- c(min(selection()$step), max(selection()$step))
        xstep <- ceiling((xlim[2] - xlim[1])/duration)
        xstep <- 360 # number of steps per day / 4
        xbks <- seq(xlim[1], xlim[2], xstep)
        
        ylim_battery <- c(min(selection()$battery), max(selection()$battery))
        ybks_battery <- seq(0, 100, 10)
        
        # Dual-scale plot
        scalefactor <- ylim_delay[2] / ylim_battery[2]  
        
        palette <- c("daytime"="#66CC99", "nighttime"="#9999CC") #  #CC6666
    
        ggplot(selection(), aes(x = step, color=factor(time_period))) +
            geom_line(aes(y = delay), alpha = 0.6, size = 0.5) +
            geom_line(aes(y = battery * scalefactor), color="red") +
            # geom_smooth(aes(y=delay), method = "lm") +
            # geom_smooth(aes(y=delay), method="loess") +
            labs(title=title,
                 subtitle=subtitle,
                 x = "time steps [minutes since start of experiment] - 4 breaks per day") + 
            scale_x_continuous(breaks=xbks, limits=xlim) +
            scale_y_continuous(name="delay [seconds]", breaks=ybks_delay, limits=ylim_delay, 
                               sec.axis=sec_axis(~./scalefactor, breaks=ybks_battery, name="battery [%]")) +
            scale_color_manual(values = palette,
                               breaks = names(palette),
                               labels = c("day \n(8-23 hr)", "night \n(0-7 hr)")) +
            guides(color=guide_legend(title="", override.aes=list(fill=NA), nrow=2)) + # modify legend title
            theme(legend.title = element_text(size=8), 
                  legend.position="bottom",
                  legend.direction = "horizontal") +
            theme_bw() +
            theme(
                axis.line.y.right = element_line(color = "red"), 
                axis.ticks.y.right = element_line(color = "red"),
                axis.text.y.right = element_text(color = "red"), 
                axis.title.y.right = element_text(color = "red")
            )
    }, height = 650)
    

 
}

# Run the application 
shinyApp(ui = ui, server = server)

