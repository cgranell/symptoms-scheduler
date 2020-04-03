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
library(lubridate)
library(tidyverse)

file_path <- here::here("data", "data.csv")

data <- read_csv(file_path, col_names = TRUE)

devices_lbl <- c("Advanced - BQ Aquaris V" = "BQ",
                 "Advanced - Nvidia Shield Tablet" = "NV",
                 "Advanced - Xiaomi Mi A1" = "A1",
                 "Basic - Honor 9" = "H9",
                 "Basic - Motorola Moto G" = "MO")

min_date = as_date(min(data$plan_date))
max_date = as_date(max(data$plan_date))


# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Symptoms - scheduler experiment"),
    
    sidebarLayout(
        sidebarPanel(
        
            selectInput("selected_device", 
                        label = "Devices:", 
                        choices = devices_lbl),
            
            
            checkboxInput("selected_outlier", 
                           label = "Show outliers", 
                           FALSE),
            
            dateRangeInput("selected_dates",
                           "Date range:",
                           start  = min_date,
                           end    = max_date,
                           min    = min_date,
                           max    = max_date,
                           format = "dd/mm/yy",
                           separator = " - "),
            
            # sliderInput("selected_dates",
            #             label = h5("Select mapping date"),
            #             min = min_date,
            #             max = max_date,
            #             value = as.Date(current_date),
            #             timeFormat = "%d %b")
                        # animate=animationOptions(interval = 2000, loop = FALSE)),
            
            
            width = 3
            
            
            # sliderInput("selected_dates",
            #             "Dates:",
            #             min = min_date,
            #             max = max_date,
            #             # step = 1,  
            #             # sep = "",
            #             value = min_date,
            #             timeFormat="%Y-%m-%d")
        ),
        
        mainPanel(
           plotOutput("id_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
        
        title <- paste0(unique(selection()$scheduler),
                        " scheduler - ",
                        unique(selection()$device_name))
                      
        subtitle <- paste0("[Selected interval] ",
                           "Start: ", time_start, 
                           " - ",
                           "End: ", time_end)
        
        ylim_delay <- c(min(selection()$delay), max(selection()$delay))
        ystep <- ceiling((ylim_delay[2] - ylim_delay[1]) / 10) 
        ybks_delay <- round(seq(ylim_delay[1], ylim_delay[2], (ylim_delay[2] - ylim_delay[1])/ 10),3)
        
        xlim <- c(min(selection()$step), max(selection()$step))
        xstep <- ceiling((xlim[2] - xlim[1])/duration)
        xbks <- seq(xlim[1], xlim[2], xstep)
        
        ylim_battery <- c(min(selection()$battery), max(selection()$battery))
        ybks_battery <- seq(0, 100, 10)
        
        # Dual-scale plot
        scalefactor <- ylim_delay[2] / ylim_battery[2]  
    
        ggplot(selection(), aes(x = step)) +
            geom_line(aes(y = delay), alpha = 0.6, size = 0.5) +
            geom_line(aes(y = battery * scalefactor), color="red") +
            # geom_smooth(aes(y=delay), method = "lm") +
            geom_smooth(aes(y=delay), method="loess") +
            labs(title=title,
                 subtitle=subtitle,
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
            )
    })
 
}

# Run the application 
shinyApp(ui = ui, server = server)

