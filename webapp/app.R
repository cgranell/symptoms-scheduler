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

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Symptoms - scheduler experiment"),
    
    sidebarLayout(
        sidebarPanel(
        
            selectInput("id_device", 
                        label = "Devices:", 
                        choices = devices_lbl),
            
            
            checkboxInput("id_outlier", 
                           label = "Show outliers", 
                           FALSE)
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
        
        if (input$id_outlier) {
            data %>%
                filter(device_id == input$id_device)
            
        } else {
            data %>%
                filter(device_id == input$id_device,
                       outlier == "no")
        }
    })
        
    output$id_plot <- renderPlot({
        title <- paste0(unique(selection()$scheduler),
                       " - ",
                        unique(selection()$device_name))
        
        
        time_start <- min(selection()$plan_date)
        time_end <- max(selection()$plan_date)
        
        ylim_delay <- c(min(selection()$delay), max(selection()$delay))
        ybks_delay <- round(seq(ylim_delay[1], ylim_delay[2], (ylim_delay[2] - ylim_delay[1])/ 10),3)
        xlim <- c(min(selection()$step), max(selection()$step))
        xbks <- seq(xlim[1], xlim[2], 100)
        
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
            )
    })
 
}

# Run the application 
shinyApp(ui = ui, server = server)

