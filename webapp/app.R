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

file_path <- here::here("data", "Dummy1minA1_30-03-2020T12h23.csv")

data_advance <- read_csv(file_path, col_names = TRUE)

# devices <- unique(data_advance$device) 
# schedulers <- unique(data_advance$scheduler)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Symptoms - scheduler experiment"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("id_scheduler", 
                        label = "Scheduler:", 
                        choices = c(#"Basic/naive scheduler" = "basic",
                                    "Advanced/enhanced scheduler" = "advanced")),
            
        
            selectInput("id_device", 
                        label = "Devices:", 
                        choices = c("A1" = "A1")),
            
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
            data_advance %>%
                filter(device == input$id_device)
            
        } else {
            data_advance %>%
                filter(device == input$id_device,
                       outlier == "no")
        }
    })
        
# 

#     
    output$id_plot <- renderPlot({
        time_start <- min(selection()$plan_date)
        time_end <- max(selection()$plan_date)
        
        ylim_delay <- c(min(selection()$delay), max(selection()$delay))
        ybks_delay <- seq(ylim_delay[1], ylim_delay[2], 5)
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
            labs(title="Advanced - 1A (Dummy1minA1_30-03-2020T12h23.csv)",
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

