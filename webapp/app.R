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
library(scales)

source("load_data.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Symptoms - scheduler experiment"),
    
    
    helpText("Create a point plot per type of device."),
    
    sidebarLayout(
        sidebarPanel(
        
            selectInput(inputId = "selected_device", 
                        label = "Scheduler - Devices", 
                        choices = devices_lbl),
            
            
            checkboxInput("selected_outlier", 
                           label = "Show outliers?", 
                           FALSE),
            
            hr(),
            
            dateRangeInput(inputId =  "selected_dates",
                           label = "Date interval",
                           min    = min_date,  # minimum/maximun allowed date
                           max    = max_date,
                           start  = start_date,# initial start/end date
                           end    = end_date,
                           format = "dd/MM",
                           separator = " to "),
            
            
            sliderInput(inputId = "selected_timestart", 
                        label = "From hour (of start date)", 
                        min = 0, 
                        max = 24,
                        step = 1,
                        value = 12),
            
            sliderInput(inputId = "selected_timeend", 
                        label = "Until hour (of end date)", 
                        min = 0, 
                        max = 24,
                        step= 1,
                        value = 12),
            
            hr(),
            
            radioButtons(inputId = "selected_grouping", 
                         label = "Grouping",
                         choices = list("No grouping (+ smoothing line)" = "1", 
                                        "By daytime/nightime" = "2", 
                                        "By calendar day (+ smoothing line)" = "3"), 
                         selected = 1),
            
            selectInput(inputId = "selected_model", 
                        label = "Smoothing", 
                        choices = c("gam", "loess")),

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
        "Summary delay ranges (Y-axis). Outlier = value out of the range [lower, upper]."
    })
    
    output$id_table <- renderTable({means},
                                   striped = TRUE,
                                   spacing = "xs")
    
    # Filter data series to current selection
    selection <- reactive({

        start <- lubridate::as_date(input$selected_dates[1])
        selected_startdate <- make_datetime(year(start), month(start), day(start), 
                                            hour = input$selected_timestart, tz="Europe/Madrid")
        end <- lubridate::as_date(input$selected_dates[2])
        selected_enddate <- make_datetime(year(end), month(end), day(end), 
                                          hour = input$selected_timeend, tz="Europe/Madrid")
        selected_interval <- interval(selected_startdate, selected_enddate)

        if (input$selected_outlier) {
            data %>%
                filter(device_id == input$selected_device,
                       plan_date %within% selected_interval)
                       # plan_date >= selected_startdate,
                       # plan_date <= selected_enddate)

        } else {
            data %>%
                filter(device_id == input$selected_device,
                       outlier == "no",
                       plan_date %within% selected_interval)
                       # plan_date >= selected_startdate,
                       # plan_date <= selected_enddate)
                       
        }
    })
        
    output$id_plot <- renderPlot({
        time_start <- min(selection()$plan_date)
        time_end <- max(selection()$plan_date)
        time_elapsed <- interval(time_start, time_end)

        title <- paste0(selection()$device_desc)
                      
        subtitle <- paste("[Selected interval]",
                           "From", time_start,
                           "to", time_end)
        
        ylim_delay <- c(min(selection()$delay), max(selection()$delay))
        ystep <- ceiling((ylim_delay[2] - ylim_delay[1]) / 10) 
        ybks_delay <- round(seq(ylim_delay[1], ylim_delay[2], (ylim_delay[2] - ylim_delay[1])/ 10),3)
        
        ylim_battery <- c(min(selection()$battery), max(selection()$battery))
        ybks_battery <- seq(0, 100, 10)
        
        legend.nrow <- length(unique(selection()$plan_day))
        
        # Dual-scale plot
        scalefactor <- ylim_delay[2] / ylim_battery[2]  
        
        palette <- c("daytime"="#66CC99", "nighttime"="#9999CC") #  #CC6666
    
        p <- switch (input$selected_grouping,
            "1" = ggplot(selection(), aes(x = plan_date)),
            "2" = ggplot(selection(), aes(x = plan_date, color=factor(time_period))),
            "3" = ggplot(selection(), aes(x = plan_date, color=factor(plan_day)))
        )
        
        p <- p + 
            geom_point(aes(y = delay), alpha = 0.6, size = 0.7) +
            geom_line(aes(y = battery * scalefactor), color="red", size=0.7) +
            scale_x_datetime(breaks = scales::date_breaks("8 hours"),
                             labels = scales::date_format("%d-%b\n%H:%M", tz="CET"),
                             expand = c(0,0))
        
        if (input$selected_grouping=="1" |
            input$selected_grouping=="3") {
            p <- p +
                geom_smooth(aes(y=delay), method=input$selected_model)
        }
         
        p <- p +
            labs(title=title,
                 subtitle=subtitle,
                 x = "Date [3 breaks per day]") +
            scale_y_continuous(name="delay [seconds]", breaks=ybks_delay, limits=ylim_delay,
                               sec.axis=sec_axis(~./scalefactor, breaks=ybks_battery, name="battery [%]"))
             
        if (input$selected_grouping=="2") {
            p <- p +
                scale_color_manual(values = palette,
                                   breaks = names(palette),
                                   labels = c("day \n(8-23 hr)", "night \n(0-7 hr)")) +
                guides(color=guide_legend(title="Period of day", nrow=2))
        }

        if (input$selected_grouping=="3") {
            p <- p +
                guides(color=guide_legend(title="Day",
                                          # override.aes=list(fill=NA),
                                          nrow=legend.nrow))
        }

        p <- p +
            theme(legend.title = element_text(size=8)) +
            theme_bw() +
            theme(
                axis.line.y.right = element_line(color = "red"),
                axis.ticks.y.right = element_line(color = "red"),
                axis.text.y.right = element_text(color = "red"),
                axis.title.y.right = element_text(color = "red")

            )
        p
            
    }, height = 650)
    

 
}

# Run the application 
shinyApp(ui = ui, server = server)

