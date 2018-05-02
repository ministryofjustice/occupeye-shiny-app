library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(plotly)


source("charting_functions.R")
source("data_cleaning_functions.R")
source("data_retrieval_functions.R")

#Temporary data import
#df <- s3tools::read_using(FUN=readr::read_csv, s3_path="alpha-fact/OccupEye/occupeye_automation/sensor_df_20180412_full.csv")

time_list <- unique(strftime(df$obs_datetime,format="%H:%M"))
date_list <- unique(date(df$obs_datetime))
device_types <- unique(df$devicetype)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = "date_range",
                     label = "Select sample date range",
                     start = min(date_list),
                     end = max(date_list),
                     min = min(date_list),
                     max = max(date_list)),
      
      pickerInput(inputId = "desk_type",
                  label = "Pick desk type(s)",
                  choices = device_types,
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  selected = device_types),
      
      selectInput(inputId = "start_time",
                  label = "Start time:",
                  choices = time_list,
                  selected = "09:00"),
    
      selectInput(inputId = "end_time",
                  label = "End time:",
                  choices = time_list,
                  selected = "17:00"),
      
      numericInput(inputId = "smoothing_factor",
                   label = "Smoothing Factor",
                   min = 0,
                   max = 1,
                   value=0.5,
                   step=0.1)
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("Recommendation Table",tableOutput(outputId = "recom_table")),
        tabPanel("Smoothing",plotlyOutput(outputId = "smoothChart")),
        tabPanel("daily usage",plotOutput(outputId = "dailyChart")), #for some reason this chart doesn't work with plotly so just using regular plotting
        tabPanel("usage by weekday",plotlyOutput(outputId = "weekdayChart")),
        tabPanel("usage by desk type",plotlyOutput(outputId = "deskChart"))
      )
      
    )
  )
)

server <- function(input,output) {
  
  filtered <- reactive({
    filtered <- get_df_sum(df,input$start_time,input$end_time) %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2],
             devicetype %in% input$desk_type)
  })

  output$recom_table <- renderTable({
    allocation_strategy_table(filtered())
  })
  
  #for some reason the daily chart doesn't work with plotly so just using regular plotting
  output$dailyChart <- renderPlot({
    prop_daily_usage_chart(filtered())
  })
  
  output$weekdayChart <- renderPlotly({
    prop_weekday_usage_chart(filtered())
  })
  
  output$deskChart <- renderPlotly({
    prop_desk_usage_chart(filtered())
  })
  
  output$smoothChart <- renderPlotly({
    smoothing_chart(filtered(),input$smoothing_factor)
  })
  

  
}  

shinyApp(ui=ui,server=server)


