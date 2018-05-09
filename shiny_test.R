library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(plotly)
library(shinyTree)

source("charting_functions.R")
source("data_cleaning_functions.R")
source("data_retrieval_functions.R")

#Temporary data import
#df <- s3tools::read_using(FUN=readr::read_csv, s3_path="alpha-fact/OccupEye/occupeye_automation/sensor_df_20180412_full.csv")

time_list <- unique(strftime(df$obs_datetime,format="%H:%M"))
date_list <- unique(date(df$obs_datetime))
surveys_list <- get_surveys_list()
device_types <- unique(df$devicetype)
category_lists = get_cat_list(get_sensors_list(330))
cat1 <- unique(category_lists$category_1)
cat2 <- unique(category_lists$category_2)
cat3 <- unique(category_lists$category_3)



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(

       
      selectInput(inputId = "survey_name",
                 label = "Select OccupEye survey",
                 choices = surveys_list$name),
      
      
      selectInput(inputId = "start_time",
                 label = "Start time:",
                 choices = time_list,
                 selected = "09:00"),
      
      selectInput(inputId = "end_time",
                 label = "End time:",
                 choices = time_list,
                 selected = "17:00"),
      

      pickerInput(inputId = "category_1",
                  label = "Select Group",
                  choices = cat1,
                  options = list(`actions-box` = TRUE,`selected-text-format` = "count > 3"),
                  multiple = TRUE,
                  selected = cat1),
      
      pickerInput(inputId = "category_2",
                  label = "Select Department(s)",
                  choices = cat2,
                  options = list(`actions-box` = TRUE,`selected-text-format` = "count > 3"),
                  multiple = TRUE,
                  selected = cat2),
      
      pickerInput(inputId = "category_3",
                  label = "Select team(s)",
                  choices = cat3,
                  options = list(`actions-box` = TRUE,`selected-text-format` = "count > 3"),
                  multiple = TRUE,
                  selected = cat3),
      
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
      
      
      
      numericInput(inputId = "smoothing_factor",
                   label = "Smoothing Factor",
                   min = 0,
                   max = 1,
                   value=0.5,
                   step=0.1),
      actionButton("download_button","Download the data"),
      actionButton("goButton","Go!")
    
      ),
    
      mainPanel(
        tabsetPanel(
          tabPanel("Tree",shinyTree("tree",checkbox = TRUE),
                   verbatimTextOutput("selTxt")),
          tabPanel("df_sum",tableOutput(outputId = "df_sum")),
          tabPanel("Recommendation Table",tableOutput(outputId = "recom_table")),
          tabPanel("Smoothing",plotlyOutput(outputId = "smoothChart")),
          tabPanel("daily usage",plotlyOutput(outputId = "dailyChart")),
          tabPanel("usage by weekday",plotlyOutput(outputId = "weekdayChart")),
          tabPanel("usage by desk type",plotlyOutput(outputId = "deskChart"))
      )
      
    )
  )
)

server <- function(input,output) {
  
  # df_sum <- reactive({
  #   input$download_button
  #   isolate(get_sensor_df(get_survey_id(surveys_list,input$survey_name),
  #                         input$date_range[1],
  #                         input$date_range[2],
  #                         input$category_1,
  #                         input$category_2,
  #                         input$category_3) %>%
  #     get_df_sum(input$start_time,input$end_time))
  # })
  
  
  filtered <- reactive({
    filtered <- df_sum %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2],
             devicetype %in% input$desk_type,
             category_1 %in% input$category_1,
             category_2 %in% input$category_2,
             category_3 %in% input$category_3)
  })
  
  output$df_sum <- renderTable({
    df_sum
  })

  output$recom_table <- renderTable({
    input$goButton
    isolate(allocation_strategy_table(filtered()))
  })
  

  output$dailyChart <- renderPlotly({
    input$goButton
    isolate(prop_daily_usage_chart(filtered()))
  })
  
  output$weekdayChart <- renderPlotly({
    input$goButton
    isolate(prop_weekday_usage_chart(filtered()))
  })
  
  output$deskChart <- renderPlotly({
    input$goButton
    isolate(prop_desk_usage_chart(filtered()))
  })
  
  output$smoothChart <- renderPlotly({
    input$goButton
    isolate(smoothing_chart(filtered(),input$smoothing_factor))
  })
  
  output$tree <- renderTree({
    cat1split <- with(category_lists,split(category_lists,category_1))
    
    cat2split <- lapply(cat1split, function(x) split(x, x$category_2))
    
    cat3split <- lapply(cat2split,lapply,function(x) split(x,x$category_3))
    
    final <- lapply(cat3split,lapply,lapply,function(x) x = names(x))
    
  })
  
  output$selTxt <- renderText({
    tree <- input$tree
    if (is.null(tree)){
      "None"
    } else{
      unlist(get_selected(tree))
    }
  })

  
}  

shinyApp(ui=ui,server=server)

