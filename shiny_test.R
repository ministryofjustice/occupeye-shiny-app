library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(plotly)
library(shinyTree)
library(rpivotTable)

source("charting_functions.R")
source("data_cleaning_functions.R")
source("data_retrieval_functions.R")

#Temporary data import
#df <- s3tools::read_using(FUN=readr::read_csv, s3_path="alpha-fact/OccupEye/occupeye_automation/sensor_df_20180412_full.csv")

time_list <- unique(strftime(df$obs_datetime,format="%H:%M"))
date_list <- unique(date(df$obs_datetime))
surveys_list <- get_surveys_list()
device_types <- unique(df$devicetype)



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      actionButton("download_button","Download the data"),
      actionButton("goButton","Go!"),

       
      selectInput(inputId = "survey_name",
                 label = "Select OccupEye survey",
                 choices = surveys_list$name,
                 selected = "102 Petty France"),
      
      
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
                   step=0.1),
      
      
      shinyTree("tree",checkbox = TRUE,search=TRUE)
      
    
      ),
    
      mainPanel(
        tabsetPanel(
          tabPanel("Pivot table",rpivotTableOutput("myPivot")),
          tabPanel("Recommendation Table",tableOutput(outputId = "recom_table")),
          tabPanel("Smoothing",plotlyOutput(outputId = "smoothChart")),
          tabPanel("daily usage",plotlyOutput(outputId = "dailyChart")),
          tabPanel("usage by weekday",plotlyOutput(outputId = "weekdayChart")),
          tabPanel("usage by desk type",plotlyOutput(outputId = "deskChart")),
          tabPanel("df_sum",tableOutput(outputId = "df_sum"))
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
    
    # only update this when you hit go
    input$goButton
    
    
    isolate({
    
      #loop through the layers of the tree to get the selected names at each level
      tree <- input$tree
      l1Names <- NULL
      l2Names <- NULL
      l3Names <- NULL
      if (is.null(tree)){
        "None"
      } else{
        selected <-get_selected(tree,"slice")
        
        
        for(x in selected) {
          l1Names <- c(l1Names,names(x))
          for(y in x) {
            l2Names <- c(l2Names,names(y))
            for (z in y) {
              l3Names <- c(l3Names,names(z))
            }
          }
        }
      }
      
      
      # apply the filters
      filtered <- df_sum %>%
        filter(date >= input$date_range[1] & date <= input$date_range[2],
               devicetype %in% input$desk_type,
               category_1 %in% l1Names,
               category_2 %in% l2Names,
               category_3 %in% l3Names)
    
    })
  })
  
  output$df_sum <- renderDataTable({
    filtered()
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
    
    category_list <- get_cat_list(get_sensors_list(get_survey_id(surveys_list,input$survey_name)))
    
    # split the data.frame of categories into a nested list of lists
    cat1split <- with(category_list,split(category_list,category_1))
    cat2split <- lapply(cat1split, function(x) split(x, x$category_2))
    cat3split <- lapply(cat2split,lapply,function(x) split(x,x$category_3))
    
    # replace the category-3 level with the names, 
    # so that the original data frame headings aren't added as an extra layer.
    final <- lapply(cat3split,lapply,lapply,function(x) x <- names(x))
    
    
  })

  output$myPivot <- renderRpivotTable({
    rpivotTable(data = filtered())
  })

  
}  

shinyApp(ui=ui,server=server)

