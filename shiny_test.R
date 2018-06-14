
# Library declarations ----------------------------------------------------

library(shiny)          # For the shiny!
library(ggplot2)        # For plotting
library(dplyr)          # For pipes
library(htmlwidgets)    # rpivottool depends on it
library(shinyWidgets)   # For pickerInput
library(plotly)         # Makes ggplot interactive
library(shinyTree)      # for the category tree
library(rpivotTable)    # Pivot tables


# import other source code ------------------------------------------------


source("charting_functions.R")
source("data_cleaning_functions.R")
source("data_retrieval_functions.R")




# Temporary initialise functions ------------------------------------------
# Downloads a sample dataset from S3, and uses it to initialise the UI fields.
# These will need to be removed/revised once the Athena connection is fixed.

df <- s3tools::read_using(FUN=readr::read_csv, s3_path="alpha-fact/OccupEye/occupeye_automation/sensor_df_20180412_full.csv")
df_sum <- get_df_sum(df,"09:00","17:00")
time_list <- unique(strftime(df$obs_datetime,format="%H:%M"))
date_list <- unique(date(df$obs_datetime))
surveys_list <- s3tools::read_using(FUN=readr::read_csv,s3_path = "alpha-fact/OccupEye/occupeye_automation/surveys.csv")
device_types <- unique(df$devicetype)
floors <- unique(df$floor)
report_list <- s3tools::list_files_in_buckets("alpha-fact") %>% filter(grepl("336",path))


# UI function -------------------------------------------------------------
# Constructs the UI

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Choose report to download",
          selectInput(inputId = "raw_csv",
                      label = "Select report to download",
                      choices = report_list$filename),
          
          selectInput(inputId = "start_time",
                      label = "Start time:",
                      choices = time_list,
                      selected = "09:00"),
          
          selectInput(inputId = "end_time",
                      label = "End time:",
                      choices = time_list,
                      selected = "17:00"),
          
          actionButton("loadCSV","Load report")
          
        ),
        
        
        
        
        
        tabPanel("Report config",
                 
          helpText("Hit the go button below to update the filter"),
      
          
          actionButton("goButton","Go!"),
    
           
          selectInput(inputId = "survey_name",
                     label = "Select OccupEye survey",
                     choices = surveys_list$name,
                     selected = "102 Petty France v1.1"),
          
          
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
          
          pickerInput(inputId = "floors",
                      label = "Pick floor(s)",
                      choices = floors,
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE,
                      selected = floors),
          
          numericInput(inputId = "smoothing_factor",
                       label = "Smoothing Factor",
                       min = 0,
                       max = 1,
                       value=0.5,
                       step=0.1),
          
          helpText("Select Department(s) and team(s)"),
          shinyTree("tree",checkbox = TRUE,search=TRUE)
          ),
          
          tabPanel("Download Report",
            radioButtons('format', 'Document format', c('HTML', 'Word'),
                         inline = TRUE),
            downloadButton("download_button","Generate report")
          )
        
        )
      ),
    
      mainPanel(
        tabsetPanel(
          tabPanel("Pivot table",rpivotTableOutput("myPivot")),
          tabPanel("Summary tables",tableOutput(outputId = "recom_table"),
                   column(4,tableOutput(outputId = "team_count")),
                   column(4,tableOutput(outputId = "desk_count")),
                   column(4,tableOutput(outputId = "team_desk_count"))),
          tabPanel("Smoothing",plotlyOutput(outputId = "smoothChart")),
          tabPanel("daily usage",plotlyOutput(outputId = "dailyChart")),
          tabPanel("usage by weekday",plotlyOutput(outputId = "weekdayChart")),
          tabPanel("usage by desk type",plotlyOutput(outputId = "deskChart")),
          tabPanel("usage by floor",plotlyOutput(outputId = "floorChart")),
          tabPanel("raw data",dataTableOutput(outputId = "df_sum"))
      )
      
    )
  )
)


# Server function ---------------------------------------------------------
# This function defines the server function. Should be separated into a separate file
server <- function(input,output,session) {
  

# Raw data download -------------------------------------------------------

  
  # This section (or something like it) is here as a placeholder while waiting for the Athena fix.
  # Once that occurs, this will need to be uncommented and the filtered() function below
  # will need to hook to this function rather than a local version of df_sum
  
  df_sum <- reactive({
    input$loadCSV
    withProgress(message = paste0("Loading report ",input$raw_csv), {
      isolate({
        csv_Path <- report_list %>% filter(filename == input$raw_csv)
        df <- s3tools::read_using(FUN=readr::read_csv, s3_path=csv_Path$path)
        get_df_sum(df,input$start_time,input$end_time)
  
      })
    })

  })
  
  
  observeEvent(input$loadCSV, {
    updatePickerInput(session, inputId = "floors",
                      choices = unique(df_sum()$floor),
                      selected = unique(df_sum()$floor))
    updatePickerInput(session, inputId = "desk_type",
                      choices = unique(df_sum()$devicetype),
                      selected =unique(df_sum()$devicetype))
    updateDateRangeInput(session, inputId = "date_range",
                         min = min(unique(df_sum()$date)),
                         max = max(unique(df_sum()$date)))
  })


# Data filter -------------------------------------------------------------
  
  
  # Filter the data based on the input filters. This forms the input for the plots and tables
  filtered <- reactive({
    
    # only update this when you hit go
    input$goButton
    
    # Isolate the actual calculation bit so it only updates when you want it to (i.e. when you hit go)
    isolate({
    
      #loop through the layers of the team selection tree to get the selected names at each level
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
       filtered <- df_sum() %>%
        filter(date >= input$date_range[1] & date <= input$date_range[2],
               devicetype %in% input$desk_type,
               floor %in% input$floors,
               category_1 %in% l1Names,
               category_2 %in% l2Names,
               category_3 %in% l3Names)
    
    })
  })
  
  

# Team selection tree -----------------------------------------------------
# Creates the UI for selecting the teams
  
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

# Plots and table outputs -------------------------------------------------

# These functions generate the charts and tables in the report
  observeEvent(input$goButton, {
  output$myPivot <- renderRpivotTable({
    rpivotTable(data = filtered())
  })

  output$recom_table <- renderTable({
    isolate(allocation_strategy_table(filtered()))
  })
  
  output$desk_count <- renderTable({
    isolate(desks_by_desk_type(filtered()))
  })
  
  output$team_count <- renderTable({
    isolate(desks_by_team(filtered()))
  })
  
  output$team_desk_count <- renderTable({
    isolate(desks_by_desk_type_and_team(filtered()))
  })
  
  
  output$smoothChart <- renderPlotly({
    isolate(smoothing_chart(filtered(),input$smoothing_factor))
  })

  output$dailyChart <- renderPlotly({
    isolate(prop_daily_usage_chart(filtered()))
  })
  
  output$weekdayChart <- renderPlotly({
    isolate(prop_weekday_usage_chart(filtered()))
  })
  
  output$deskChart <- renderPlotly({
    isolate(prop_desk_usage_chart(filtered()))
  })
  
  output$floorChart <- renderPlotly({
    isolate(prop_floor_usage_chart(filtered()))
  })
  
  
  output$df_sum <- renderDataTable({
    filtered()
  })

})
# Download handler --------------------------------------------------------

# Functions for handling the report download  
  output$download_button <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      
      out_report <- switch(
        input$format, PDF = 'pdf', HTML = 'slidy_report.Rmd', Word = 'word_report.Rmd'
      )
      
      src <- normalizePath(out_report)
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, out_report, overwrite = TRUE)
      
      out <- rmarkdown::render(out_report)
      file.rename(out, file)
    }
    
    
  )
  
  # change to TRUE when deployed
  
  # refreshes connection when grey screened
  
  session$allowReconnect("force")

  
}  

shinyApp(ui=ui,server=server)

