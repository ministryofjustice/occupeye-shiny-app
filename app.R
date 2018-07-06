
# Library declarations ----------------------------------------------------

library(shiny)          # For the shiny!
library(ggplot2)        # For plotting
library(dplyr)          # For pipes
library(htmlwidgets)    # rpivotTable depends on it
library(shinyWidgets)   # For pickerInput
library(plotly)         # Makes ggplot interactive
library(shinyTree)      # for the category tree
library(rpivotTable)    # Pivot tables


# import other source code ------------------------------------------------


source("charting_functions.R")
source("data_cleaning_functions.R")

# Temporarily excluding Athena queries
#source("data_retrieval_functions.R")




# Temporary initialise functions ------------------------------------------
# Downloads a sample dataset from S3, and uses it to initialise the UI fields.
# These will need to be removed/revised once the Athena connection is fixed.

temp_df <- s3tools::read_using(FUN=readr::read_csv, s3_path="alpha-fact/OccupEye/occupeye_automation/surveys/336/HMCTS - Finance.csv")
temp_df_sum <- get_df_sum(temp_df,"09:00","17:00")
time_list <- unique(strftime(temp_df$obs_datetime,format="%H:%M"))
date_list <- unique(lubridate::date(temp_df$obs_datetime))
device_types <- unique(temp_df$devicetype)
floors <- unique(temp_df$floor)


report_list <- s3tools::list_files_in_buckets("alpha-fact") %>% dplyr::filter(grepl("336",path)) %>% arrange(filename)
surveys_list <- s3tools::read_using(FUN=readr::read_csv,s3_path = "alpha-fact/OccupEye/occupeye_automation/surveys.csv")

# UI function -------------------------------------------------------------
# Constructs the UI

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Choose report to download",
                 
         selectInput(inputId = "survey_name",
                     label = "Select OccupEye survey",
                     choices = surveys_list$name,
                     selected = "102 Petty France v1.1"),
         
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
      
          
          actionButton("goButton","Update filter"),
    
          
          
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
          tabPanel("Introduction",includeMarkdown("intro.md")),
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
          tabPanel("summarised data",dataTableOutput(outputId = "df_sum")),
          tabPanel("raw data",dataTableOutput(outputId = "raw_data"))
      )
      
    )
  )
)


# Server function ---------------------------------------------------------
# This function defines the server function. Should be separated into a separate file
server <- function(input,output,session) {
  

# Raw data download -------------------------------------------------------

  RV <- reactiveValues(data = temp_df,df_sum = temp_df_sum, filtered = temp_df_sum)


  observeEvent(input$loadCSV, {
    
    
    withProgress(message = paste0("Loading report ",input$raw_csv), {
      csv_Path <- report_list %>% dplyr::filter(filename == input$raw_csv)
      RV$data <- s3tools::read_using(FUN=readr::read_csv, s3_path=csv_Path$path)
      
    })
    
    withProgress(message="summarising the dataset", {
      RV$df_sum <- get_df_sum(RV$data,input$start_time,input$end_time)
    })
    

    
    
    
    floor_list <- unique(RV$df_sum$floor)
    desk_type_list <- unique(RV$df_sum$devicetype)
    date_list <- unique(RV$df_sum$date)
    
    updatePickerInput(session, inputId = "floors",
                      choices = floor_list,
                      selected = floor_list)
    updatePickerInput(session, inputId = "desk_type",
                      choices = desk_type_list,
                      selected =desk_type_list)
    updateDateRangeInput(session, inputId = "date_range",
                         min = min(date_list,na.rm = TRUE),
                         max = max(date_list,na.rm = TRUE),
                         start = min(date_list,na.rm = TRUE),
                         end = max(date_list,na.rm = TRUE))
  })


# Data filter -------------------------------------------------------------
  
  
  # Filter the data based on the input filters. This forms the input for the plots and tables
  observeEvent(input$goButton, {
    
    
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
    
    l3Names <- replace(l3Names,l3Names == "N/A",NA) # Convert N/A from string to NA to make filtering work

    
    # apply the filters
     RV$filtered <- RV$df_sum %>%
      dplyr::filter(date >= input$date_range[1] & date <= input$date_range[2],
             devicetype %in% input$desk_type,
             floor %in% input$floors,
             category_1 %in% l1Names,
             category_2 %in% l2Names,
             category_3 %in% l3Names)
  })
  
  

# Team selection tree -----------------------------------------------------
# Creates the UI for selecting the teams
  
  output$tree <- renderTree({
    
    # Ideally get category list from Athena...
    #category_list <- get_cat_list(get_sensors_list(get_survey_id(surveys_list,input$survey_name)))
    
    # But while that isn't working, get it from the CSV instead
    
    category_list <- RV$df_sum %>%
      select(category_1,category_2,category_3) %>%
      unique %>%
      mutate_all(funs(ifelse(is.na(.) | .=="","N/A",.))) # replaces nulls/empty string with "N/A" so that ShinyTree works properly
    
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
    rpivotTable(data = RV$filtered)
  })

  output$recom_table <- renderTable({
    isolate(allocation_strategy_table(RV$filtered))
  })
  
  output$desk_count <- renderTable({
    isolate(desks_by_desk_type(RV$filtered))
  })
  
  output$team_count <- renderTable({
    isolate(desks_by_team(RV$filtered))
  })
  
  output$team_desk_count <- renderTable({
    isolate(desks_by_desk_type_and_team(RV$filtered))
  })
  
  
  output$smoothChart <- renderPlotly({
    isolate(smoothing_chart(RV$filtered,input$smoothing_factor))
  })

  output$dailyChart <- renderPlotly({
    isolate(prop_daily_usage_chart(RV$filtered))
  })
  
  output$weekdayChart <- renderPlotly({
    isolate(prop_weekday_usage_chart(RV$filtered))
  })
  
  output$deskChart <- renderPlotly({
    isolate(prop_desk_usage_chart(RV$filtered))
  })
  
  output$floorChart <- renderPlotly({
    isolate(prop_floor_usage_chart(RV$filtered))
  })
  
  
  output$df_sum <- renderDataTable({
    RV$df_sum
  })
  
  output$raw_data <- renderDataTable({
    RV$data
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
      
      withProgress(message = "Generating report...", {
        out <- rmarkdown::render(out_report,params = list(start_date = input$date_range[1],end_date=input$date_range[2]))
        file.rename(out, file)
      })
      
    }
    
    
  )
  
  # change to TRUE when deployed
  
  # refreshes connection when grey screened
  
  session$allowReconnect("force")

  
}  

shinyApp(ui=ui,server=server)

