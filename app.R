
# Library declarations ----------------------------------------------------

library(shiny)          # For the shiny!
library(ggplot2)        # For plotting
library(dplyr)          # For pipes
library(htmlwidgets)    # rpivotTable depends on it
library(shinyWidgets)   # For pickerInput
library(plotly)         # Makes ggplot interactive
library(shinyTree)      # for the category tree
library(rpivotTable)    # Pivot tables
library(feather)        # Feather data reading

# import other source code ------------------------------------------------


source("charting_functions.R")
source("data_cleaning_functions.R")

# Temporarily excluding Athena queries
#source("data_retrieval_functions.R")




# Temporary initialise functions ------------------------------------------
# Downloads a sample dataset from S3, and uses it to initialise the UI fields.
# These will need to be removed/revised once the Athena connection is fixed.

temp_df <- s3tools::read_using(FUN=readr::read_csv, s3_path="alpha-app-occupeye-automation/surveys/336/Unallocated.csv")
temp_df_sum <- get_df_sum(temp_df,"09:00","17:00")
time_list <- unique(strftime(temp_df$obs_datetime,format="%H:%M"))
date_list <- unique(lubridate::date(temp_df$obs_datetime))
device_types <- unique(temp_df$devicetype)
floors <- unique(temp_df$floor)

sensors <- s3tools::read_using(FUN=feather::read_feather,s3_path = "alpha-app-occupeye-automation/sensors.feather") %>%
            mutate_at(.funs = funs(ifelse(.=="",NA,.)), # Feather imports missing values 
            .vars = vars(category_1,category_2,category_3))



surveys_list <- s3tools::read_using(FUN=feather::read_feather,s3_path = "alpha-app-occupeye-automation/surveys.feather")
surveys_hash <- with(surveys_list[c('name','survey_id')],setNames(survey_id,name))

report_list <- s3tools::list_files_in_buckets("alpha-app-occupeye-automation") %>% filter(grepl("\\.feather",path))



# UI function -------------------------------------------------------------
# Constructs the UI

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Report config",
          selectInput(inputId = "survey_name",
                      label = "Select OccupEye survey",
                      choices = surveys_list$name,
                      selected = "102 Petty France v1.2"),

          selectInput(inputId = "raw_feather",
                      label = "Select report to download",
                      choices = gsub("\\.feather","",report_list$filename)),
          
          selectInput(inputId = "start_time",
                      label = "Start time:",
                      choices = time_list,
                      selected = "09:00"),
          
          selectInput(inputId = "end_time",
                      label = "End time:",
                      choices = time_list,
                      selected = "17:00"),
          
          actionButton("loadCSV","Load report"),
          
          actionButton("toggleFilter","Show/hide report filters"),
          
          
          conditionalPanel("input.toggleFilter % 2 == 0",
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
          )
                 
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
        tabPanel("daily usage",plotlyOutput(outputId = "dailyChart"),includeMarkdown("chart_info.md")),
        tabPanel("usage by weekday",plotlyOutput(outputId = "weekdayChart"),includeMarkdown("chart_info.md")),
        tabPanel("usage by desk type",plotlyOutput(outputId = "deskChart"),includeMarkdown("chart_info.md")),
        tabPanel("usage by floor",plotlyOutput(outputId = "floorChart"),includeMarkdown("chart_info.md")),
        tabPanel("summarised data",downloadButton("download_summarised_data"),dataTableOutput(outputId = "df_sum")),
        tabPanel("filtered data",downloadButton("download_filtered_data"),dataTableOutput(outputId = "filtered")),
        tabPanel("raw data",downloadButton("download_raw_data"),dataTableOutput(outputId = "raw_data")),
        tabPanel("bad observations",downloadButton("download_bad_observations"),dataTableOutput(outputId = "bad_observations"))
      )
        
    )
  )
)


# Server function ---------------------------------------------------------
# This function defines the server function. Should be separated into a separate file
server <- function(input,output,session) {
  
  
  # Raw data download -------------------------------------------------------
  
  RV <- reactiveValues(data = temp_df,df_sum = temp_df_sum, filtered = temp_df_sum)
  output$tree <- renderEmptyTree()
  
  
  # Data filter -------------------------------------------------------------
  
  
  # Filter the data based on the input filters. This forms the input for the plots and tables
  update_filter <- function() {
    
    
    #loop through the layers of the team selection tree to get the selected names at each level
    l1Names <- NULL
    l2Names <- NULL
    l3Names <- NULL
    if (is.null(input$tree)){
      "None"
    } else{
      selected <-get_selected(input$tree,"slice")
      
      
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
    
    l1Names <- replace(l1Names,l1Names %in% c("N/A",""),NA)
    l2Names <- replace(l2Names,l2Names %in% c("N/A",""),NA)
    l3Names <- replace(l3Names,l3Names %in% c("N/A",""),NA) # Convert N/A from string to NA to make filtering work
    
    RV$l1Names <- l1Names
    RV$l2Names <- l2Names
    RV$l3Names <- l3Names
    
    
    # apply the filters
    RV$filtered <- RV$df_sum %>%
      dplyr::filter(date >= input$date_range[1] & date <= input$date_range[2],
                    devicetype %in% input$desk_type,
                    floor %in% input$floors,
                    category_1 %in% l1Names,
                    category_2 %in% l2Names,
                    category_3 %in% l3Names)
  }
  
  
  
  # Team selection tree -----------------------------------------------------
  # Creates the UI for selecting the teams
  
  get_team_tree <- function() {
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
    RV$team_tree <- lapply(cat3split,lapply,lapply,function(x) x <- structure(names(x),stselected=TRUE))
  }
  
  get_bad_observations <- function(df) {
    df %>%
      filter(!sensor_value %in% c(1,0))
      mutate(obs_date = date(obs_datetime)) %>%
      group_by(obs_date,sensor_value,surveydeviceid,hardware_id,sensor_id,location) %>% 
      summarise(count = n())
    
  }
  
  observeEvent(input$survey_name, {
    survey_reports <- report_list %>% dplyr::filter(grepl(surveys_hash[input$survey_name],path)) %>% arrange(filename)
    survey_files <- gsub("\\.feather","",survey_reports$filename)
    updateSelectInput(session,inputId = "raw_feather",
                      choices = survey_files)
  })
  
  
  observeEvent(input$loadCSV, {
    
    withProgress(message = paste0("Loading report ",input$raw_feather), {
      feather_path <- report_list %>% dplyr::filter(filename == paste0(input$raw_feather,".feather"),
                                                    grepl(surveys_hash[input$survey_name],path))
      df_min <- s3tools::read_using(FUN=feather::read_feather, s3_path=feather_path$path)
      df_full <- left_join(df_min,sensors,by=c("survey_device_id" = "surveydeviceid")) %>% 
                  rename(surveydeviceid = survey_device_id)
      
      
      RV$data <- df_full
      
    })
    
    withProgress(message="summarising the dataset", {
      RV$df_sum <- get_df_sum(RV$data,input$start_time,input$end_time)
    })
    
    
    
  })
  
  observeEvent(RV$df_sum, {
    
    floor_list <- unique(RV$df_sum$floor) %>% sort()
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
    
    updateTree(session,"tree",data=get_team_tree())
    
  })
  
  # Update the report if any of the filters have changed
  observeEvent({input$tree
    input$floor
    input$date_range
    input$desk_type
    input$smoothing_factor},
    
    {
      RV$filtered <- update_filter()
    }
  )
  
  # Plots and table outputs -------------------------------------------------
  
  # These functions generate the charts and tables in the report
  observeEvent(RV$filtered, {
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
    
    output$filtered <- renderDataTable({
      RV$filtered
    })
    
    output$raw_data <- renderDataTable({
      RV$data
    })
    
    output$bad_observations <- renderDataTable({
      get_bad_observations(RV$data)
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
  
  output$download_summarised_data <- downloadHandler(
    filename = "summarised data.csv",
    content = function(file) {
      write.csv(RV$df_sum, file, row.names = FALSE)
    }
  )
  
  output$download_filtered_data <- downloadHandler(
    filename = "filtered data.csv",
    content = function(file) {
      write.csv(RV$filtered, file, row.names = FALSE)
    }
  )
  
  output$download_raw_data <- downloadHandler(
    filename = "raw data.csv",
    content = function(file) {
      write.csv(RV$data, file, row.names = FALSE)
    }
  )
  
  output$download_bad_observations<- downloadHandler(
    filename = "bad data.csv",
    content = function(file) {
      write.csv(get_bad_observations(RV$data), file, row.names = FALSE)
    }
  )
  
  
  # change to TRUE when deployed
  
  # refreshes connection when grey screened
  
  session$allowReconnect("force")
  
  
}  

shinyApp(ui=ui,server=server)

