
# Library declarations ----------------------------------------------------

library(shiny)          # For the shiny!
library(ggplot2)        # For plotting
library(dplyr)          # For pipes data wrangling
library(htmlwidgets)    # rpivotTable depends on it
library(shinyWidgets)   # For pickerInput
library(plotly)         # Makes ggplot interactive
library(shinyTree)      # for the category tree. Specifically requires the schaffman5 fork on github
library(rpivotTable)    # Pivot tables
library(feather)        # Feather data reading
library(glue)           # Interpreted string literals

# import other source code ------------------------------------------------


source("charting_functions.R")
source("data_cleaning_functions.R")



# Initialise functions ------------------------------------------
# Downloads a sample dataset from S3, and uses it to initialise the UI fields.


temp_df <- s3tools::read_using(FUN = readr::read_csv, s3_path = "alpha-app-occupeye-automation/surveys/336/Unallocated.csv")
temp_df_sum <- get_df_sum(temp_df, "09:00", "17:00")
time_list <- unique(strftime(temp_df$obs_datetime, format = "%H:%M"))
date_list <- unique(lubridate::date(temp_df$obs_datetime))
room_types <- unique(temp_df$roomtype)
device_types <- unique(temp_df$devicetype)
floors <- unique(temp_df$floor)


# Get the surveys table, and make a dictionary of survey names to their IDs. 
# So calling surveys_hash["survey_name"] returns its corresponding survey_id
surveys_list <- s3tools::read_using(FUN = feather::read_feather, s3_path = "alpha-app-occupeye-automation/surveys.feather")
surveys_hash <- with(surveys_list[c("name", "survey_id")], setNames(survey_id, name))

# Get the list of active survey
active_surveys <- s3tools::read_using(FUN = feather::read_feather, s3_path = "alpha-app-occupeye-automation/active surveys.feather")
selected_survey_id <- surveys_hash[1]

# Get the list of reports in the folder of the selected file
report_list <- s3tools::list_files_in_buckets("alpha-app-occupeye-automation", 
                                              prefix = glue("surveys/{selected_survey_id}")) %>% filter(grepl("\\.feather", path))



# UI function -------------------------------------------------------------
# Constructs the UI, starting with the sidebar, which has the user controls

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Report config",
          selectInput(inputId = "survey_name",
                      label = "Select OccupEye survey",
                      choices = active_surveys$surveyname,
                      selected = "102 Petty France v2.1"),

          selectInput(inputId = "raw_feather",
                      label = "Select report to download",
                      choices = gsub("\\.feather", "", report_list$filename)),
          
          dateRangeInput(inputId = "download_date_range",
                         label = "Select date range to download",
                         start = min(date_list),
                         end = max(date_list),
                         min = min(date_list),
                         max = max(date_list)),
          
          selectInput(inputId = "start_time",
                      label = "Start time:",
                      choices = time_list,
                      selected = "09:00"),
          
          selectInput(inputId = "end_time",
                      label = "End time:",
                      choices = time_list,
                      selected = "17:00"),
          
          actionButton("loadCSV", "Load report"),
          
          actionButton("toggleFilter", "Show/hide report filters"),
          
          # Action buttons have an integer value that increments every time it's pressed.
          # Hence, for every even number of clicks, togglefilter's value is even
          # Also note that the test is a javascript expression, hence why it says "input.toggleFilter" rather than "input$toggleFilter"
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
                        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 4"),
                        multiple = TRUE,
                        selected = device_types),
            
            pickerInput(inputId = "floors",
                        label = "Pick floor(s)",
                        choices = floors,
                        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 4"),
                        multiple = TRUE,
                        selected = floors),
    
            
            helpText("Select Department(s) and team(s)"),
            shinyTree("tree", checkbox = TRUE, search = TRUE)
          )
                 
        ),
        
        tabPanel("Download Report",
                 radioButtons("format", "Document format", c("HTML", "Word"),
                              inline = TRUE),
                 downloadButton("download_button", "Generate report")
        )
        
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction",
          fluidPage(
            fluidRow(
              column(8, includeMarkdown("intro.md"))
            )
          )
        ),
        tabPanel("Changelog",
          fluidPage(
            fluidRow(
              column(8,includeMarkdown("changelog.md"))
            )
          )
        ),
        tabPanel("Pivot table", rpivotTableOutput("myPivot")),
        tabPanel("Summary tables",
          fluidPage(
            fluidRow(
               column(6, tableOutput(outputId = "recom_table")),
               column(6, htmlOutput("peak_occupancy_text"),
                      tableOutput(outputId = "peak_occupancy"))
               ),
            fluidRow(
              column(3, tableOutput(outputId = "team_count")),
              column(3, tableOutput(outputId = "desk_count")),
              column(3, tableOutput(outputId = "team_desk_count"))
              )
            )
          ),
        tabPanel("Smoothing", plotlyOutput(outputId = "smoothChart"),
                 numericInput(inputId = "smoothing_factor",
                              label = "Smoothing Factor",
                              min = 0,
                              max = 1,
                              value = 0.5,
                              step = 0.1),
                 htmlOutput(outputId = "smoothing_description")),
        tabPanel("daily usage",
                 plotlyOutput(outputId = "dailyChart"),
                 htmlOutput(outputId = "daily_chart_narrative"),
                 includeMarkdown("chart_info.md")),
        tabPanel("usage by weekday",
                 plotlyOutput(outputId = "weekdayChart"),
                 textOutput(outputId = "weekday_chart_narrative"),
                 includeMarkdown("chart_info.md")),
        tabPanel("usage by desk type",
                 plotlyOutput(outputId = "deskChart"),
                 includeMarkdown("chart_info.md")),
        tabPanel("usage by floor",
                 plotlyOutput(outputId = "floorChart"),
                 includeMarkdown("chart_info.md")),
        tabPanel("summarised data",
                 downloadButton("download_summarised_data"),
                 dataTableOutput(outputId = "df_sum")),
        tabPanel("filtered data",
                 downloadButton("download_filtered_data"),
                 dataTableOutput(outputId = "filtered")),
        tabPanel("raw data",
                 downloadButton("download_raw_data"),
                 dataTableOutput(outputId = "raw_data")),
        tabPanel("bad observations",
                 downloadButton("download_bad_observations"),
                 dataTableOutput(outputId = "bad_observations"))
      )
        
    )
  )
)


# Server function -----------------------------------------------------------
# This function defines the server function, which does the backend calculations
server <- function(input, output, session) {
  
  sensors <- s3tools::read_using(FUN = feather::read_feather, s3_path = "alpha-app-occupeye-automation/sensors.feather") %>%
    mutate_at(.funs = funs(ifelse(. == "", NA, .)), # Feather imports missing values as emptystring, so convert them to NA
              .vars = vars(category_1, category_2, category_3)) # This only pertains to the team categories, so just mutate the team categories
  
  
  # Create and initialise RV, which is a collection of the reactive values
  RV <- reactiveValues(data = temp_df,
                       df_sum = temp_df_sum,
                       filtered = temp_df_sum)
  
  # Initialise the team's shinyTree
  output$tree <- renderEmptyTree()
  
  
  # Data filter -------------------------------------------------------------
  # Filter the data based on the input filters. This forms the input for the plots and tables
  update_filter <- function() {
    
    
  # first, loop through the layers of the team selection tree to get the selected names at each level
    l1Names <- NULL
    l2Names <- NULL
    l3Names <- NULL
    if (is.null(input$tree)){
      "None"
    } else{
      selected <- get_selected(input$tree, "slice")

      
      for (x in selected) {
        l1Names <- c(l1Names, names(x))
        for (y in x) {
          l2Names <- c(l2Names, names(y))
          for (z in y) {
            l3Names <- c(l3Names, names(z))
          }
        }
      }
    }
    
    # Convert N/A back from string to NA to make filtering work
    l1Names <- replace(l1Names, l1Names %in% c("N/A", ""), NA)
    l2Names <- replace(l2Names, l2Names %in% c("N/A", ""), NA)
    l3Names <- replace(l3Names, l3Names %in% c("N/A", ""), NA)
    
    RV$l1Names <- l1Names
    RV$l2Names <- l2Names
    RV$l3Names <- l3Names
    
    
    # apply the filters
    RV$filtered <- RV$df_sum %>%
      dplyr::filter(date >= input$date_range[1] & date <= input$date_range[2],
                    devicetype %in% input$desk_type,
                    floor %in% input$floors,
                    trimws(category_1) %in% l1Names,
                    trimws(category_2) %in% l2Names,
                    trimws(category_3) %in% l3Names)
  }
  
  
  
  # Team selection tree -----------------------------------------------------
  # Creates the UI for selecting the teams
  
  get_team_tree <- function() {
    
    category_list <- RV$df_sum %>%
      select(category_1, category_2, category_3) %>%
      unique %>%
      mutate_all(funs(ifelse(is.na(.) | . == "", "N/A", .))) # replaces nulls/empty string with "N/A" so that ShinyTree works properly
    
    # split the data.frame of categories into a nested list of lists
    cat1split <- with(category_list, split(category_list, category_1))
    cat2split <- lapply(cat1split, function(x) split(x, x$category_2))
    cat3split <- lapply(cat2split, lapply, function(x) split(x, x$category_3))
    
    # replace the category-3 level with the names, 
    # so that the original data frame headings aren't added as an extra layer.
    RV$team_tree <- lapply(cat3split, lapply, lapply, function(x) x <- structure(names(x), stselected = TRUE))
  }
  
  get_bad_observations <- function(df) {
    df %>%
      filter(!sensor_value %in% c(1, 0)) %>%
      mutate(obs_date = date(obs_datetime)) %>%
      group_by(obs_date, sensor_value, surveydeviceid, hardwareid, sensorid, location) %>% 
      summarise(count = n())
    
  }
  
  # event observers -----------------------------------------------------
  
  observeEvent(input$survey_name, {

    selected_survey_id <- surveys_hash[input$survey_name]
    RV$report_list <- s3tools::list_files_in_buckets("alpha-app-occupeye-automation", prefix = glue("surveys/{selected_survey_id}"))

    survey_reports <- RV$report_list %>% arrange(filename)
    survey_files <- gsub("\\.feather", "", survey_reports$filename)
    updateSelectInput(session, inputId = "raw_feather",
                      choices = survey_files)
    
    start_date <- surveys_list %>% filter(survey_id == selected_survey_id) %>% .$startdate
    dates_list <- seq(as.Date(start_date), as.Date(today() - 1), by = "day")
    updateDateRangeInput(session, inputId = "download_date_range",
                         min = min(dates_list, na.rm = TRUE),
                         max = max(dates_list, na.rm = TRUE),
                         start = today() - months(1),
                         end = max(dates_list, na.rm = TRUE))
  })
  
  # When clicking the "load report" button...
  observeEvent(input$loadCSV, {
    
    # Add a progress bar
    withProgress(message = paste0("Loading report ", input$raw_feather), {

      # find the s3 path for the selected report
      feather_path <- RV$report_list %>% dplyr::filter(filename == paste0(input$raw_feather, ".feather"))
      
      # Download the minimal table, filtered by the download_date_range
      df_min <- s3tools::read_using(FUN = feather::read_feather, s3_path = feather_path$path) %>%
        filter(obs_datetime >= input$download_date_range[1], obs_datetime <= paste0(input$download_date_range[2]," 23:50"))
      
      # Add the other sensor metadata, dealing with the inconsistently named survey_device_id and surveydeviceid
      df_full <- left_join(df_min, sensors, by = c("survey_device_id" = "surveydeviceid")) %>% 
        rename(surveydeviceid = survey_device_id)
      
      # add the raw data for displaying on the raw data tab
      RV$data <- df_full
      
      # make the bad sensors analysis
      RV$bad_sensors <- get_bad_observations(RV$data)
    })
    
    # Then summarise the dataset
    withProgress(message = "summarising the dataset", {
      RV$df_sum <- get_df_sum(RV$data, input$start_time, input$end_time)
    
      # show dialog to show it's finished loading
      showModal(modalDialog(glue("{input$raw_feather} successfully loaded into the dashboard."), easyClose = TRUE))
    })
  })
  
  # Once it sees that RV$df_sum has updated, update the filter UI with metadata from new dataset
  observeEvent(RV$df_sum, {
    
    # Get the list of floors
    floor_list <- unique(RV$df_sum$floor) %>% as.numeric() %>% sort()
    
    # Get the list of desk types, grouped by room type
    unique_device_types <- RV$df_sum %>% select(roomtype, devicetype) %>% unique()
    desk_type_list <- lapply(split(unique_device_types$devicetype,
                                   unique_device_types$roomtype),
                             as.list)
    # get new list of dates
    date_list <- unique(RV$df_sum$date)
    
    # Update the UI
    updatePickerInput(session, inputId = "floors",
                      choices = floor_list,
                      selected = floor_list)
    updatePickerInput(session, inputId = "desk_type",
                      choices = desk_type_list,
                      selected = desk_type_list$`Desk Setting`)
    updateDateRangeInput(session, inputId = "date_range",
                         min = min(date_list, na.rm = TRUE),
                         max = max(date_list, na.rm = TRUE),
                         start = min(date_list, na.rm = TRUE),
                         end = max(date_list, na.rm = TRUE))
    
    updateTree(session, "tree", data = get_team_tree())
    
  })
  
  # Update the report if any of the filters have changed
  observeEvent({
    input$tree
    input$floors
    input$date_range
    input$desk_type
    input$smoothing_factor
    },
    
    {
      RV$filtered <- update_filter()
    }
  )
  
  
  # Plots and table outputs -------------------------------------------------
  
  
  
  # These functions generate the charts and tables in the report, only when the filter gets updated
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
    
    output$peak_occupancy_text <- renderText({
      "<b>Top 10 busiest days (in use + underutilised)</b>"
    })
    
    output$peak_occupancy <- renderTable({
      isolate(get_peak_occupancy(RV$filtered))
    })
    
    output$smoothChart <- renderPlotly({
      smoothing_chart(RV$filtered, input$smoothing_factor)
    })
    
    output$smoothing_description <- renderText({
      paste("The graph shows the difference in implied desk utilisation under the assumption of full smoothing over the week and the assumption of imperfect smoothing. 
    A smoothing factor of 0.5 represents the midpoint between current utilisation and full smoothing.",
            get_smoothing_narrative(RV$filtered, input$smoothing_factor), sep = "<br/>")
    })
    
    output$daily_chart_narrative <- renderText({
      daily_usage_chart_narrative(RV$filtered)
    })
    
    output$dailyChart <- renderPlotly({
      isolate(prop_daily_usage_chart(RV$filtered))
    })
    
    output$weekday_chart_narrative <- renderText({
      weekday_usage_narrative(RV$filtered)
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
      RV$bad_sensors
    })
    
    
  })
  # Download handler --------------------------------------------------------
  
  # Functions for handling the report download  
  output$download_button <- downloadHandler(
    filename = function() {
      paste("my-report", sep = '.', switch(
        input$format, PDF = "pdf", HTML = "html", Word = "docx"
      ))
    },
    
    content = function(file) {
      
      out_report <- switch(
        input$format, PDF = "pdf", HTML = "slidy_report.Rmd", Word = "word_report.Rmd"
      )
      
      src <- normalizePath(out_report)
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, out_report, overwrite = TRUE)
      
      
      # Downlaods a template for the word report.
      # Note - this has a specially modified style, in which Heading 5 has been adapted into a line break.
      # See here: https://scriptsandstatistics.wordpress.com/2015/12/18/rmarkdown-how-to-inserts-page-breaks-in-a-ms-word-document/
      word_report_reference <- s3tools::download_file_from_s3("alpha-app-occupeye-automation/occupeye-report-reference.dotx",
                                                              "occupeye-report-reference.dotx",
                                                              overwrite = TRUE)
      
      # Generate report, with progress bar
      withProgress(message = "Generating report...", {
        out <- rmarkdown::render(out_report, 
                                 params = list(start_date = input$date_range[1],
                                               end_date = input$date_range[2], 
                                               df_sum = RV$filtered))
        file.rename(out, file)
      })
      
    }
    
  )
  
  # Download handlers for the different datasets -------------------
  
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
  
  output$download_bad_observations <- downloadHandler(
    filename = "bad data.csv",
    content = function(file) {
      write.csv(RV$bad_sensors, file, row.names = FALSE)
    }
  )
  
  
  # change to TRUE when deployed
  
  # refreshes connection when grey screened
  
  session$allowReconnect(TRUE)
  
  
}  

# launch the app
shinyApp(ui = ui, server = server)

