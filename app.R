# Library declarations ----------------------------------------------------

library(shiny)          # For the shiny!
library(ggplot2)        # For plotting
library(dplyr)          # For pipes data wrangling
library(htmlwidgets)    # rpivotTable depends on it
library(shinyWidgets)   # For pickerInput
# library(plotly)         # Makes ggplot interactive
library(shinyTree)      # for the category tree.
library(rpivotTable)    # Pivot tables
library(feather)        # Feather data reading
library(glue)           # Interpreted string literals
library(s3tools)        # S3tools for getting stuff from S3
library(reticulate)
library(dbtools)
library(flexdashboard)
library(rhandsontable)
library(shinycssloaders)
library(gridExtra)


# import other source code ------------------------------------------------

print("source charting_functions")
source("charting_functions.R")
print("source data cleaning functions")
source("data_cleaning_functions.R")


# UI function -------------------------------------------------------------
# Constructs the UI, starting with the sidebar, which has the user controls

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        
        # Report Config -----------------------------------------------------------
        
        
        tabPanel("Report config",
                 uiOutput("survey_name"),
                 uiOutput("download_date_range"),
                 uiOutput("start_time"),
                 uiOutput("end_time"),
                 
                 
                 actionButton("loadCSV", "Load report"),
                 
                 actionButton("toggleFilter", "Show/hide report filters"),
                 
                 # Action buttons have an integer value that increments every time it's pressed.
                 # Hence, for every even number of clicks, togglefilter's value is even
                 # Also note that the test is a javascript expression, hence why it says "input.toggleFilter" rather than "input$toggleFilter"
                 conditionalPanel("input.toggleFilter % 2 == 0",
                                  uiOutput("date_range"),
                                  uiOutput("buildings"),
                                  uiOutput("floors"),
                                  uiOutput("zones"),
                                  uiOutput("desk_type"),
                                  uiOutput("desks"),
                                  helpText("Select Department(s) and team(s)"),
                                  shinyTree("tree", checkbox = TRUE, search = TRUE)
                 )
                 
                 
        ),
        
        # Download Report menu ----------------------------------------------------
        
        
        tabPanel("Download Report",
                 radioButtons(inputId = "format",
                              label = "Document format",
                              choices = c("By team",
                                          "By floor",
                                          "By floor and team",
                                          "By building"),
                              inline = FALSE),
                 downloadButton("download_button", "Generate report")
        )
        
      )
    ),
    
    # MainPanel ---------------------------------------------------------------
    
    
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
                     column(4, tableOutput(outputId = "team_count")),
                     column(2, tableOutput(outputId = "desk_count")),
                     column(4, tableOutput(outputId = "team_desk_count"))
                   )
                 )
        ),
        tabPanel("Smoothing", plotOutput(outputId = "smoothChart"),
                 numericInput(inputId = "smoothing_factor",
                              label = "Smoothing Factor",
                              min = 0,
                              max = 1,
                              value = 0.5,
                              step = 0.1),
                 htmlOutput(outputId = "smoothing_description")),
        tabPanel("Daily usage",
                 plotOutput(outputId = "dailyChart"),
                 htmlOutput(outputId = "daily_chart_narrative"),
                 includeMarkdown("chart_info.md")),
        tabPanel("Usage by weekday",
                 plotOutput(outputId = "weekdayChart"),
                 textOutput(outputId = "weekday_chart_narrative"),
                 includeMarkdown("chart_info.md")),
        tabPanel("Usage by desk type",
                 plotOutput(outputId = "deskChart"),
                 includeMarkdown("chart_info.md")),
        tabPanel("Usage by floor",
                 plotOutput(outputId = "floorChart"),
                 includeMarkdown("chart_info.md")),
        tabPanel("Summarised data",
                 downloadButton("download_summarised_data"),
                 dataTableOutput(outputId = "df_sum")),
        tabPanel("Filtered data",
                 downloadButton("download_filtered_data"),
                 dataTableOutput(outputId = "filtered")),
        tabPanel("Raw data",
                 downloadButton("download_raw_data"),
                 dataTableOutput(outputId = "raw_data")),
        tabPanel("Bad observations",
                 downloadButton("download_bad_observations"),
                 dataTableOutput(outputId = "bad_observations")),
        tabPanel("Admin",
                 fluidPage(
                   fluidRow(
                     column(4,uiOutput('all_survey_names')),
                     column(3,
                            actionButton(inputId = "add_survey_names",
                                         label = "Add survey(s) to list",
                                         icon = icon("arrow-right")),
                            actionButton(inputId = "remove_survey_names",
                                         label = "Remove survey(s) from list",
                                         icon = icon("arrow-left")),
                            actionButton("update_survey_names","Confirm list update")),
                     column(4,uiOutput('survey_name_admin'))
                   )
                 )),
        
        
        # NPS UI ------------------------------------------------------------------
        
        
        tabPanel("NPS rooms",
                 tabsetPanel(
                   tabPanel("Selected rooms",
                            fluidPage(
                              fluidRow(
                                h1("Filtered rooms"),
                                p("This view shows analysis for the data with the filters applied"),
                                numericInput("filtered_room_target",
                                             "Set target occupancy",
                                             min = 0,
                                             max = 1,
                                             step = 0.1,
                                             value = 0.5),
                                htmlOutput(outputId = "filtered_donut_narrative"),
                                column(4,
                                       plotOutput(outputId = "filtered_gauge", height = "300px") %>% withSpinner()
                                ),
                                column(6,
                                       plotOutput(outputId = "filtered_waffle", height = "300px") %>% withSpinner()
                                )),
                              fluidRow(
                                plotOutput(outputId = "filtered_weekly_plot"),
                                plotOutput(outputId = "filtered_room_distribution")
                              )
                            )
                   ),
                   tabPanel("Group Rooms",
                            numericInput("group_room_target",
                                         "Set target occupancy",
                                         min = 0,
                                         max = 1,
                                         step = 0.1,
                                         value = 0.6),
                            uiOutput("group_room_ui")
                   ),
                   tabPanel("Interview Rooms",
                            numericInput("interview_room_target",
                                         "Set target occupancy",
                                         min = 0,
                                         max = 1,
                                         step = 0.1,
                                         value = 0.6),
                            uiOutput("interview_room_ui")
                   )
                 )
                 
                 
        ),
        tabPanel("NPS Resource needs",
                 
                 fluidPage(
                   fluidRow(
                     numericInput(inputId = "fte",
                                  label = "Input FTE",
                                  value = 50,
                                  min = 1),
                     numericInput(inputId = "space_per_fte",
                                  label = HTML("Input space required per FTE (m<sup>2</sup>)"),
                                  value = 8),
                     column(4,
                            h4("Update/add to resource requirement ratios here:"),
                            rHandsontableOutput("resource_hot"),
                            h4("Update area requirements for rooms here:"),
                            rHandsontableOutput("room_footage_hot"),
                            h4("Update/add to anciliary spaces here:"),
                            rHandsontableOutput("anciliary_space_hot")),
                     column(4,
                            tableOutput("room_resource_requirements"),
                            tableOutput("anciliary_space_requirements"),
                            tableOutput("fte_resource_table"),
                            tableOutput("fte_resource_breakdown_table"),
                            tableOutput("total_resource_table"),
                            tableOutput("current_resource_levels")
                     )
                   )
                 )
        )
        
      )
    )
  )
)


# Server function -----------------------------------------------------------
# This function defines the server function, which does the backend calculations
server <- function(input, output, session) {
  
  
  # Initialise functions ------------------------------------------
  
  
  print("Getting active surveys list")
  # Get the list of active survey
  active_surveys_list <- s3tools::read_using(FUN = feather::read_feather, 
                                             s3_path = "alpha-app-occupeye-automation/active surveys.feather") %>% 
    pull(surveyname) %>% as.character()
  
  # Get the surveys table, and make a dictionary of survey names to their IDs. 
  # So calling surveys_hash["survey_name"] returns its corresponding survey_id
  surveys <- s3tools::read_using(readr::read_csv, "alpha-app-occupeye-automation/raw_data_v5/surveys/data.csv")
  active_surveys <- surveys %>% dplyr::filter(name %in% active_surveys_list)
  surveys_hash <- with(active_surveys[c("name", "survey_id")], setNames(survey_id, name))
  initial_survey_id <- surveys_hash[[1]]
  
  
  invalid_surveys <- setdiff(active_surveys_list, surveys$name)
  if(length(invalid_surveys) > 0) {
    showModal(modalDialog(HTML(glue("The following surveys in the list active surveys appear to have been removed from the list of surveys.
                               Consider reviewing the active surveys list in the Admin tab: <br> {paste(invalid_surveys, collapse = '<br>')}"))))
  }
  
  
  RV <- reactiveValues(surveys = surveys,
                       active_surveys = active_surveys,
                       active_surveys_list = active_surveys_list,
                       surveys_hash = surveys_hash)
  
  # sql <- get_df_sql(initial_survey_id, start_date = Sys.Date() %m-% months(1))
  # print(glue("sql: {sql}"))
  # df_min <- dbtools::read_sql(sql)
  # 
  # sensors <- s3tools::read_using(readr::read_csv, glue("alpha-app-occupeye-automation/raw_data_v5/sensors/survey_id={initial_survey_id}/data.csv")) %>%
  #   mutate(surveydeviceid = as.character(surveydeviceid)) %>% # coerce surveydeviceid to char to maintain type integrity
  #   mutate_at(.funs = funs(ifelse(is.na(.), "N/A",.)),
  #             .vars = vars(roomname, location))
  # 
  # temp_df <- get_full_df(df_min, sensors)
  
  #temp_df <- s3tools::read_using(FUN = feather::read_feather, s3_path = "alpha-app-occupeye-automation/temp_df.feather")
  
  
  
  temp_df <- s3tools::read_using(FUN = readr::read_csv, s3_path = "alpha-app-occupeye-automation/surveys/336/Unallocated.csv")
  
  
  temp_df_sum <- get_df_sum(temp_df, "09:00", "17:00")
  time_list <- unique(strftime(temp_df$obs_datetime, format = "%H:%M"))
  date_list <- unique(lubridate::date(temp_df$obs_datetime))
  buildings <- unique(temp_df$building)
  room_types <- unique(temp_df$roomtype)
  device_types <- unique(temp_df$devicetype)
  floors <- unique(temp_df$floor)
  zones <- unique(temp_df$roomname)
  desks <- unique(temp_df$location)
  
  # Create and initialise RV, which is a collection of the reactive values
  RV$data = temp_df
  RV$df_sum = temp_df_sum
  RV$filtered = temp_df_sum
  RV$bad_sensors = get_bad_observations(temp_df)
  
  output$download_date_range <- renderUI({
    print("rendering download_date_range")
    dateRangeInput(inputId = "download_date_range",
                   label = "Select date range to download",
                   start = min(date_list),
                   end = max(date_list),
                   min = min(date_list),
                   max = max(date_list))
  })
  
  output$start_time <- renderUI({
    print("render start time")
    selectInput(inputId = "start_time",
                label = "Start time:",
                choices = time_list,
                selected = "09:00")
  })
  
  output$end_time <- renderUI({
    print("render end time")
    selectInput(inputId = "end_time",
                label = "End time:",
                choices = time_list,
                selected = "17:00")
  })
  
  output$survey_name <- renderUI({
    print("render survey_name pickerinput")
    pickerInput(inputId = "survey_name",
                label = "Select OccupEye survey",
                choices = RV$active_surveys_list,
                selected = RV$active_surveys_list[1],
                multiple = TRUE)
  })
  
  output$all_survey_names <- renderUI({
    print("render all_survey_names")
    selectInput(inputId = "all_survey_names",
                label = "All surveys",
                choices = surveys$name,
                multiple = TRUE,
                selectize = FALSE,
                size = 12)
  })
  
  output$survey_name_admin <- renderUI({
    selectInput(inputId = "survey_name_admin",
                label = "Active surveys",
                choices = RV$active_surveys_list,
                multiple = TRUE,
                selectize = FALSE,
                size = 12)
  })
  
  output$date_range <- renderUI({
    dateRangeInput(inputId = "date_range",
                   label = "Select sample date range",
                   start = min(date_list),
                   end = max(date_list),
                   min = min(date_list),
                   max = max(date_list))
  })
  
  output$buildings <- renderUI({
    pickerInput(inputId = "buildings",
                label = "Pick building(s)",
                choices = buildings,
                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 4"),
                multiple = TRUE,
                selected = buildings)
  })
  
  output$floors <- renderUI({
    pickerInput(inputId = "floors",
                label = "Pick floor(s)",
                choices = floors,
                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 4"),
                multiple = TRUE,
                selected = floors)
  })
  
  output$zones <- renderUI({
    pickerInput(inputId = "zones",
                label = "Pick zone(s)",
                choices = zones,
                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 4"),
                multiple = TRUE,
                selected = zones)
  })
  
  output$desk_type <- renderUI({
    pickerInput(inputId = "desk_type",
                label = "Pick desk type(s)",
                choices = device_types,
                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 4"),
                multiple = TRUE,
                selected = device_types)
  })
  
  output$desks <- renderUI({
    pickerInput(inputId = "desks",
                label = "Pick desks(s)",
                choices = desks,
                options = list(`actions-box` = TRUE,
                               `selected-text-format` = "count > 4",
                               `live-search` = TRUE),
                multiple = TRUE,
                selected = desks)
  })
  
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
      tree_initialised <-FALSE
    } else{
      tree_initialised <- TRUE
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
                    building %in% input$buildings,
                    floor %in% input$floors,
                    roomname %in% input$zones,
                    location %in% input$desks)
    
    if(tree_initialised) {
      
      RV$filtered <- RV$filtered %>%
        dplyr::filter(trimws(category_1) %in% l1Names,
                      trimws(category_2) %in% l2Names,
                      trimws(category_3) %in% l3Names)
    }
    
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
  
  
  # event observers -----------------------------------------------------
  
  observeEvent(input$survey_name, {
    
    selected_survey_ids <- RV$surveys_hash[input$survey_name]
    print(glue("selected survey ids: {paste(selected_survey_ids, collapse = ', ')}"))
    start_date <- RV$active_surveys %>% dplyr::filter(survey_id %in% selected_survey_ids) %>% pull(startdate) %>% min
    end_date <- RV$active_surveys %>% dplyr::filter(survey_id %in% selected_survey_ids) %>% pull(enddate) %>% max
    
    print(start_date)
    print(end_date)
    dates_list <- seq(as.Date(start_date), min(as.Date(end_date), as.Date(today() - 1)), by = "day")
    updateDateRangeInput(session, inputId = "download_date_range",
                         min = min(dates_list, na.rm = TRUE),
                         max = max(dates_list, na.rm = TRUE),
                         start = max(min(today() %m-% months(1), as.Date(end_date) %m-% months(1)),as.Date(start_date)),
                         end = max(dates_list, na.rm = TRUE))
  })
  
  # When clicking the "load report" button...
  observeEvent(input$loadCSV, {
    print(glue("Loading sensors for surveys {paste0(RV$surveys_hash[input$survey_name], collapse = ', ')}"))
    
    withProgress(message = "Loading sensor information for selected survey(s)...", {
      RV$sensors <- dbtools::read_sql(glue("select * from occupeye_app_db.sensors where survey_id in ({paste0(RV$surveys_hash[input$survey_name], collapse = ', ')})")) %>%
        mutate(surveydeviceid = as.character(surveydeviceid)) %>% # coerce surveydeviceid to char to maintain type integrity
        mutate_at(.funs = funs(ifelse(is.na(.), "N/A",.)),
                  .vars = vars(roomname, location))
    })
    
    # Store the selected survey name to log what survey is currently loaded, in case the selection is changed in the dropdown later
    RV$survey_name <- input$survey_name
    
    # Add a progress bar
    
    withProgress(message = paste0("Loading report ", paste0(input$survey_name, collapse = ", ")), {
      start.time <- Sys.time()
      
      sql <- get_df_sql(RV$surveys_hash[input$survey_name],
                        start_date = input$download_date_range[1], 
                        end_date = input$download_date_range[2],
                        start_time = input$start_time,
                        end_time = input$end_time)
      
      print(glue("executing query: {sql}"))
      
      
      # Download the minimal table, filtered by the download_date_range
      df_min <- dbtools::read_sql(sql)
      
      end.time <- Sys.time()
      diff <- end.time - start.time
      print(diff)
      # Add the other sensor metadata, dealing with the inconsistently named survey_device_id and surveydeviceid
      df_full <- left_join(df_min, RV$sensors, by = c("survey_device_id" = "surveydeviceid")) %>% 
        rename(surveydeviceid = survey_device_id)
      
      # add the raw data for displaying on the raw data tab
      RV$data <- df_full %>%
        clean_and_mutate_raw_data() %>%
        remove_non_business_days()
      
      feather::write_feather(RV$data, "temp_df.feather")
      
      s3tools::write_file_to_s3("temp_df.feather",s3_path = "alpha-app-occupeye-automation/temp_df.feather", overwrite = T)
      
      # make the bad sensors analysis
      RV$bad_sensors <- get_bad_observations(RV$data)
    })
    
    # Then summarise the dataset
    withProgress(message = "summarising the dataset", {
      RV$df_sum <- get_df_sum(RV$data, input$start_time, input$end_time)
      
      # show dialog to show it's finished loading
      showModal(modalDialog(glue("{paste0(input$survey_name, collapse = ', ')} successfully loaded into the dashboard."), easyClose = TRUE))
    })
  })
  
  # Once it sees that RV$df_sum has updated, update the filter UI with metadata from new dataset
  observeEvent(RV$df_sum, {
    print("Updating RV$df_sum")
    # Get the list of buildings
    building_list <- unique(RV$df_sum$building) %>% sort()
    
    # Get the list of floors
    floor_list <- unique(RV$df_sum$floor) %>% as.numeric() %>% sort()
    
    # Get the list of desk types, grouped by room type
    unique_device_types <- RV$df_sum %>% select(roomtype, devicetype) %>% unique()
    desk_type_list <- lapply(split(unique_device_types$devicetype,
                                   unique_device_types$roomtype),
                             as.list)
    
    # Get list of zones
    
    zone_list <- unique(RV$df_sum$roomname) %>% sort()
    
    # Get list of desks
    desks_list <- unique(RV$df_sum$location) %>% sort()
    
    # get new list of dates
    date_list <- unique(RV$df_sum$date)
    
    # Update the UI
    updatePickerInput(session, inputId = "buildings",
                      choices = building_list,
                      selected = building_list)
    updatePickerInput(session, inputId = "floors",
                      choices = floor_list,
                      selected = floor_list)
    updatePickerInput(session, inputId = "desk_type",
                      choices = desk_type_list,
                      selected = desk_type_list$`Meeting Room`)
    
    updatePickerInput(session, inputId = "zones",
                      choices = zone_list,
                      selected = zone_list)
    
    updatePickerInput(session, inputId = "desks",
                      choices = desks_list,
                      selected = desks_list)
    
    
    updateDateRangeInput(session, inputId = "date_range",
                         min = min(date_list, na.rm = TRUE),
                         max = max(date_list, na.rm = TRUE),
                         start = min(date_list, na.rm = TRUE),
                         end = max(date_list, na.rm = TRUE))
    
    print("Updating Tree")
    
    updateTree(session, "tree", data = get_team_tree())
    
    
    
  })
  
  observeEvent(input$add_survey_names, {
    asl <- RV$active_surveys_list
    RV$active_surveys_list <- unique(c(input$all_survey_names, asl))
    RV$active_surveys <- surveys %>% dplyr::filter(name %in% RV$active_surveys_list)
    RV$surveys_hash <- with(RV$active_surveys[c("name", "survey_id")], setNames(survey_id, name))
    new_choices <- RV$active_surveys_list
    
    updateSelectInput(session = session, 
                      inputId = "survey_name_admin", 
                      choices = new_choices)
  })
  
  observeEvent(input$remove_survey_names, {
    RV$active_surveys_list <- RV$active_surveys_list[!RV$active_surveys_list %in% input$survey_name_admin]
    new_choices <- RV$active_surveys_list
    
    updateSelectInput(session = session, 
                      inputId = "survey_name_admin",
                      choices = new_choices)
  })
  
  observeEvent(input$update_survey_names, {
    RV$active_surveys <- surveys %>% dplyr::filter(name %in% RV$active_surveys_list)
    RV$surveys_hash <- with(RV$active_surveys[c("name", "survey_id")], setNames(survey_id, name))
    updateSelectInput(session, inputId = "survey_name", choices = RV$active_surveys_list)
    
    my_df <- data.frame(surveyname = RV$active_surveys_list)
    feather::write_feather(my_df, "active surveys.feather")
    s3tools::write_file_to_s3("active surveys.feather", "alpha-app-occupeye-automation/active surveys.feather", overwrite = TRUE)
    
    showModal(modalDialog(HTML(glue("Survey list saved. Current list of active surveys: <br> {paste(RV$active_surveys_list, collapse = '<br>')}"))))
    
    
  })
  
  # Update the report if any of the filters have changed
  observeEvent({
    input$tree
    input$floors
    input$buildings
    input$date_range
    input$desk_type
    input$smoothing_factor
    input$zones
    input$desks
  },
  
  {
    RV$filtered <- update_filter()
  }
  )
  
  
  # Plots and table outputs -------------------------------------------------
  
  
  
  # These functions generate the charts and tables in the report, only when the filter gets updated
  observeEvent(RV$filtered, {
    print("Observing RV$filtered")
    
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
    
    output$smoothChart <- renderPlot({
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
    
    output$dailyChart <- renderPlot({
      isolate(prop_daily_usage_chart(RV$filtered))
    })
    
    output$weekday_chart_narrative <- renderText({
      weekday_usage_narrative(RV$filtered)
    })
    
    output$weekdayChart <- renderPlot({
      isolate(prop_weekday_usage_chart(RV$filtered))
    })
    
    output$deskChart <- renderPlot({
      isolate(prop_desk_usage_chart(RV$filtered))
    })
    
    output$floorChart <- renderPlot({
      isolate(prop_floor_usage_chart(RV$filtered))
    })
    
    output$df_sum <- renderDataTable({
      RV$df_sum
    })
    
    output$filtered <- renderDataTable({
      filtered_room_df()
    })
    
    output$raw_data <- renderDataTable({
      RV$data
    })
    
    output$bad_observations <- renderDataTable({
      RV$bad_sensors
    })
    
    
    
  })
  
  # NPS render functions ----------------------------------------------------
  
  filtered_room_df <- reactive({
    print("Filtering filtered_room_df")
    df <- RV$data %>%
      dplyr::filter(devicetype %in% input$desk_type,
                    building %in% input$buildings,
                    floor %in% input$floors,
                    roomname %in% input$zones,
                    location %in% input$desks,
                    category_1 %in% RV$l1Names,
                    category_2 %in% RV$l2Names,
                    category_3 %in% RV$l3Names)
    df
  })
  
  group_room_df <- reactive({
    filtered_room_df() %>%
      dplyr::filter(devicetype == "Group Room")
  })
  
  interview_room_df <- reactive({
    filtered_room_df() %>%
      dplyr::filter(devicetype == "Interview Room")
  })
  
  output$filtered_gauge <- renderPlot({
    vertical_gauge_chart(filtered_room_df(), input$filtered_room_target)
    
    
  })
  
  output$filtered_waffle <- renderPlot({
    room_waffle_chart(filtered_room_df(), input$filtered_room_target)
  })
  
  output$filtered_donut_narrative <- renderText({
    nps_donut_narrative(filtered_room_df(), input$filtered_room_target)
    
  })
  
  output$filtered_weekly_plot <- renderPlot({
    
    weekday_usage_chart(filtered_room_df())
    
  })
  
  output$filtered_room_distribution <- renderPlot({
    concurrent_room_usage_chart(filtered_room_df())
  })
  
  
  
  output$group_room_ui <- renderUI({
    fluidPage(
      
      fluidRow(
        h1("Group Rooms"),
        column(4,
               plotOutput(outputId = "group_gauge", height = "300px") %>% withSpinner()
        ),
        column(6,
               plotOutput(outputId = "group_waffle", height = "300px") %>% withSpinner()
        )),
      fluidRow(
        plotOutput(outputId = "group_weekly_plot"),
        plotOutput(outputId = "group_room_distribution")
      )
    )
    
  })
  
  output$group_gauge <- renderPlot({
    if(nrow(group_room_df()) > 0){vertical_gauge_chart(group_room_df(),
                                                       input$group_room_target)
    } else {
      error_chart(" ")
    }
    
    
  })
  
  output$group_waffle <- renderPlot({
    if(nrow(group_room_df()) > 0) {room_waffle_chart(group_room_df(),
                                                     input$group_room_target)
    } else {error_chart("There are no Group Rooms in this survey.")}
  })
  
  output$group_donut_narrative <- renderText({
    nps_donut_narrative(group_room_df(), input$group_room_target)
    
  })
  
  output$group_weekly_plot <- renderPlot({
    if(nrow(group_room_df()) > 0) {
      weekday_usage_chart(group_room_df())
    } else(error_chart(" "))
  })
  
  output$group_room_distribution <- renderPlot({
    if(nrow(group_room_df()) > 0) {
      concurrent_room_usage_chart(group_room_df())
    } else(error_chart(" "))
  })
  
  output$interview_room_ui <- renderUI({
    fluidPage(
      fluidRow(
        h1("Interview Rooms"),
        column(4,
               plotOutput(outputId = "interview_gauge", height = "300px") %>% withSpinner()
        ),
        column(6,
               plotOutput(outputId = "interview_waffle", height = "300px") %>% withSpinner()
        )),
      fluidRow(
        plotOutput(outputId = "interview_weekly_plot"),
        plotOutput(outputId = "interview_room_distribution")
      )
    )
  })
  
  output$interview_gauge <- renderPlot({
    if(nrow(interview_room_df() > 0)) {
      vertical_gauge_chart(interview_room_df(), input$interview_room_target)
    } else(error_chart(" "))
    
  })
  
  output$interview_waffle <- renderPlot({
    if(nrow(interview_room_df()) > 0) {
      room_waffle_chart(interview_room_df(),
                        input$interview_room_target)
    } else(error_chart("There are no interview rooms in this selection"))
  })
  
  output$interview_donut_narrative <- renderText({
    nps_donut_narrative(interview_room_df(), input$interview_room_target)
    
  })
  
  output$interview_weekly_plot <- renderPlot({
    if(nrow(interview_room_df()) > 0) {
      
      weekday_usage_chart(interview_room_df())
    } else(error_chart(" "))
  })
  
  output$interview_room_distribution <- renderPlot({
    if(nrow(interview_room_df()) > 0) {
      concurrent_room_usage_chart(interview_room_df())
    } else(error_chart(" "))
  })
  
  # probably should shift this to an s3 file rather than hard-coding.
  get_resource_df <- function() {
    tribble(
      ~resource_name, ~resource, ~per_fte,
      "Long Stay Desks", 8, 10,
      "Touchdown", 1, 20,
      "Quiet/phone room", 1, 20,
      "Open Meeting", 1, 30,
      "Breakout", 1, 40,
      "Tea point", 1, 50,
      "Print & copy", 1, 100,
      "Lockers", 1, 1,
      "File Storage", 0.5, 1
    )
  }
  
  
  output$resource_hot <- renderRHandsontable({
    resource_df <- get_resource_df() %>%
      mutate_all(as.character)
    rhandsontable(resource_df,
                  colHeaders = snakecase::to_sentence_case(names(resource_df))) %>%
      hot_cols(format = "0")
    
  })
  
  output$room_footage_hot <- renderRHandsontable({
    df <- data.frame(resource_name = c("Group Room", "Interview Room"),
                     space_required = c(22,9))
    rhandsontable(df %>% mutate_all(as.character)) %>%
      hot_cols(format = "0")
    
  })
  
  output$anciliary_space_hot <- renderRHandsontable({
    df <- data.frame(resource_name = c("Reception", "Waiting room"),
                     qty = c(1, 1),
                     space_required = c(10, 15))
    
    rhandsontable(df %>% mutate_all(as.character)) %>%
      hot_cols(format = "0")
  })
  
  output$fte_resource_table <- renderTable({
    
    if(is.null(input$resource_hot)) {
      return(NULL)
    }
    else{
      get_fte_resource_requirements(input$fte, input$space_per_fte)
    }
  },
  caption = "Staff side total space",
  caption.placement = "top")
  
  output$fte_resource_breakdown_table <- renderTable({
    if(is.null(input$resource_hot)) {
      return(NULL)
    }
    else{
      get_staff_accommodation_requirements(hot_to_r(input$resource_hot),
                                           input$fte)
    }
  },
  caption = "Within which we propose to provide",
  caption.placement = "top")
  
  output$current_resource_levels <- renderTable({
    RV$data %>%
      group_by(devicetype) %>%
      summarise("count" = n_distinct(surveydeviceid),
                "occupancy" = scales::percent(mean(sensor_value, na.rm = T)))
  },
  caption = "Current resource usage",
  caption.placement = "top")
  
  output$room_resource_requirements <- renderTable({
    
    if(is.null(input$room_footage_hot)) {
      return(NULL)
    } else{
      get_room_resource_requirements(RV$data,
                                     input$group_room_target,
                                     input$interview_room_target,
                                     hot_to_r(input$room_footage_hot))
    }
  },
  caption = "Service user space",
  caption.placement = "top")
  
  
  output$anciliary_space_requirements <- renderTable({
    hot_to_r(input$anciliary_space_hot) %>%
      mutate(total_space = make_numeric(qty) * make_numeric(space_required))
    
  },
  caption = "Anciliary space requirements",
  caption.placement = "top")
  
  
  output$total_resource_table <- renderTable({
    shiny::req(RV$data,
               input$group_room_target,
               input$interview_room_target,
               input$room_footage_hot)
    room_resources <- get_room_resource_requirements(RV$data,
                                                     input$group_room_target,
                                                     input$interview_room_target,
                                                     hot_to_r(input$room_footage_hot)) %>%
      select(resource_name = devicetype,
             qty = recommended_rooms,
             space_required,
             total_space) %>%
      mutate_all(as.character)
    
    fte_resources <- get_fte_resource_requirements(input$fte,
                                                   input$space_per_fte) %>%
      mutate_all(as.character)
    
    anciliary_resources <- hot_to_r(input$anciliary_space_hot) %>%
      mutate(total_space = make_numeric(qty) * make_numeric(space_required)) %>%
      mutate_all(as.character)
    
    bind_rows(room_resources, fte_resources, anciliary_resources) %>%
      mutate(total_space = as.numeric(total_space)) %>%
      janitor::adorn_totals()
    
    
    
    
  })
  
  # Download handler --------------------------------------------------------
  
  # Functions for handling the report download  
  output$download_button <- downloadHandler(
    filename = "my-report.docx",
    
    content = function(file) {
      
      out_report <- switch(
        input$format, 
        "By team" = "word_report.Rmd",
        "By floor" = "word_report_floors.Rmd", 
        "By floor and team" = "word_report_floors_teams.Rmd",
        "By building" = "word_report_building.Rmd"
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
                                               df_sum = RV$filtered,
                                               survey_name = RV$survey_name))
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

