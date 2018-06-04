
# UI function -------------------------------------------------------------
# Constructs the UI

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Report config",
                 
                 helpText("Hit the go button below to update the filter"),
                 
                 
                 actionButton("goButton","Go!"),
                 
                 
                 selectInput(inputId = "survey_name",
                             label = "Select OccupEye survey",
                             choices = surveys_list$name,
                             selected = "102 Petty France v1.0"),
                 
                 
                 dateRangeInput(inputId = "date_range",
                                label = "Select sample date range",
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
                 tableOutput(outputId = "team_count"),
                 tableOutput(outputId = "desk_count"),
                 tableOutput(outputId = "team_desk_count")),
        tabPanel("Smoothing",plotlyOutput(outputId = "smoothChart")),
        tabPanel("daily usage",plotlyOutput(outputId = "dailyChart")),
        tabPanel("usage by weekday",plotlyOutput(outputId = "weekdayChart")),
        tabPanel("usage by desk type",plotlyOutput(outputId = "deskChart")),
        tabPanel("usage by floor",plotlyOutput(outputId = "floorChart")),
        tabPanel("df_sum",dataTableOutput(outputId = "df_sum"))
      )
      
    )
  )
)
)

