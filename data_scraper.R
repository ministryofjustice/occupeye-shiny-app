library(s3tools)
library(dplyr)
library(glue)
library(odbc)

source("data_retrieval_functions.R")

refresh_connection <- function(con) {
  dbDisconnect(con)
  rm(con)
  s3tools::get_credentials()
  con <- dbConnect(odbc::odbc(), "Athena")
}


scrape_team_csvs <- function(survey_id) {

  sensors <- get_sensors_list(survey_id) %>% filter(!grepl("\"",category_1)) # filter out sensors with quotes
  department_list <- unique(sensors$category_2)

  for(department in department_list) {
    
    
    start.time <- Sys.time()
    filtered <- sensors %>% filter(category_2 == department)
    cat1 <- filtered$category_1[1]
    department <- gsub("[[:punct:]]", " ",department)
    out_path <- paste0("alpha-fact/OccupEye/occupeye_automation/surveys/",survey_id,"/",cat1,"/",department,".csv")
    already_exists <- nrow(s3tools::list_files_in_buckets("alpha-fact") %>% filter(grepl(out_path,path))) >0
    
    if(!already_exists) {
      my_data <- get_sensor_df(survey_id,"2017-10-22",today(),category_2 = department)
      s3tools::write_df_to_csv_in_s3(my_data,out_path, row.names = FALSE)
      end.time <- Sys.time()
      
      print(paste0("uploaded ",department))
      print(end.time - start.time)
    } else {
      print(paste0(department, " already added"))
    }
  }
}


scrape_sensors <- function() {
  
  sensors <- get_all_sensors() %>% filter(!grepl("\"",category_1)) # filter out sensors with quotes
  s3tools::write_df_to_csv_in_s3(sensors,"alpha-fact/OccupEye/occupeye_automation/sensors.csv",overwrite = TRUE, row.names = FALSE)
  
}

scrape_surveys <- function() {
  surveys <- get_surveys_list()
  s3tools::write_df_to_csv_in_s3(surveys,"alpha-fact/OccupEye/occupeye_automation/surveys.csv",overwrite = TRUE,row.names = FALSE)
  
}