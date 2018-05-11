s3tools::get_credentials()

library(glue)
library(odbc)
con <- dbConnect(odbc::odbc(), "Athena")

get_sensor_data_sql <- function(survey_id, start_date, end_date_exclusive, category_1=NULL, category_2=NULL, category_3=NULL) {

  notnull <- function(x) ! is.null(x)
  
  category_filter = ""
  
  if (notnull(category_1)) {
    category_filter = glue("{category_filter} and category_1 = '{category_1}'")
  }
  if (notnull(category_2)) {
    category_filter = glue("{category_filter} and category_2 = '{category_2}'")
  }
  if (notnull(category_3)) {
    category_filter = glue("{category_filter} and category_3 = '{category_3}'")
  }
  
  sql <- "
  select so.sensor_value, so.obs_datetime, so.surveydeviceid 
  from occupeye_db.sensor_observations as so
  left join occupeye_db.sensors as se 
  on so.surveydeviceid = se.surveydeviceid 
  left join occupeye_db.surveys su
  on se.survey_id = su.survey_id
  where so.survey_id = {survey_id} 
  {category_filter}
  and so.obs_datetime >= timestamp '{start_date} 00:00'
  and so.obs_datetime < timestamp '{end_date_exclusive} 00:00'
  
  "
  
  glue(sql)

}
get_sensors_list <- function(survey_id) {
  
  sensors_sql <- glue("select * from occupeye_db.sensors where survey_id={survey_id}")
  sensors <- dbGetQuery(con, sensors_sql)
  
}

get_sensor_df <- function(survey_id, start_date, end_date_exclusive, category_1=NULL, category_2=NULL, category_3=NULL) {
  
  sql <- get_sensor_data_sql(survey_id, start_date, end_date_exclusive, category_1, category_2, category_3)
  
  obs <- dbGetQuery(con, sql)
  
  sensors <- get_sensors_list(survey_id)
  
  dplyr::left_join(obs, sensors, by="surveydeviceid")
  
}

get_surveys_list <- function() {
  
  
  surveys_sql <- "select * from occupeye_db.surveys"
  
  surveys <- dbGetQuery(con,surveys_sql)
}



get_survey_id <- function(surveys_list,survey_name) {

  filter(surveys_list,name==survey_name)$survey_id
  
}

get_cat_list <- function(sensors) {
  
  category_list <- sensors %>%
    select(category_1,category_2,category_3) %>%
    unique %>%
    mutate_all(funs(ifelse(.=="","N/A",.)))
  
  
}