# s3tools::get_credentials()

library(glue)
library(odbc)
# con <- dbConnect(odbc::odbc(), "Athena")

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


get_sensor_df <- function(survey_id, start_date, end_date_exclusive, category_1=NULL, category_2=NULL, category_3=NULL) {
  
  sql <- get_sensor_data_sql(survey_id, start_date, end_date_exclusive, category_1, category_2, category_3)
  
  obs <- dbGetQuery(con, sql)
  
  sensors_sql <- glue("select * from occupeye_db.sensors where survey_id={survey_id}")
  sensors <- dbGetQuery(con, sensors_sql)
  
  dplyr::left_join(obs, sensors, by="surveydeviceid")
  
}


