library(dplyr)
library(lubridate)
library(snakecase)

dt_to_numeric <- function(dt) {
  3600 * hour(dt) + 60 * minute(dt) + second(dt)
}

hours_minutes_string_to_numeric <- function(hm_string) {
  # 2010-01-01 is an arbitary date because we're just interested in the time
  dt <- as.POSIXct(paste("2010-01-01", hm_string), tz = "UTC")
  dt_to_numeric(dt)
}

in_time_range <- function(datetime_column, start_time, end_time) {
  
  between(dt_to_numeric(datetime_column),
          hours_minutes_string_to_numeric(start_time),
          hours_minutes_string_to_numeric(end_time) - 600) # 
  
}

filter_time_range <- function(df, start_time, end_time) {
  df %>%
    dplyr::filter(in_time_range(obs_datetime, start_time, end_time))
}

fix_bad_sensor_observations <- function(df) {
  # Set any observation which is not 1 or 0 to null
  bad_rows <- !(df$sensor_value %in% c(1, 0))
  df[bad_rows, "sensor_value"] <- NA
  df
}

add_is_used <- function(df, perc_util=0.15) {
  
  f1 <- df$utilisation > perc_util
  df %>%
    mutate(in_use = f1)
}

add_util_category <- function(df) {
  
  level_order <- c("Unused", "Under utilised", "Effective utilisation")
  df %>%
    mutate(util_cat = case_when(in_use == FALSE  ~ "Unused",
                                in_use == TRUE & utilisation < 0.5  ~ "Under utilised",
                                in_use == TRUE & utilisation >= 0.5  ~ "Effective utilisation"
    ))  %>% # Convert in_use to a factor with levels in a specific order - this controls the order in which it appears in ggplot 
    mutate(util_cat = factor(util_cat, levels = level_order))
  
}

remove_non_business_days <- function(df) {
  bank_holidays <- jsonlite::fromJSON("https://www.gov.uk/bank-holidays.json")
  bank_holidays <- bank_holidays$`england-and-wales`$events
  
  df %>%
    dplyr::filter(!(date(obs_datetime) %in% as.Date(bank_holidays$date))) %>% 
    dplyr::filter(!(weekdays(date(obs_datetime)) %in% c("Saturday", "Sunday")))
}


clean_and_mutate_raw_data <- function(df) {
  df %>%
    fix_bad_sensor_observations()
}

get_summarised_data <- function(df) {
  
  df %>%
    dplyr::filter(!is.na(sensor_value)) %>%
    group_by(date = date(obs_datetime),
             survey_device_id) %>%
    summarise(utilisation = mean(sensor_value)) %>%  
    ungroup(date,
            survey_device_id) %>%
    add_is_used() %>%
    add_util_category()
  
}

get_df_sum <- function(df,
                       sensors,
                       start_time = "09:00",
                       end_time = "17:00") {
  
  
  df2 <- df %>% 
    filter_time_range(start_time, end_time) %>%         
    clean_and_mutate_raw_data() %>% 
    remove_non_business_days()
  
  
  get_summarised_data(df2) %>%
    get_full_df(sensors)
}

get_full_df <- function(df, sensors) {
  left_join(df, sensors, by = c("survey_device_id"))
}

get_bad_observations <- function(df) {
  df %>%
    dplyr::filter(!sensor_value %in% c(1, 0)) %>%
    mutate(obs_date = date(obs_datetime)) %>%
    group_by(obs_date,
             sensor_value,
             survey_device_id,
             hardwareid,
             sensorid,
             location) %>% 
    summarise(count = n())
  
}



get_df_sql <- function(survey_ids,
                       category_1=NULL,
                       category_2=NULL,
                       category_3=NULL,
                       floor=NULL,
                       start_date = "2018-07-01",
                       end_date = Sys.Date(),
                       start_time = "09:00",
                       end_time = "17:00") {
  
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
  if (notnull(floor)) {
    category_filter = glue("{category_filter} and floor = '{floor}'")
  }
  
  sql <- "
    select so.sensor_value, so.obs_datetime, so.survey_device_id
    from occupeye_app_db.sensor_observations as so
    left join occupeye_app_db.sensors as se
    on so.survey_device_id = se.surveydeviceid
    where so.survey_id in ({paste0(survey_ids, collapse = ',')})
    {category_filter}
    and so.obs_datetime >= timestamp '{start_date} 00:00'
    and so.obs_datetime <= timestamp '{end_date} 23:50'
    and CAST(so.obs_datetime AS TIME) >= time '{start_time}' 
    and CAST(so.obs_datetime AS TIME) < time '{end_time}'
    
    "
  
  glue(sql)
  
}

make_numeric <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", x))
}

convert_fields_to_sentence_case <- function(df) {
  names(df) <- snakecase::to_sentence_case(names(df))
  df
}
