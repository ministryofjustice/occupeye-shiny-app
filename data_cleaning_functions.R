library(dplyr)
library(lubridate)

dt_to_numeric <- function(dt) {
  3600 * hour(dt) + 60 * minute(dt) + second(dt)
}

hours_minutes_string_to_numeric = function(hm_string) {
  # 2010-01-01 is an arbitary date because we're just interested in the time
  dt = as.POSIXct(paste("2010-01-01", hm_string), tz = "UTC")
  dt_to_numeric(dt)
}

in_time_range <- function(datetime_column, start_time, end_time) {

  between(dt_to_numeric(datetime_column),
          hours_minutes_string_to_numeric(start_time),
          hours_minutes_string_to_numeric(end_time))

}

filter_time_range <- function(df, start_time, end_time) {
  df %>%
    filter(in_time_range(obs_datetime,start_time, end_time))
}

fix_bad_sensor_observations <- function(df) {
  # Set any observation which is not 1 or 0 to null
  bad_rows <- !(df$sensor_value %in% c(1,0))
  df[bad_rows, "obs_datetime"] <- NA
  df
}

get_run_groups <- function(x) {
  runs <- rle(x)
  num_runs <- length(runs$values)
  rep(1:num_runs, runs$lengths)
}

add_sensor_acc <- function(df) {

   df %>%
      mutate(temp_values__ = ifelse(sensor_value == 0,-1,sensor_value)) %>%
      group_by(date__ = date(obs_datetime), run__ = get_run_groups(temp_values__)) %>%
      mutate(sensor_acc = cumsum(temp_values__)) %>%
      select(-temp_values__) %>%
      ungroup(date__, run__) %>%
      select(-run__, -date__)
}

add_is_workdesk_column <- function(df) {

  sensor_types <- c("longstay desk","jigsaw desk","touchdown booth","touchdown desk")
  df %>%
    mutate(is_workdesk = tolower(devicetype) %in% sensor_types)

}


add_is_used <- function(df, perc_util=0.15, count_used_30 = 2) {

  f1 <- df$utilisation > perc_util
  f2 <- df$count_used_30 >= count_used_30
  df %>%
    mutate(in_use = f1 | f2)

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
    filter(!(date(obs_datetime) %in% as.Date(bank_holidays$date))) %>% 
    filter(!(weekdays(date(obs_datetime)) %in% c('Saturday','Sunday')))
}


clean_and_mutate_raw_data <- function(df) {
  df %>%
    fix_bad_sensor_observations() %>%
    #add_is_workdesk_column() %>% 
    add_sensor_acc()
}

get_summarised_data <- function(df) {
  
  df %>%
    group_by(date = date(obs_datetime), surveydeviceid, devicetype, category_1, category_2,category_3,floor) %>%
    summarise(utilisation = mean(sensor_value, rm.na=TRUE),
              longest_in_use = max(sensor_acc,0)*10,
              longest_empty = min(sensor_acc,0)*-10,
              count_used_30 = sum(sensor_acc == 3), # The sum of the number of occasions sensor_acc contains the value 3
              count_na = sum(is.na(sensor_value))) %>%  
    ungroup(date, surveydeviceid, devicetype, category_1, category_2,category_3,floor) %>%
    add_is_used() %>%
    add_util_category

}

get_df_sum <- function(df,start_time,end_time) {
  
  
  df2 <- df %>% 
    filter_time_range(start_time,end_time) %>%         
    clean_and_mutate_raw_data() %>% 
    remove_non_business_days()
  
  
  df_sum <- get_summarised_data(df2) 
}


