source("data_retrieval_functions.R")
source("data_cleaning_functions.R")

# df <-  get_sensor_df(330, "2018-03-01", "2018-04-01", category_2='Analytical services')
df <- readr::read_csv("sensor_df_20180408.csv")

df2 <- df %>% 
  filter_time_range("09:00", "17:00") %>%         
  clean_and_mutate_raw_data() %>% 
  filter(is_workdesk == 1) %>% 
  remove_non_business_days()
  

df_sum <- get_summarised_data(df2) 

readr::write_csv(df_sum, "chart_specs/temp_delete_data/summary.csv")

system("vl2png chart_specs/underutilised.json -s4 rendered_charts/underutilised.png")



