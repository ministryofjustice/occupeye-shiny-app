source("data_retrieval_functions.R")
library(dplyr)


#Report parameters


start_date <- "2017-10-30"
end_date_exclusive <- "2017-12-02"
start_time <- "09:00"
end_time <- "17:00"
department <- "Analytical services"

start.time <- Sys.time()

#df <-  get_sensor_df(330, start_date, end_date_exclusive,category_2 = department)

end.time <- Sys.time()

elapsed <- end.time - start.time

elapsed

my_data <- s3tools::read_using(FUN=readr::read_csv, s3_path="alpha-fact/OccupEye/occupeye_automation/sensor_df_20180412_full.csv")



df <- my_data %>%
      filter(as.Date(obs_datetime)>= start_date,
             as.Date(obs_datetime) < end_date_exclusive,
             category_3 == "Analytical Services")