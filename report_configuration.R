source("data_retrieval_functions.R")
library(dplyr)


#Report parameters


start_date <- "2017-10-30"
end_date_exclusive <- "2017-12-02"
start_time <- "09:00"
end_time <- "17:00"
department <- "Analytical services"

start.time <- Sys.time()

#df2 <-  get_sensor_df(330, start_date, end_date_exclusive,category_2 = department)

end.time <- Sys.time()

elapsed <- end.time - start.time

elapsed

my_data <- s3tools::read_using(FUN=readr::read_csv, s3_path="alpha-fact/OccupEye/occupeye_automation/sensor_df_20180412_full.csv")



df <- my_data %>%
      filter(as.Date(obs_datetime)>= start_date,
             as.Date(obs_datetime) < end_date_exclusive,
             sensor_value %in% c(1,0))




# department_list <- unique(sensors$category_2)
# 
# for(department in department_list) {
#   start.time <- Sys.time()
#   my_data <- get_sensor_df(330,"2017-10-22","2018-05-14",category_2 = department)
#   department <- gsub("[[:punct:]]", " ",department)
#   s3tools::write_df_to_csv_in_s3(my_data,paste0("alpha-fact/OccupEye/occupeye_automation/",department,".csv"))
#  
#   end.time <- Sys.time()
#   
#   
#   
#   print(paste0("uploaded ",department," ",end.time - start.time))
#    
# }
