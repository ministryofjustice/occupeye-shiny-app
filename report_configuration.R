#Report parameters

start_date <- "2018-03-01"
end_date_exclusive <- "2018-04-01"
start_time <- "09:00"
end_time <- "17:00"
department <- "Analytical services"

# The following line will be used as soon as we get Athena access
# df <-  get_sensor_df(330, "2018-03-01", "2018-04-01", category_2='Analytical services')

df <- s3tools::read_using(FUN=readr::read_csv, path="alpha-fact/OccupEye/occupeye_automation/sensor_df_20180408.csv")