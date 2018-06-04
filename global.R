
# Library declarations ----------------------------------------------------

library(shiny)          # For the shiny!
library(ggplot2)        # For plotting
library(dplyr)          # For pipes
library(htmlwidgets)    # rpivottool depends on it
library(shinyWidgets)   # For pickerInput
library(plotly)         # Makes ggplot interactive
library(shinyTree)      # for the category tree
library(rpivotTable)    # Pivot tables

# import other source code ------------------------------------------------


source("charting_functions.R")
source("data_cleaning_functions.R")
source("data_retrieval_functions.R")


# Temporary initialise functions ------------------------------------------
# Downloads a sample dataset from S3, and uses it to initialise the UI fields.
# These will need to be removed/revised once the Athena connection is fixed.

df <- s3tools::read_using(FUN=readr::read_csv, s3_path="alpha-fact/OccupEye/occupeye_automation/sensor_df_20180412_full.csv")
df_sum <- get_df_sum(df,"09:00","17:00")
time_list <- unique(strftime(df$obs_datetime,format="%H:%M"))
date_list <- unique(date(df$obs_datetime))
surveys_list <- get_surveys_list()
device_types <- unique(df$devicetype)
floors <- unique(df$floor)
