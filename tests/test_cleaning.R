library(testthat)
setwd("..") 

source("data_cleaning_functions.R")

testthat::context("Test data cleaning functions")

testthat::test_that("Test remove_non_business_days works", {
  
  # Basic weekend filter
  df <- tibble::data_frame(obs_datetime = as.Date(c("2018-04-08","2018-04-09")))
  
  df <- df %>% 
    remove_non_business_days()
  
  testthat::expect_true(as.Date("2018-04-09") %in% df)
  testthat::expect_false(as.Date("2018-04-08") %in% df)
  
  
  # Easter 2018 check
  easter <- tibble::data_frame(obs_datetime = seq(as.Date("2018-03-29"), by = "day", length.out = 6))
  
  easter <- easter %>% 
    remove_non_business_days()
  
  testthat::expect_true(nrow(easter)==2)

  # Check bank holidays are removed
  bank_holidays <- jsonlite::fromJSON("https://www.gov.uk/bank-holidays.json")
  bank_holidays <- bank_holidays$`england-and-wales`$events
  
  bh <- bank_holidays %>%
    mutate( obs_datetime = as.Date(date)) %>% 
    remove_non_business_days()
  
  testthat::expect_true(nrow(bh)==0)
  
})