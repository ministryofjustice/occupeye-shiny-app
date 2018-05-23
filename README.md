# Occupeye

## Introduction

This is a prototype Shiny app for extracting and analysing sensor data from OccupEye desk sensors.

## Code structure

There are currently 4 main sections of code:

* `data_retrieval_functions.R`: Functions for loading data from OccupEye into R
* `data_cleaning_functions.R`: functions for cleaning the data into format useful for charting
* `charting_functions.R`: produces the charts and tables used in the reports
* `shiny_test.R`: contains the UI and server functions for creating the shiny application

In addition, there are two rmd files that the shiny app calls when generating reports:

* `slidy_report.Rmd`: an HTML slideshow
* `word_report.Rmd`: a docx report

### data_retrieval_functions.R and the data store

As a bit of background: the OccupEye data is extracted from the OccupEye via a data scraper: https://github.com/moj-analytical-services/occupye_data_scraper
That tool pulls the data into S3, and configures Amazon Athena so that the data can be accessed via Athena SQL, as though it were a database.

The data retrieval functions file contains functions for connecting to Athena and extracting data.

The primary dataset to extract is the sensor data itself. The sensors record a 1 or 0 for every 10 minute window, depending on whether or not the desk (or room) is occupied. The raw data is extracted in Tidy format - one row each per observation.

The first function, `get_sensor_data_sql`, generates SQL for a query to get the observation, time and device ID for a filtered cut of the the data.

The second function, `get_sensor_df`, retrieves the output of the SQL query, then joins the output to a table of sensors to add extra metadata fields associated with the sensor - team, floor, desk type and so on. This two-step process is done to minimise the amount of data needed to be downloaded from S3.

A number of other methods extract data useful for populating the controls in the Shiny app.

### data_cleaning_functions.R

This file contains the methods for producing a cleaned, summarised dataset, referred to as `df_sum`, from the raw data. The main utility is in determining the daily utilisation of each sensor, and categorising it into the categories "used", "partially used" and "unused".

A sensor is defined as being unused if it is occupied for less than 15% of the day, AND has fewer than 2 periods of 30 minutes of sustained use.
It is under-utilised if it has less than 50% utilisation.

Other cleaning functions include filtering for work desk types only and filtering out non-business days.

### charting_functions.R

This file contains standard charts and tables for output, generated from manipulations of the standard `df_sum` dataset. These are replications of the charts used in the report.

### shiny_test.R

This is where the Shiny app code lives, though it will need to be reworked/split up in order to do make it deployable.

At the top are the library declarations, source() functions for pulling in the other source files, and then some placeholder functions for initialising some of the inputs. After that comes the functions defining the UI and Server parts of the app.

### slidy_report and word_report

There is facility to download reports akin to the old reports, either in a word document or in an HTML slideshow using Slidy. As the latter requires slightly different markup (with dashes separating different slides) they are separate files.

## Unit tests

To run the unit tests, issue the following command in the console:

`testthat::test_dir("tests")`