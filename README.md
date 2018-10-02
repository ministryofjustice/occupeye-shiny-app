# Occupeye

## Introduction

This is a Shiny app for extracting and analysing sensor data from OccupEye desk sensors.

## Code structure

There are currently 3 main sections of code:

* `data_cleaning_functions.R`: functions for cleaning the data into format useful for charting
* `charting_functions.R`: produces the charts and tables used in the reports
* `app.R`: contains the UI and server functions for creating the shiny application

In addition, there are two rmd files that the shiny app calls when generating reports:

* `slidy_report.Rmd`: an HTML slideshow
* `word_report.Rmd`: a docx report

### data retrieval and the data store

As a bit of background: the OccupEye data is extracted from the OccupEye via a data scraper: https://github.com/moj-analytical-services/airflow_occupeye_scraper
That tool pulls the data into S3, and configures Amazon Athena so that the data can be accessed via Athena SQL, as though it were a database.

Unfortunately Athena isn't fast enough to be used directly by the app. Therefore a secondary scraper creates aggregate datasets for use in the app: https://github.com/moj-analytical-services/airflow-occupeye-dashboard-aggregation


### data_cleaning_functions.R

This file contains the methods for producing a cleaned, summarised dataset, referred to as `df_sum`, from the raw data. The main utility is in determining the daily utilisation of each sensor, and categorising it into the categories "used", "partially used" and "unused".

A sensor is defined as being unused if it is occupied for less than 15% of the day.
It is under-utilised if it has less than 50% utilisation.

Other cleaning functions include filtering out non-business days.

### charting_functions.R

This file contains standard charts and tables for output, generated from manipulations of the standard `df_sum` dataset. These are replications of the charts used in the old Powerpoint report.

### app.R

This is where the Shiny app code lives, though it will need to be reworked/split up in order to do make it deployable.

At the top are the library declarations, source() functions for pulling in the other source files, and then some placeholder functions for initialising some of the inputs. After that comes the functions defining the UI and Server parts of the app.

### slidy_report and word_report

There is facility to download reports akin to the old reports, either in a word document or in an HTML slideshow using Slidy. As the latter requires slightly different markup (with dashes separating different slides) they are separate files.

## Unit tests

To run the unit tests, issue the following command in the console:

`testthat::test_dir("tests")`
