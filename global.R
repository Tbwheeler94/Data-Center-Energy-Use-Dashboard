library(here)
library(janitor)
library(tidyverse)
library(lubridate)
library(shiny)
library(shiny.router)
library(shiny.fluent)

#############################
#######TEMPORARY SETUP#######
#############################
# When data is hosted remotely this page will consist of the following
# (1) An API call to the AWS server to collect the latest version of the raw collection spreadsheet
# (2) All of the code in the data_preprocessing.Rmd file to prefilter the data
# (3) A list of objects with pre-sorted datasets that will feed into visualizations (which currently point to csvs)

#Tab 1 - Home

#Tab 2 - Aggregate Statistics
aggregate_data <- read.csv(here('data', 'aggregate_data.csv'))

#Tab 3 - Company Profiles
by_fuel_type_data <- read.csv(here('data', 'by_fuel_type_data.csv'))

#Tab 4 -

#Tab 5 -

#Tab 6 - Methodology