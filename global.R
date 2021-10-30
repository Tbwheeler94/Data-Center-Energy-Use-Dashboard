library(here)
library(here)
library(janitor)
library(tidyverse)
library(lubridate)
library(shiny)

#change to API call to AWS hosting in future
aggregate_data <- read.csv(here('data', 'aggregate_data.csv'))
by_fuel_type_data <- read.csv(here('data', 'by_fuel_type_data.csv'))