library(here)
library(janitor)
library(tidyverse)
library(lubridate)
library(shiny)
library(shiny.router)
library(shiny.fluent)
library(shiny.router)
library(xlsx)
library(readxl)
library(DT)


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

##################################################
########### Tab 3 - Company Profiles #############
##################################################

#Import energy spreadsheet
data_sheet_energy <- read.xlsx2(here('data',"DataCenterEnergyUse-RawCollection.xlsx"), 1, #the "1" specifies to import sheet 1
                                
                                #specify column data types to ensure proper recognition
                                colClasses=c("character","integer","Date","Date","character", #columns 1-5
                                             "character", "character","character","numeric","integer", #columns 6-10
                                             "character", "character","character","character","numeric", #columns 11-15 (Fuel 1)
                                             "integer", "character","character","character","numeric", #columns 15-20 (Fuel 2)
                                             "integer", "character","character","character","numeric", #columns 21-25 (Fuel 3)
                                             "integer", "character","character","character","numeric", #columns 26-30 (Fuel 4)
                                             "integer", "character","character","character","numeric", #columns 31-35 (Fuel 5)
                                             "integer", "character")) #column 36, 37 (Notes)

#move second row values to column headers, put header names in tidy format
data_sheet_energy <- data_sheet_energy %>% 
  row_to_names(1) %>% 
  clean_names()

#change column names that were set to incorrect values when column classes were set
colnames(data_sheet_energy) [2] <- 'report_year'
colnames(data_sheet_energy) [3] <- 'period_covered_start_date'
colnames(data_sheet_energy) [4] <- 'period_covered_end_date'
colnames(data_sheet_energy) [5] <- 'energy_reporting_scope'
colnames(data_sheet_energy) [6] <- 'level_of_ownership'
colnames(data_sheet_energy) [9] <- 'electricity_value'
colnames(data_sheet_energy) [10] <- 'unit_scale'
colnames(data_sheet_energy) [15] <- 'fuel_1_value'
colnames(data_sheet_energy) [16] <- 'fuel_1_unit_scale'
colnames(data_sheet_energy) [20] <- 'fuel_2_value'
colnames(data_sheet_energy) [21] <- 'fuel_2_unit_scale'
colnames(data_sheet_energy) [25] <- 'fuel_3_value'
colnames(data_sheet_energy) [26] <- 'fuel_3_unit_scale'
colnames(data_sheet_energy) [30] <- 'fuel_4_value'
colnames(data_sheet_energy) [31] <- 'fuel_4_unit_scale'
colnames(data_sheet_energy) [35] <- 'fuel_5_value'
colnames(data_sheet_energy) [36] <- 'fuel_5_unit_scale'

by_fuel_type_data <- read.csv(here('data', 'by_fuel_type_data.csv'))
data_sheet_company <- read.csv(here('data', 'data_sheet_company.csv'))

#generate unique list of companies in alphabetical order and drop blank
unique_companies <- list()
companies <- str_subset(sort(unique(data_sheet_company$company_name)),"")

for (i in 1:length(companies)) {
  
  unique_companies[[i]]<- list(key = {companies[i]}, 
                               text = {companies[i]})
}

#Tab 4 -

#Tab 5 -

#Tab 6 - Methodology