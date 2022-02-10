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
library(plotly)
library(gt)


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

#Import raw energy spreadsheet
data_sheet_energy_raw <- read.xlsx2(here('data',"DataCenterEnergyUse-RawCollection.xlsx"), 1, #the "1" specifies to import sheet 1
                                
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
data_sheet_energy_raw <- data_sheet_energy_raw %>% 
  row_to_names(1) %>% 
  clean_names()

#change column names that were set to incorrect values when column classes were set
colnames(data_sheet_energy_raw) [2] <- 'report_year'
colnames(data_sheet_energy_raw) [3] <- 'period_covered_start_date'
colnames(data_sheet_energy_raw) [4] <- 'period_covered_end_date'
colnames(data_sheet_energy_raw) [5] <- 'energy_reporting_scope'
colnames(data_sheet_energy_raw) [6] <- 'level_of_ownership'
colnames(data_sheet_energy_raw) [9] <- 'electricity_value'
colnames(data_sheet_energy_raw) [10] <- 'unit_scale'
colnames(data_sheet_energy_raw) [15] <- 'fuel_1_value'
colnames(data_sheet_energy_raw) [16] <- 'fuel_1_unit_scale'
colnames(data_sheet_energy_raw) [20] <- 'fuel_2_value'
colnames(data_sheet_energy_raw) [21] <- 'fuel_2_unit_scale'
colnames(data_sheet_energy_raw) [25] <- 'fuel_3_value'
colnames(data_sheet_energy_raw) [26] <- 'fuel_3_unit_scale'
colnames(data_sheet_energy_raw) [30] <- 'fuel_4_value'
colnames(data_sheet_energy_raw) [31] <- 'fuel_4_unit_scale'
colnames(data_sheet_energy_raw) [35] <- 'fuel_5_value'
colnames(data_sheet_energy_raw) [36] <- 'fuel_5_unit_scale'

#Import pre-tranformed energy spreadsheet
data_sheet_energy_transformed <- read.csv(here('data', 'data_sheet_energy_transformed.csv'))

#Isolate fuel values from transformed dataset and then stack - for fuel data graph

#isolate electricity values
data_sheet_energy_electricity <- data_sheet_energy_transformed %>% 
  select("company", "data_year", "electricity_converted") %>% 
  add_column(fuel_type = "Electricity") %>% 
  relocate("fuel_type", .after = data_year) %>% 
  rename(value = electricity_converted)

#isolate fuel 1 values
data_sheet_energy_combined_1 <- data_sheet_energy_transformed %>% 
  select("company", "data_year", "fuel_1_type", "fuel_1_converted") %>% 
  rename(fuel_type = fuel_1_type) %>% 
  rename(value = fuel_1_converted)

#isolate fuel 2 values
data_sheet_energy_combined_2 <- data_sheet_energy_transformed %>% 
  select("company", "data_year", "fuel_2_type", "fuel_2_converted") %>% 
  rename(fuel_type = fuel_2_type) %>% 
  rename(value = fuel_2_converted)

#isolate fuel 3 values
data_sheet_energy_combined_3 <- data_sheet_energy_transformed %>% 
  select("company", "data_year", "fuel_3_type", "fuel_3_converted") %>% 
  rename(fuel_type = fuel_3_type) %>% 
  rename(value = fuel_3_converted)

#isolate fuel 4 values
data_sheet_energy_combined_4 <- data_sheet_energy_transformed %>% 
  select("company", "data_year", "fuel_4_type", "fuel_4_converted") %>% 
  rename(fuel_type = fuel_4_type) %>% 
  rename(value = fuel_4_converted)

#isolate fuel 5 values
data_sheet_energy_combined_5 <- data_sheet_energy_transformed %>% 
  select("company", "data_year", "fuel_5_type", "fuel_5_converted") %>% 
  rename(fuel_type = fuel_5_type) %>% 
  rename(value = fuel_5_converted)

by_fuel_type_data <- rbind(data_sheet_energy_electricity, data_sheet_energy_combined_1, #stack converted electricity, fuel values sheets on top of each other
        data_sheet_energy_combined_2, data_sheet_energy_combined_3,
        data_sheet_energy_combined_4, data_sheet_energy_combined_5) %>%
  drop_na()

#Import raw company profile sheet
data_sheet_company_raw <- read.xlsx2(here('data',"DataCenterEnergyUse-RawCollection.xlsx"), 2, #the "2" specifies to import sheet 2
                                 
                                 #specify column data types to ensure proper recognition
                                 colClasses=c("character","character","character","integer","Date", #columns 1-5
                                              "character","character","character","character","character", #columns 6-10
                                              "character","character","character","character","character", #columns 11-15
                                              "character","character","character","character","character", #columns 16-20
                                              "character","character","character","character","character", #columns 21-25
                                              "character","character","character","character","character", #columns 26-30
                                              "character","character","character","character","character", #columns 31-35
                                              "character","character","character","character")) #column 36-39

#move second row values to column headers, put header names in tidy format
data_sheet_company_raw <- data_sheet_company_raw %>% 
  row_to_names(2) %>% 
  clean_names() %>%
  select(!c(who_added_the_company, x, qts, x2019, x_2, x_3, x_4))

#change column names that were set to incorrect values when column classes were set
colnames(data_sheet_company_raw) [3] <- 'company_founding_year'
colnames(data_sheet_company_raw) [4] <- 'date_last_updated'

#Import raw PUE sheet
data_sheet_pue_raw <- read.xlsx2(here('data',"DataCenterEnergyUse-RawCollection.xlsx"), 3, #the "3" specifies to import sheet 3
                             
                             #specify column data types to ensure proper recognition
                             colClasses=c("character","integer","character","character","character", #columns 1-5
                                          "numeric","character","character","character","character", #columns 6-10
                                          "character","character","character","character"))          #columns 11-14

#move second row values to column headers, put header names in tidy format
data_sheet_pue_raw <- data_sheet_pue_raw %>% 
  row_to_names(1) %>% 
  clean_names()

colnames(data_sheet_pue_raw) [2] <- 'applicable_year'
colnames(data_sheet_pue_raw) [6] <- 'pue_value'

#generate unique list of companies in alphabetical order and drop blank
unique_companies <- list()
companies <- str_subset(sort(unique(data_sheet_company_raw$company_name)),"")

for (i in 1:length(companies)) {
  
  unique_companies[[i]]<- list(key = {companies[i]}, 
                               text = {companies[i]})
}

##################################################
########### Tab 4 - Methods #############
##################################################

#Tab 5 -

#Tab 6 - Methodology