library(here)
library(janitor)
library(tidyverse)
library(lubridate)
library(shiny)
library(shiny.router)
library(shiny.fluent)
library(xlsx)
library(readxl)
library(DT)
library(plotly)
library(gt)
library(scales)
library(tidyselect)
library(waiter)
library(fastDummies)
library(shinyjs)
library(data.table)

source(here("R", "transformEnergyDataRaw.R"))

##################################################
#### NOTE: Need to transfer over code from data_preprocessing.Rmd to 
#### data_sheet_energy_transformed <- read.csv(here('data', 'data_sheet_energy_transformed.csv'))
#### to phase out using this csv as an intermediary
##################################################

##################################################
### Import All Raw Sheets and Pre-process Data ###
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

#Transform raw spreadsheet
data_sheet_energy_transformed <- transformEnergyDataRaw(data_sheet_energy_raw) #read.csv(here('data', 'data_sheet_energy_transformed.csv'))

##################################################
################ Tab 1 - Home ####################
##################################################

##################################################
######## Tab 2 - Data Center Energy 101 ##########
##################################################

##################################################
########### Tab 3 - Industry Trends ##############
##################################################

#Generate list of scope options for UI dropdown
unique_scope_selection <- list()
industry_trends_scopes <- c('Data Centers', 'Company Wide')

for (i in 1:length(industry_trends_scopes)) {
  
  unique_scope_selection[[i]]<- list(key = {industry_trends_scopes[i]}, 
                               text = {industry_trends_scopes[i]})
}

#Generate list of years for UI dropdown
unique_years <- list()
industry_trends_years <- str_subset(sort(unique(data_sheet_energy_transformed$data_year), decreasing = TRUE),"")

for (i in 1:length(industry_trends_years)) {
  
  unique_years[[i]]<- list(key = {industry_trends_years[i]}, 
                               text = {industry_trends_years[i]})
}

##################################################
########### Tab 4 - Company Profiles #############
##################################################

no_data <- data.frame(no_data_reported = "No data reported")
`%not_in%` <- purrr::negate(`%in%`)

#generate unique list of companies in alphabetical order and drop blank
unique_companies <- list()
companies <- str_subset(sort(unique(data_sheet_energy_raw$company)),"")

for (i in 1:length(companies)) {
  
  unique_companies[[i]]<- list(key = {companies[i]}, 
                               text = {companies[i]})
}

#########################################
########### Tab 5 - Methods #############
#########################################

#Tab 6 -

#Tab 7 - 