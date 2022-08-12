##############################################################
##############################################################
###### Section 1: Pre-load ###################################
##############################################################
##############################################################

########################################################
###### Load packages ###################################
########################################################

library(shiny)

#basic data wrangling dependencies
library(here)
library(janitor)
library(tidyverse)
library(lubridate)
library(fastDummies)
library(glue)

#used for importing data spreadsheet
library(xlsx)
library(readxl)

#used to develop rapidly deployable and stylable datatables
library(DT)
library(data.table)

#plotting and visualization
library(plotly)
library(visNetwork) 
library(scales)
library(ggiraph)

#initiating core shiny, multiple web-pages and fluent ui styling, respectively
library(shiny.router)
library(shiny.fluent)
library(rintrojs)

#website interactivity
library(tidyselect)
library(shinyjs) #enables use of javascript functions
library(shinymanager) #generates a user authentication page
library(waiter) #package for generating loading animations

#packages used to create connection between google sheet storing contact form responses and app
library(googlesheets4) 
library(googledrive)

#used to developed accordion on methods page
#library(rsconnect)

########################################################
###### Suppress warnings ###############################
########################################################

#suppress groupby warning
options(dplyr.summarise.inform = FALSE)

###################################################################################################################################
###### Reference function which prepares takes raw dataset and transforms data to be visualization-ready ##########################
###################################################################################################################################

source(here("R", "transformEnergyDataRaw.R"))

########################################################
###### Authorize user login (authentication) ###########
########################################################

#inactivity <- "function idleTimer() {
#    var t = setTimeout(logout, 120000);
#    window.onmousemove = resetTimer; // catches mouse movements
#    window.onmousedown = resetTimer; // catches mouse movements
#    window.onclick = resetTimer;     // catches mouse clicks
#    window.onscroll = resetTimer;    // catches scrolling
#    window.onkeypress = resetTimer;  //catches keyboard actions
#    
#    function logout() {
#    window.close();  //close the window
#    }
#    
#    function resetTimer() {
#    clearTimeout(t);
#    t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
#    }
#    }
#    idleTimer();"
#
#
## data.frame with credentials info
#credentials <- data.frame(
#  user = c("ucsb"),
#  password = c("isal"))

##############################################################
##############################################################
###### Section 2: Import data ################################
##############################################################
##############################################################

######################################################################################################################################################
### Import Sheet 1 from data.xlsx which contains energy data and transform data via transformEnergyDataRaw() function ###
######################################################################################################################################################

#Import raw energy spreadsheet
data_sheet_energy_raw <- read.xlsx2(here('source_data',"data.xlsx"), 1, #the "1" specifies to import sheet 1
                                
                                #specify column data types to ensure proper recognition
                                colClasses=c("character","integer","Date","Date","character", #columns 1-5
                                             "character", "character","character", "character", "character", #columns 6-10
                                             "numeric","integer", "character", "character", "character", #columns 11-15
                                             "character","character", "character", "numeric", "integer", #columns 16-20
                                             "character","character","character","numeric", "integer", #columns 21-25
                                             "character","character","character","numeric", "integer", #columns 26-30
                                             "character","character","character","numeric", "integer", #columns 31-35
                                             "character","character","character","numeric","integer", #columns 36-40
                                             "character", "character", "character")) #column 41,42,43

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
colnames(data_sheet_energy_raw) [11] <- 'electricity_value'
colnames(data_sheet_energy_raw) [12] <- 'unit_scale'
colnames(data_sheet_energy_raw) [19] <- 'fuel_1_value'
colnames(data_sheet_energy_raw) [20] <- 'fuel_1_unit_scale'
colnames(data_sheet_energy_raw) [24] <- 'fuel_2_value'
colnames(data_sheet_energy_raw) [25] <- 'fuel_2_unit_scale'
colnames(data_sheet_energy_raw) [29] <- 'fuel_3_value'
colnames(data_sheet_energy_raw) [30] <- 'fuel_3_unit_scale'
colnames(data_sheet_energy_raw) [34] <- 'fuel_4_value'
colnames(data_sheet_energy_raw) [35] <- 'fuel_4_unit_scale'
colnames(data_sheet_energy_raw) [39] <- 'fuel_5_value'
colnames(data_sheet_energy_raw) [40] <- 'fuel_5_unit_scale'
  
######################################################################################################
### Import Sheet 2 from data.xlsx which contains company profile data ###
######################################################################################################

#Import raw company profile sheet
data_sheet_company_raw <- read.xlsx2(here('source_data',"data.xlsx"), 2, #the "2" specifies to import sheet 2
                                     
                                     #specify column data types to ensure proper recognition
                                     colClasses=c("character","character","character","integer","Date", #columns 1-5
                                                  "character","character","character","character","character", #columns 6-10
                                                  "character","character","character","character","character", #columns 11-15
                                                  "character","character","character","character","character", #columns 16-20
                                                  "character","character","character","character","character", #columns 21-25
                                                  "character","character","character","character","character", #columns 26-30
                                                  "character","character","character","character","character", #columns 31-35
                                                  "character","character","character","character")) #column 36-39

#move second row values to column headers, put header names in tidy format and filter by only companies that have been checked back to 2007
data_sheet_company_raw <- data_sheet_company_raw %>% 
  row_to_names(2) %>% 
  clean_names() %>%
  select(!c(who_added_the_company, x)) %>% 
  filter(checked_back_to_founding_year_or_2007 == "Yes")

#change column names that were set to incorrect values when column classes were set
colnames(data_sheet_company_raw) [3] <- 'company_founding_year'
colnames(data_sheet_company_raw) [4] <- 'date_last_updated'

###########################################################################################
### Import Sheet 3 from data.xlsx which contains pue data ####
###########################################################################################

#Import raw PUE sheet
data_sheet_pue_raw <- read.xlsx2(here('source_data',"data.xlsx"), 3, #the "3" specifies to import sheet 3
                                 
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


#Transform raw energy data spreadsheet, filter to include only data newer than 2006 and include only companies that have been checked back to 2007
data_sheet_energy_transformed <- transformEnergyDataRaw(data_sheet_energy_raw) %>%
                                    #only include data years after 2006
                                    filter(data_year > 2006) %>% 
                                    #filter by the list of companies left after data_sheet_company_raw is filtered to include ony companies checked back to 2007
                                    filter(company %in% data_sheet_company_raw$company_name)

##################################################
################ Tab 1 - Home ####################
##################################################

# Creating big boxes for main tabs in the landing page (see ui for formatting css)
lp_main_box <- function(title_box, image_name, button_name, description) {
  div(class="landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      div(description, class = "landing-page-box-description"),
      div(class = "landing-page-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      actionButton(button_name, NULL, class="landing-page-button")
  )
}

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

#Generate list of unique companies from PUE sheet
unique_companies_pue <- list()
list_of_pue_companies <- str_subset(sort(unique(data_sheet_pue_raw$company), decreasing = FALSE),"")

for (i in 1:length(list_of_pue_companies)) {
  
  unique_companies_pue[[i]]<- list(key = {list_of_pue_companies[i]}, 
                                   text = {list_of_pue_companies[i]})
}

#Generate list of unique scopes from PUE sheet
unique_scopes_pue <- list()
list_of_pue_scopes <- c("Fleet Wide", "Individual Locations")

for (i in 1:length(list_of_pue_scopes)) {
  
  unique_scopes_pue[[i]]<- list(key = {list_of_pue_scopes[i]}, 
                                   text = {list_of_pue_scopes[i]})
}

#Generate list of unique scales for energy data trends plot
unique_scales <- list()
list_of_scales <- c("1 KWh - 100 MWh", "1 GWh - 10 GWh", "10 GWh - 100 GWh", "1 TWh - 10 TWh", "10+ TWh")

for (i in 1:length(list_of_scales)) {
  
  unique_scales[[i]]<- list(key = {list_of_scales[i]}, 
                                text = {list_of_scales[i]})
}

##################################################
########### Tab 4 - Company Profiles #############
##################################################

no_data <- data.frame(no_data_reported = "No data reported")
`%not_in%` <- purrr::negate(`%in%`)

#generate unique list of companies in alphabetical order and drop blank
unique_companies <- list()
companies <- str_subset(sort(unique(data_sheet_company_raw %>% pull(company_name))),"")

for (i in 1:length(companies)) {
  
  unique_companies[[i]]<- list(key = {companies[i]}, 
                               text = {companies[i]})
}

#########################################
########### Tab 5 - Methods #############
#########################################

##############################################
########### Tab 7 - Contact Page #############
##############################################

contact_referral_options <- list()
referal_option_list <- c("Website", "Newspaper", "Podcast", "Friend", "Other")

for (i in 1:length(referal_option_list)) {
  
  contact_referral_options[[i]]<- list(key = {referal_option_list[i]}, 
                               text = {referal_option_list[i]})
}