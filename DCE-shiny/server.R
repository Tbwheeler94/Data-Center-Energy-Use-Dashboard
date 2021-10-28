## Load packages needed for data import
library(here)
library(janitor)
library(readxl)
library(xlsx)

## Source files
source(here("R", "companyOutput.R"))

#Server code
server <- function(input, output) {
    
    ####################
    #### DATA IMPORT ###
    ####################
    
    data_sheet <- read.xlsx2(here('data',"DataCenterEnergyUse-RawCollection.xlsx"), 1,
                             colClasses=c("character","integer","Date","Date","character", #columns 1-5
                                          "character","character","numeric","integer", "character", #columns 6-10
                                          "character","character","character","numeric","integer", #columns 11-15 (Fuel 1)
                                          "character","character","character","numeric","integer", #columns 15-20 (Fuel 2)
                                          "character","character","character","numeric","integer", #columns 21-25 (Fuel 3)
                                          "character","character","character","numeric","integer", #columns 26-30 (Fuel 4)
                                          "character","character","character","numeric","integer", #columns 31-35 (Fuel 5)
                                          "character")) #column 36 (Notes)
    
    #move second row values to column headers, put header names in tidy format
    data_sheet <- data_sheet %>% 
        row_to_names(1) %>% 
        clean_names()
    
    #change column names that were set to incorrect values when column classes were set
    colnames(data_sheet) [2] <- 'report_year'
    colnames(data_sheet) [3] <- 'period_covered_start_date'
    colnames(data_sheet) [4] <- 'period_covered_end_date'
    colnames(data_sheet) [5] <- 'energy_reporting_scope'
    colnames(data_sheet) [8] <- 'electricity_value'
    colnames(data_sheet) [9] <- 'unit_scale'
    colnames(data_sheet) [14] <- 'fuel_1_value'
    colnames(data_sheet) [15] <- 'fuel_1_unit_scale'
    colnames(data_sheet) [19] <- 'fuel_2_value'
    colnames(data_sheet) [20] <- 'fuel_2_unit_scale'
    colnames(data_sheet) [24] <- 'fuel_3_value'
    colnames(data_sheet) [25] <- 'fuel_3_unit_scale'
    colnames(data_sheet) [29] <- 'fuel_4_value'
    colnames(data_sheet) [30] <- 'fuel_4_unit_scale'
    colnames(data_sheet) [34] <- 'fuel_5_value'
    colnames(data_sheet) [35] <- 'fuel_5_unit_scale'
    
    #####################################################
    #### SET UP PLOTS - CALLING ON EXTERNAL FUNCTIONS ###
    #####################################################
    
    # Annual impact globe output
    output$companyPlot <- renderPlot({
        buildCompanyOutput(data_sheet)
    })
}