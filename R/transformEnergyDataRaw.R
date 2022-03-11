transformEnergyDataRaw <- function(data_sheet_energy_raw) {
  
  ########################################## SECTION 2 #######################################################
  ########## Transform data across multiple years in proportion to its Period Covered Start and End ##########
  ############################################################################################################
  
  ###############################################################
  ########## Prepare values for transformation ##################
  ###############################################################
  
  data_sheet_energy_raw <- as_tibble(data_sheet_energy_raw)
  
  data_sheet_energy_scaled <- data_sheet_energy_raw %>% 
    #calculate total number of reported days data is applicable to by subtracting the period covered start date from the end date (R recognizes date values as integers)
    mutate(year_total_reported_days = period_covered_end_date - period_covered_start_date, .after = period_covered_end_date) %>% 
    
    #extract the year from period_covered_end_date to create conditional for leap year
    mutate(period_covered_end_year = year(period_covered_end_date), .after = period_covered_end_date) %>%
    mutate(period_covered_start_year = year(period_covered_start_date), .after = period_covered_start_date) %>% 
    
    #convert year values into the number of in the start or end year data is reported
    mutate(period_covered_start_date_number = (365 - yday(period_covered_start_date)), .after = period_covered_start_date) %>% 
    mutate(period_covered_end_date_number = yday(period_covered_end_date), .after = period_covered_end_date) %>% 
    
    #convert dates to proportions
    mutate(period_covered_start_date_number = (period_covered_start_date_number/as.integer(year_total_reported_days))) %>% 
    mutate(period_covered_end_date_number = (period_covered_end_date_number/as.integer(year_total_reported_days))) %>% 
    
    #rename columns
    rename(period_start_proportion = period_covered_start_date_number) %>% 
    rename(period_end_proportion = period_covered_end_date_number)
  
  #make 2 copies of dataframe, 1 for period 1 values, one for period 2 values
  data_sheet_energy_scaled_period_1 <- data_sheet_energy_scaled
  data_sheet_energy_scaled_period_2 <- data_sheet_energy_scaled
  
  ###############################################################
  ####### Transform values for period 1 in first sheet ##########
  ###############################################################
  
  #created scaled values for period 2
  data_sheet_energy_scaled_period_1$electricity_value_scaled_period <-
    
    #if start and end year are the same, multiply electricity value by 1, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_1$period_covered_start_year == 
             data_sheet_energy_scaled_period_1$period_covered_end_year, data_sheet_energy_scaled_period_1$electricity_value*1,
           data_sheet_energy_scaled_period_1$electricity_value*data_sheet_energy_scaled_period_1$period_start_proportion) 
  
  data_sheet_energy_scaled_period_1$fuel_1_value_scaled_period <-
    
    #if start and end year are the same, multiply fuel 1 value by 1, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_1$period_covered_start_year == 
             data_sheet_energy_scaled_period_1$period_covered_end_year, data_sheet_energy_scaled_period_1$fuel_1_value*1,
           data_sheet_energy_scaled_period_1$fuel_1_value*data_sheet_energy_scaled_period_1$period_start_proportion)
  
  data_sheet_energy_scaled_period_1$fuel_2_value_scaled_period <-
    
    #if start and end year are the same, multiply fuel 2 value by 2, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_1$period_covered_start_year == 
             data_sheet_energy_scaled_period_1$period_covered_end_year, data_sheet_energy_scaled_period_1$fuel_2_value*1,
           data_sheet_energy_scaled_period_1$fuel_2_value*data_sheet_energy_scaled_period_1$period_start_proportion)
  
  data_sheet_energy_scaled_period_1$fuel_3_value_scaled_period <-
    
    #if start and end year are the same, multiply fuel 3 value by 3, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_1$period_covered_start_year == 
             data_sheet_energy_scaled_period_1$period_covered_end_year, data_sheet_energy_scaled_period_1$fuel_3_value*1,
           data_sheet_energy_scaled_period_1$fuel_3_value*data_sheet_energy_scaled_period_1$period_start_proportion)
  
  data_sheet_energy_scaled_period_1$fuel_4_value_scaled_period <-
    
    #if start and end year are the same, multiply fuel 4 value by 4, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_1$period_covered_start_year == 
             data_sheet_energy_scaled_period_1$period_covered_end_year, data_sheet_energy_scaled_period_1$fuel_4_value*1,
           data_sheet_energy_scaled_period_1$fuel_4_value*data_sheet_energy_scaled_period_1$period_start_proportion)
  
  data_sheet_energy_scaled_period_1$fuel_5_value_scaled_period <-
    
    #if start and end year are the same, multiply fuel 5 value by 5, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_1$period_covered_start_year == 
             data_sheet_energy_scaled_period_1$period_covered_end_year, data_sheet_energy_scaled_period_1$fuel_5_value*1,
           data_sheet_energy_scaled_period_1$fuel_5_value*data_sheet_energy_scaled_period_1$period_start_proportion)
  
  #shift all scaled columns to earlier columns in the spreadsheet for easier viewing
  data_sheet_energy_scaled_period_1 <- data_sheet_energy_scaled_period_1 %>% 
    relocate(electricity_value_scaled_period, .after = electricity_value) %>% 
    relocate(fuel_1_value_scaled_period, .after = fuel_1_value) %>% 
    relocate(fuel_2_value_scaled_period, .after = fuel_2_value) %>%
    relocate(fuel_3_value_scaled_period, .after = fuel_3_value) %>%
    relocate(fuel_4_value_scaled_period, .after = fuel_4_value) %>%
    relocate(fuel_5_value_scaled_period, .after = fuel_5_value) %>% 
    
    #save only start date, end date, and data year (year data applies to) by removing the following columns
    select(!c(period_start_proportion, period_covered_end_year, period_end_proportion, year_total_reported_days)) %>% 
    rename(data_year = period_covered_start_year)
  
  ###############################################################
  ####### Transform values for period 2 in second sheet #########
  ###############################################################
  
  #created scaled values for period 2
  data_sheet_energy_scaled_period_2$electricity_value_scaled_period <-
    
    #if start and end year are the same, multiply electricity value by 1, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_2$period_covered_start_year == 
             data_sheet_energy_scaled_period_2$period_covered_end_year, data_sheet_energy_scaled_period_2$electricity_value*1,
           data_sheet_energy_scaled_period_2$electricity_value*data_sheet_energy_scaled_period_2$period_end_proportion) 
  
  data_sheet_energy_scaled_period_2$fuel_1_value_scaled_period <-
    
    #if start and end year are the same, multiply fuel 1 value by 1, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_2$period_covered_start_year == 
             data_sheet_energy_scaled_period_2$period_covered_end_year, data_sheet_energy_scaled_period_2$fuel_1_value*1,
           data_sheet_energy_scaled_period_2$fuel_1_value*data_sheet_energy_scaled_period_2$period_end_proportion)
  
  data_sheet_energy_scaled_period_2$fuel_2_value_scaled_period <-
    
    #if start and end year are the same, multiply fuel 1 value by 1, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_2$period_covered_start_year == 
             data_sheet_energy_scaled_period_2$period_covered_end_year, data_sheet_energy_scaled_period_2$fuel_2_value*1,
           data_sheet_energy_scaled_period_2$fuel_2_value*data_sheet_energy_scaled_period_2$period_end_proportion)
  
  data_sheet_energy_scaled_period_2$fuel_3_value_scaled_period <-
    
    #if start and end year are the same, multiply fuel 1 value by 1, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_2$period_covered_start_year == 
             data_sheet_energy_scaled_period_2$period_covered_end_year, data_sheet_energy_scaled_period_2$fuel_3_value*1,
           data_sheet_energy_scaled_period_2$fuel_3_value*data_sheet_energy_scaled_period_2$period_end_proportion)
  
  data_sheet_energy_scaled_period_2$fuel_4_value_scaled_period <-
    
    #if start and end year are the same, multiply fuel 1 value by 1, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_2$period_covered_start_year == 
             data_sheet_energy_scaled_period_2$period_covered_end_year, data_sheet_energy_scaled_period_2$fuel_4_value*1,
           data_sheet_energy_scaled_period_2$fuel_4_value*data_sheet_energy_scaled_period_2$period_end_proportion)
  
  data_sheet_energy_scaled_period_2$fuel_5_value_scaled_period <-
    
    #if start and end year are the same, multiply fuel 1 value by 1, else, multiply by the proportion of days in the first year
    ifelse(data_sheet_energy_scaled_period_2$period_covered_start_year == 
             data_sheet_energy_scaled_period_2$period_covered_end_year, data_sheet_energy_scaled_period_2$fuel_5_value*1,
           data_sheet_energy_scaled_period_2$fuel_5_value*data_sheet_energy_scaled_period_2$period_end_proportion)
  
  #drop all values where period covered start and end are equal prior to rbinding to the period 1 dataframe
  data_sheet_energy_scaled_period_2 <- subset(data_sheet_energy_scaled_period_2, period_covered_start_year != period_covered_end_year) 
  
  #relocate scaled column values to align with period 1 values
  data_sheet_energy_scaled_period_2 <- data_sheet_energy_scaled_period_2 %>% 
    relocate(electricity_value_scaled_period, .after = electricity_value) %>% 
    relocate(fuel_1_value_scaled_period, .after = fuel_1_value) %>% 
    relocate(fuel_2_value_scaled_period, .after = fuel_2_value) %>%
    relocate(fuel_3_value_scaled_period, .after = fuel_3_value) %>%
    relocate(fuel_4_value_scaled_period, .after = fuel_4_value) %>%
    relocate(fuel_5_value_scaled_period, .after = fuel_5_value) %>% 
    
    #save only start date, end date, and data year (year data applies to) by removing the following columns
    select(!c(period_start_proportion, period_covered_start_year, period_end_proportion, year_total_reported_days)) %>% 
    rename(data_year = period_covered_end_year)
  
  ###############################################################
  ####### Combine spreadsheets together and combine years #######
  ###############################################################
  
  data_sheet_energy_scaled_combined <- rbind(data_sheet_energy_scaled_period_1, data_sheet_energy_scaled_period_2) %>%
    
    #drop original raw values from spreadsheet
    select(!c(electricity_value, fuel_1_value, fuel_2_value, fuel_3_value, fuel_4_value, fuel_5_value)) %>% 
    
    #rename scaled columns to be original raw value names
    rename(electricity_value = electricity_value_scaled_period) %>% 
    rename(fuel_1_value = fuel_1_value_scaled_period) %>% 
    rename(fuel_2_value = fuel_2_value_scaled_period) %>% 
    rename(fuel_3_value = fuel_3_value_scaled_period) %>% 
    rename(fuel_4_value = fuel_4_value_scaled_period) %>% 
    rename(fuel_5_value = fuel_5_value_scaled_period) %>% 
    
    #rename columns
    relocate(electricity_value, .after = notes) %>% 
    relocate(fuel_1_value, .after = fuel_1_type) %>% 
    relocate(fuel_2_value, .after = fuel_2_type) %>%
    relocate(fuel_3_value, .after = fuel_3_type) %>%
    relocate(fuel_4_value, .after = fuel_4_type) %>%
    relocate(fuel_5_value, .after = fuel_5_type)
  
  ########################################## SECTION 3 #######################################################
  ####### Multiply all data by respective unit scale and conversion unit to transform all values to kWh ######
  ############################################################################################################
  
  ###########################################################################################
  ## Multiply values by unit scale to generate values that are ready for unit conversion ####
  ###########################################################################################
  
  data_sheet_energy <- data_sheet_energy_scaled_combined %>% 
    
    #replace any unit_scale NA values with 1
    mutate(unit_scale = replace_na(unit_scale, 1)) %>%
    mutate(fuel_1_unit_scale = replace_na(fuel_1_unit_scale, 1)) %>%
    mutate(fuel_2_unit_scale = replace_na(fuel_2_unit_scale, 1)) %>%
    mutate(fuel_3_unit_scale = replace_na(fuel_3_unit_scale, 1)) %>%
    mutate(fuel_4_unit_scale = replace_na(fuel_4_unit_scale, 1)) %>%
    mutate(fuel_5_unit_scale = replace_na(fuel_5_unit_scale, 1)) %>%
    
    #multiply electricity and fuel values by unit scale
    mutate(electricity_prepped = electricity_value * unit_scale, .after = unit_scale) %>% 
    mutate(fuel_1_prepped = fuel_1_value * fuel_1_unit_scale, .after = fuel_1_unit_scale) %>% 
    mutate(fuel_2_prepped = fuel_2_value * fuel_2_unit_scale, .after = fuel_2_unit_scale) %>% 
    mutate(fuel_3_prepped = fuel_3_value * fuel_3_unit_scale, .after = fuel_3_unit_scale) %>% 
    mutate(fuel_4_prepped = fuel_4_value * fuel_4_unit_scale, .after = fuel_4_unit_scale) %>% 
    mutate(fuel_5_prepped = fuel_5_value * fuel_5_unit_scale, .after = fuel_5_unit_scale)
  
  data_sheet_energy <- data_sheet_energy %>% 
    
    #merge fuel type and units to prepare for vlookup function
    unite(fuel_unit_1, c(fuel_1_type, fuel_1_unit), sep = " ", remove = FALSE) %>% 
    unite(fuel_unit_2, c(fuel_2_type, fuel_2_unit), sep = " ", remove = FALSE) %>% 
    unite(fuel_unit_3, c(fuel_3_type, fuel_3_unit), sep = " ", remove = FALSE) %>% 
    unite(fuel_unit_4, c(fuel_4_type, fuel_4_unit), sep = " ", remove = FALSE) %>% 
    unite(fuel_unit_5, c(fuel_5_type, fuel_5_unit), sep = " ", remove = FALSE)
  
  #################################################################################################
  ####### Calculate energy unit conversions to base unit (kWh) and add to data_sheet_energy #######
  #################################################################################################
  
  #import conversions spreadsheet, select columns data types, clean header names, select columns, unite fuel and unit columns
  conversions_sheet <- read.xlsx2(here('data',"DataCenterEnergyUse-RawCollection.xlsx"), 6,
                                  colClasses=c("character", "character", "character", "numeric")) %>% 
    clean_names() %>% 
    select('fuel', 'unit', 'k_wh_1_unit') %>% 
    drop_na() %>% 
    unite('fuel_unit', fuel:unit, sep = " ", remove = FALSE)
  
  ######################################
  #calculate conversion for electricity#
  ######################################
  
  #isolate electricity values and respective units into dataframe
  electricity_data <- data_sheet_energy %>% 
    select('electricity_prepped', 'unit') %>% 
    relocate(electricity_prepped, .after = unit)
  
  #select just unit column and the relevant conversion because electricity values do not have an associated fuel
  conversions_sheet_electricity <- conversions_sheet %>%
    select('unit', 'k_wh_1_unit') %>% 
    distinct()
  
  #join and match unit conversion factor for electricity values (i.e. if MWh value should be multiplied by 1000)
  electricity_conversion <- left_join(electricity_data, conversions_sheet_electricity, by="unit") %>% 
    mutate(electricity_converted = electricity_prepped * k_wh_1_unit)
  
  #################################
  #calculate conversions for fuels#
  #################################
  
  #FUEL 1
  
  #isolate fuel unit and prepped values into dataframe
  fuel_1_data <- data_sheet_energy %>% 
    select('fuel_unit_1', 'fuel_1_prepped')
  
  #change name of left column from conversion sheet to match fuel number
  conversions_sheet_fuel_1 <- conversions_sheet %>%
    select('fuel_unit', 'k_wh_1_unit') %>% 
    rename(fuel_unit_1 = fuel_unit)
  
  #join and match, calculate prepped fuel value multiplied by its respective conversion
  fuel_1_conversion <- left_join(fuel_1_data, conversions_sheet_fuel_1, by="fuel_unit_1") %>% 
    mutate(fuel_1_converted = fuel_1_prepped * k_wh_1_unit)
  
  #FUEL 2
  
  #isolate fuel unit and prepped values into dataframe
  fuel_2_data <- data_sheet_energy %>% 
    select('fuel_unit_2', 'fuel_2_prepped')
  
  #change name of left column from conversion sheet to match fuel number
  conversions_sheet_fuel_2 <- conversions_sheet %>%
    select('fuel_unit', 'k_wh_1_unit') %>% 
    rename(fuel_unit_2 = fuel_unit)
  
  #join and match, calculate prepped fuel value multiplied by its respective conversion
  fuel_2_conversion <- left_join(fuel_2_data, conversions_sheet_fuel_2, by="fuel_unit_2") %>% 
    mutate(fuel_2_converted = fuel_2_prepped * k_wh_1_unit)
  
  #FUEL 3
  
  #isolate fuel unit and prepped values into dataframe
  fuel_3_data <- data_sheet_energy %>% 
    select('fuel_unit_3', 'fuel_3_prepped')
  
  #change name of left column from conversion sheet to match fuel number
  conversions_sheet_fuel_3 <- conversions_sheet %>% 
    select('fuel_unit', 'k_wh_1_unit') %>% 
    rename(fuel_unit_3 = fuel_unit)
  
  #join and match, calculate prepped fuel value multiplied by its respective conversion
  fuel_3_conversion <- left_join(fuel_3_data, conversions_sheet_fuel_3, by="fuel_unit_3") %>% 
    mutate(fuel_3_converted = fuel_3_prepped * k_wh_1_unit)
  
  #FUEL 4
  
  #isolate fuel unit and prepped values into dataframe
  fuel_4_data <- data_sheet_energy %>% 
    select('fuel_unit_4', 'fuel_4_prepped')
  
  #change name of left column from conversion sheet to match fuel number
  conversions_sheet_fuel_4 <- conversions_sheet %>%
    select('fuel_unit', 'k_wh_1_unit') %>% 
    rename(fuel_unit_4 = fuel_unit)
  
  #join and match, calculate prepped fuel value multiplied by its respective conversion
  fuel_4_conversion <- left_join(fuel_4_data, conversions_sheet_fuel_4, by="fuel_unit_4") %>% 
    mutate(fuel_4_converted = fuel_4_prepped * k_wh_1_unit)
  
  #FUEL 5
  
  #isolate fuel unit and prepped values into dataframe
  fuel_5_data <- data_sheet_energy %>% 
    select('fuel_unit_5', 'fuel_5_prepped')
  
  #change name of left column from conversion sheet to match fuel number
  conversions_sheet_fuel_5 <- conversions_sheet %>% 
    select('fuel_unit', 'k_wh_1_unit') %>% 
    rename(fuel_unit_5 = fuel_unit)
  
  #join and match, calculate prepped fuel value multiplied by its respective conversion
  fuel_5_conversion <- left_join(fuel_5_data, conversions_sheet_fuel_5, by="fuel_unit_5") %>% 
    mutate(fuel_5_converted = fuel_5_prepped * k_wh_1_unit)
  
  #Bind converted values to data_sheet_energy, move columns next to fuel_1_prepped for easier viewing
  data_sheet_energy <- cbind(data_sheet_energy, electricity_conversion["electricity_converted"],
                             fuel_1_conversion["fuel_1_converted"], 
                             fuel_2_conversion["fuel_2_converted"],
                             fuel_3_conversion["fuel_3_converted"],
                             fuel_4_conversion["fuel_4_converted"],
                             fuel_5_conversion["fuel_5_converted"]) %>% 
    relocate(fuel_1_converted, .after = fuel_1_prepped) %>% 
    relocate(fuel_2_converted, .after = fuel_2_prepped) %>%
    relocate(fuel_3_converted, .after = fuel_3_prepped) %>%
    relocate(fuel_4_converted, .after = fuel_4_prepped) %>%
    relocate(fuel_5_converted, .after = fuel_5_prepped) %>% 
    relocate(electricity_converted, .after = electricity_prepped)
  
  data_sheet_energy
  
}