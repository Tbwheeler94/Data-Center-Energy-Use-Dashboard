buildIndustryTrendsTransparencyPlot <- function(data_sheet_energy_raw) {
  
  for (year_count in 2007:as.integer(max(na.omit(data_sheet_energy_raw$report_year)))) {
    # create a sub data frame that is filtered by data year and single data center scope
    data_of_transparency_SDC <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "Single Data Center") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_SDC <- distinct(data_of_transparency_SDC, .keep_all = TRUE)
    
    # create a sub data frame that is filtered by data year and multiple data centers scope
    data_of_transparency_MDC <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "Multiple Data Centers") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type)
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_MDC <- distinct(data_of_transparency_MDC, .keep_all = TRUE)
    
    # create a sub data frame that is filtered by data year and company wide electricity scope
    data_of_transparency_TO <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "Total Operations") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_TO <- distinct(data_of_transparency_TO, .keep_all = TRUE)
    
    # create a sub data frame that is filtered by data year (***LOOK BACK AT THIS***)
    # IMPORTANT: leave energy reporting scope column blank for this transparency level!
    data_of_transparency_ND <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type)
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_ND <- distinct(data_of_transparency_ND, .keep_all = TRUE)
    
    # stack single data center/multiple data center data frames on top of each other
    data_of_transparency_L1 <- rbind(data_of_transparency_SDC, data_of_transparency_MDC)
    # since single/multiple data centers fall under same level, diminish to 1 row per company
    data_of_transparency_L1 <- distinct(data_of_transparency_L1, company, .keep_all = TRUE)
    
    # stack rest of the data frames together
    data_of_transparency_L1 <- rbind(data_of_transparency_L1, data_of_transparency_TO,
                                     data_of_transparency_ND)
    data_of_transparency_L1 <- distinct(data_of_transparency_L1, company, .keep_all = TRUE)
    data_of_transparency_L1$value <- nrow(data_of_transparency_L1)
    
    # create the master data frame from the first index of the for loop
    # else: add the existing master data frame to the recently computed data frames
    if (year_count == 2007) {
      data_of_transparency <- rbind(data_of_transparency_L1)
    } else {
      data_of_transparency <- data_of_transparency %>% rbind(data_of_transparency_L1)
    }
    # sort graph in alphabetical order of company in order to make summation loop easier
    data_of_transparency <- data_of_transparency[order(data_of_transparency[,"data_year"]), ]
    #data_of_transparency <- distinct(data_of_transparency, company, .keep_all = TRUE)
  }
  
  # create another data frame that is filtered by company founding year
  data_founding_year <- data_sheet_company_raw %>% select(company_name, 
                                                          checked_back_to_founding_year_or_2007,
                                                          company_founding_year)
  # drop all the excess rows that have NA data
  data_founding_year <- na.omit(data_founding_year)
  data_founding_year <- data_founding_year[data_founding_year$checked_back_to_founding_year_or_2007 == "Yes", ]
  
  data_of_transparency <- subset(data_of_transparency, 
                                 company %in% data_founding_year$company_name)
  
  # assessing if there are companies in data_founding_year that ARE NOT in data_of_transparency
  for (i in 1:nrow(data_founding_year)) {
    if (!any(data_of_transparency == data_founding_year[i,1])) {
      data_of_transparency[nrow(data_of_transparency)+1, ] <- 
        c(data_founding_year[i,1], 2007, 0, 0, 0)
    }
  }
  
  # create new rows for companies that do not have L1/L2/L3 transparency in that given year
  # in other words, create rows for level 4 companies (no report found)
  data_of_transparency <- dummy_rows(data_of_transparency, 
                                     select_columns = c("company", "data_year"),
                                     dummy_value = 0)
  data_of_transparency <- data_of_transparency[order(data_of_transparency[,"data_year"]), ]
  
  # NOW we have data for some companies that didn't even exist yet!
  # take each company in data_founding year, compare with ALL rows in data_of_transparency,
  # and if the founding year of the company is greater than the data year of the company,
  # then keep track of that index in deleted_rows vector 
  deleted_rows <- vector()
  z = 1
  for (i in 1:nrow(data_founding_year)) {
    for (j in 1:nrow(data_of_transparency)) {
      if (data_founding_year[i,1] == data_of_transparency[j,1]) {
        if (data_founding_year[i,3] > data_of_transparency[j,2]) {
          deleted_rows[z] <- j
          z <- z + 1
        }
      }
    }
  }
  
  # drop all rows that have company data prior to their founding year
  if (length(deleted_rows) != 0) {
    data_of_transparency <- data_of_transparency[-c(deleted_rows), ]
  }
  
  data_of_transparency$value <- 0
  data_of_transparency$number_of_companies <- 0
  data_of_transparency <- data_of_transparency %>% select(data_year, energy_reporting_scope, 
                                                          fuel_1_type, value, number_of_companies)
  
  # temporary garbage values 
  year_dummy = 2006
  report_dummy = "Hi"
  
  # loop through all rows and rename energy reporting scopes
  j = 0
  for (i in 1:nrow(data_of_transparency)) {
    if (data_of_transparency[i,2] == "Single Data Center" || data_of_transparency[i,2] == "Multiple Data Centers") {
      data_of_transparency[i,2] <- "Reported Data Center Electricity"
    } else if (data_of_transparency[i,2] == "Total Operations" && data_of_transparency[i,3] != "Total Energy Use") {
      data_of_transparency[i,2] <- "Reported Company Wide Electricity"
    } else if (data_of_transparency[i,2] == "Total Operations" && data_of_transparency[i,3] == "Total Energy Use") {
      data_of_transparency[i,2] <- "Reported Company Wide Total Energy"
    } else {
      data_of_transparency[i,2] <- "No Reporting of Data"
    }
  }
  
  data_of_transparency <- data_of_transparency[order(data_of_transparency[,"data_year"],
                                                     data_of_transparency[,"energy_reporting_scope"]), ]
  
  for (i in 1:nrow(data_of_transparency)) {
    # sum the number rows based on company name and energy reporting scope
    # keeping track of the summation at the first instance of a distinct row
    if (data_of_transparency[i,1] == year_dummy && data_of_transparency[i,2] == report_dummy) {
      data_of_transparency[i-j,4] <- data_of_transparency[i-j,4] + 1
      j <- j + 1
    } else {
      year_dummy <- data_of_transparency[i,1]
      report_dummy <- data_of_transparency[i,2]
      data_of_transparency[i,4] <- 1
      j <- 1
    }
  }
  
  # drop the rest of the rows with value of 0
  #data_of_transparency <- data_of_transparency[order(data_of_transparency[,""]), ]
  #data_of_transparency <- distinct(data_of_transparency, data_year, .keep_all = TRUE)
  
  # find line graph values
  year_dummy = 2006
  j = 0
  for (i in 1:nrow(data_of_transparency)) {
    if (data_of_transparency[i,1] == year_dummy) {
      data_of_transparency[i-j,5] = data_of_transparency[i-j,5] + data_of_transparency[i,4]
      j = j + 1
    } else {
      year_dummy = data_of_transparency[i,1]
      data_of_transparency[i,5] = data_of_transparency[i,4]
      j = 1
    }
  }
  
  # assign line graph values to each row with same year
  year_dummy = 2006
  for (i in 1:nrow(data_of_transparency)) {
    if (data_of_transparency[i,1] == year_dummy) {
      data_of_transparency[i,5] = data_of_transparency[i-1,5]
    } else {
      year_dummy = data_of_transparency[i,1]
    }
  }
  
  p <- ggplot(data_of_transparency, aes(x=data_year)) + 
    geom_bar(aes(y=value, fill=energy_reporting_scope),
             position=position_stack(reverse = TRUE), stat="identity") +
    #geom_line(aes(y=number_of_companies), size=1.2, color="black") +
    ggtitle("Data Center Transparency") +
    scale_fill_manual(values = c("#E9967A", "#8FBC8F", "#00CED1", "#1E90FF")) +
    xlab("Year") +
    ylab("Number of Companies") +
    labs(fill = "Energy Reporting Scope")
  
  ggplotly(p)
  
}