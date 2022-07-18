buildIndustryTrendsTransparencyPlot <- function(data_sheet_energy_raw) {
  
  for (year_count in 2007:as.integer(max(na.omit(data_sheet_energy_transformed$data_year)))) {
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
  data_founding_year <- data_founding_year[data_founding_year$checked_back_to_founding_year_or_2007=="Yes", ]
  data_of_transparency <- subset(data_of_transparency, company %in% data_founding_year$company_name)
  
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
  
  data_of_transparency$value <- 1
  data_of_transparency <- data_of_transparency %>% select(data_year, energy_reporting_scope, 
                                                          fuel_1_type, value)
  
  # loop through all rows and rename energy reporting scopes
  data_of_transparency$energy_reporting_scope[data_of_transparency$energy_reporting_scope == "Single Data Center" ] <- "Reported Data Center Electricity"
  data_of_transparency$energy_reporting_scope[data_of_transparency$energy_reporting_scope == "Multiple Data Centers" ] <- "Reported Data Center Electricity"
  data_of_transparency$energy_reporting_scope[data_of_transparency$energy_reporting_scope == "Total Operations"] <- "Reported Company Wide Electricity"
  data_of_transparency$energy_reporting_scope[data_of_transparency$fuel_1_type == "Total Energy Use"] <- "Reported Company Wide Total Energy"
  data_of_transparency$energy_reporting_scope[data_of_transparency$energy_reporting_scope == "0"] <- "No Reporting of Publicly Available Data"
  data_of_transparency$energy_reporting_scope[data_of_transparency$energy_reporting_scope == "No Reporting of Publicly Available Data" & data_of_transparency$data_year == max(na.omit(data_sheet_energy_transformed$data_year))] <- "Pending Data Submission"
  
  # stack single data center/multiple data center data frames on top of each other
  data_of_transparency_final <- data_of_transparency %>%
    group_by(data_year,energy_reporting_scope) %>%
    dplyr::summarise(value = sum(value), .groups = "rowwise") %>%
    as.data.frame()
  data_of_transparency_final <- data_of_transparency_final %>%
    group_by(data_year,energy_reporting_scope)
  
  data_of_transparency <- data_of_transparency_final
  
  data_of_transparency$energy_reporting_scope <- factor(data_of_transparency$energy_reporting_scope, 
                                                        levels=c("Reported Data Center Electricity",
                                                                 "Reported Company Wide Electricity",
                                                                 "Reported Company Wide Total Energy",
                                                                 "No Reporting of Publicly Available Data",
                                                                 "Pending Data Submission"))
  
  status_levels <- c("Reported Data Center Electricity", "Reported Company Wide Electricity",
                     "Reported Company Wide Total Energy", "No Reporting of Publicly Available Data", "Pending Data Submission")
  status_colors <- c("#3BCA6D", "#77945C", "#FF6865", "#ED2938", "#B88C8C")
  
  p <- ggplot(data_of_transparency, aes(x=data_year, 
        text=paste("Data Year: ", data_year, "\nNumber of Companies: ", value))) + 
    geom_bar(aes(y=value, fill=energy_reporting_scope),
             position="stack", stat="identity") +
    scale_fill_manual(values=status_colors, labels=status_levels, drop=FALSE) +
    theme_classic() +
    theme(
      legend.title = element_text(size=14),
      legend.text = element_text(size=12)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.01,0))) +
    xlab("Year") +
    ylab("Number of Companies") +
    labs(fill = "Energy Reporting Scope")
  
  ggplotly(p, tooltip = "text")
  
}