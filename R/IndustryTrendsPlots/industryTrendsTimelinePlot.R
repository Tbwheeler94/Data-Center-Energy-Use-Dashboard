buildIndustryTrendsTimelinePlot <- function(data_sheet_energy_transformed) {
  for (year in 2007:as.integer(max(na.omit(data_sheet_energy_raw$report_year)))) {
    # create a sub data frame that is filtered by data year and single data center scope
    company_SDC <- data_sheet_energy_transformed %>%
      filter(data_year == year, energy_reporting_scope == "Single Data Center") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    # create a sub data frame that is filtered by data year and multiple data centers scope
    company_MDC <- data_sheet_energy_transformed %>%
      filter(data_year == year, energy_reporting_scope == "Multiple Data Centers") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type)
    
    # create a sub data frame that is filtered by data year / company wide electricity scope
    company_CW <- data_sheet_energy_transformed %>%
      filter(data_year == year, energy_reporting_scope == "Total Operations") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    company_EN <- data_sheet_energy_transformed %>%
      filter(data_year == year, fuel_1_type == "Total Energy Use") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    if (year == 2007) {
      industry_transparency <- rbind(company_SDC, company_MDC, company_CW, company_EN)
    } else {
      industry_transparency <- industry_transparency %>% rbind(company_SDC, company_MDC, company_CW, company_EN)
    }
  }
  
  industry_transparency <- industry_transparency %>% distinct(company, data_year, .keep_all = TRUE)
  industry_transparency <- dummy_rows(industry_transparency, 
                                     select_columns = c("company", "data_year"),
                                     dummy_value = "")
  industry_transparency <- industry_transparency[order(industry_transparency$data_year), ]
  industry_transparency$energy_reporting_scope[industry_transparency$energy_reporting_scope == "Single Data Center"] <- "Reported Data Center Electricity"
  industry_transparency$energy_reporting_scope[industry_transparency$energy_reporting_scope == "Multiple Data Centers"] <- "Reported Data Center Electricity"
  industry_transparency$energy_reporting_scope[industry_transparency$energy_reporting_scope == "Total Operations"] <- "Reported Company Wide Electricity"
  industry_transparency$energy_reporting_scope[industry_transparency$fuel_1_type == "Total Energy Use"] <- "Reported Company Wide Energy"
  industry_transparency$energy_reporting_scope[industry_transparency$energy_reporting_scope == ""] <- "No Reporting of Data"
  industry_transparency <- industry_transparency %>% select(-c(fuel_1_type))
  
  industry_transparency$data_year <- paste0("01/01/", industry_transparency$data_year)
  industry_transparency$data_year <- as.Date(industry_transparency$data_year, "%d/%m/%Y")
  
  p <- ggplot(industry_transparency, aes(x=data_year, y=company)) +
    geom_tile(aes(fill=energy_reporting_scope), height=0.75) +
    #geom_bar(aes(fill=energy_reporting_scope), stat="identity", position="stack") +
    #scale_x_date(breaks = industry_transparency$data_year) +
    #xlim(as.Date("2007-01-01"), as.Date("2020-12-31")) +
    theme_classic()
  
  ggplotly(p, height = 1500)
  
}