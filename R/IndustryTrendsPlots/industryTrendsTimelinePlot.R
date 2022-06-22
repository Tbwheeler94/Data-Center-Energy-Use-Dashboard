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
  
  company_profile <- data_sheet_company_raw %>% select(company_name, checked_back_to_founding_year_or_2007) %>%
    filter(checked_back_to_founding_year_or_2007 == "Yes")
  
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
  industry_transparency$energy_reporting_scope[industry_transparency$energy_reporting_scope == "No Reporting of Data" & industry_transparency$data_year == max(na.omit(data_sheet_energy_raw$report_year))] <- "Pending Data Submission"
  industry_transparency <- industry_transparency %>% select(-c(fuel_1_type)) %>% filter(company %in% company_profile$company_name)
  industry_transparency <- industry_transparency[order(industry_transparency$company),]
  industry_transparency$company <- factor(industry_transparency$company, levels=rev(unique(industry_transparency$company)))
  
  list_of_companies <- unique(industry_transparency$company)
  list_of_companies
  
  industry_transparency$data_year <- paste0("01/01/", industry_transparency$data_year)
  industry_transparency$data_year <- as.Date(industry_transparency$data_year, "%d/%m/%Y")
  
  p <- ggplot(industry_transparency, aes(x=data_year, y=company, 
                                         text=paste("Company: ", company, "\nData Year: ", 
                                                    format(data_year, format="%Y"), 
                                                    "\nReporting Scope: ", energy_reporting_scope))) +
    geom_tile(aes(fill=energy_reporting_scope), height=0.75) +
    labs(energy_reporting_scope="Reporting Scope") +
    theme(
      legend.text=element_text(size=10),
      axis.line.x=element_blank(),
      axis.text.x=element_blank(),
      axis.title.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_text(size=12),
      axis.line.y=element_line(colour="black", size=1),
      panel.background=element_blank()
    )
  
  #ggplotly(p, tooltip = "text")
  
  timeline_plot_height <- length(unique_companies) * 30
  
  return(ggplotly(p, height=timeline_plot_height, tooltip = "text") %>% config(displayModeBar = T)  %>%
           plotly::layout(legend = list(orientation = "h", x = 0.05, y = 1.1)))
  
}