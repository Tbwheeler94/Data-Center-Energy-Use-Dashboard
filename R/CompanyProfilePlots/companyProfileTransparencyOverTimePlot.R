buildCompanyProfileTransparencyOverTimePlot <- function(data_sheet_energy_transformed, selected_company) {
  for (year in 2007:as.integer(max(na.omit(data_sheet_energy_raw$report_year)))) {
    # create a sub data frame that is filtered by data year and single data center scope
    company_SDC <- data_sheet_energy_transformed %>%
      filter(company %in% selected_company, data_year == year, energy_reporting_scope == "Single Data Center") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    # create a sub data frame that is filtered by data year and multiple data centers scope
    company_MDC <- data_sheet_energy_transformed %>%
      filter(company %in% selected_company, data_year == year, energy_reporting_scope == "Multiple Data Centers") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type)
    
    # create a sub data frame that is filtered by data year / company wide electricity scope
    company_CW <- data_sheet_energy_transformed %>%
      filter(company %in% selected_company, data_year == year, energy_reporting_scope == "Total Operations") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    company_EN <- data_sheet_energy_transformed %>%
      filter(company %in% selected_company, data_year == year, fuel_1_type == "Total Energy Use") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    if (year == 2007) {
      company_transparency <- rbind(company_SDC, company_MDC, company_CW, company_EN)
       if (nrow(company_transparency) == 0) {
         company_transparency[nrow(company_transparency)+1, ] <- c(selected_company,year,"","")
       }
    } else {
      company_transparency <- company_transparency %>% rbind(company_SDC, company_MDC, company_CW, company_EN)
      if (!(year %in% company_transparency)) {
         company_transparency[nrow(company_transparency)+1, ] <- c(selected_company,year,"","")
       }
    }
  }

  #company_transparency <- company_transparency %>% distinct(company, data_year, .keep_all = TRUE)
  year_axis <- company_transparency %>% distinct(company, data_year, .keep_all = TRUE) %>% select(-c(company, fuel_1_type))
  year_axis$energy_reporting_scope[year_axis$energy_reporting_scope == "Single Data Center"] <- "Reported Data\nCenter Electricity"
  year_axis$energy_reporting_scope[year_axis$energy_reporting_scope == "Multiple Data Centers"] <- "Reported Data\nCenter Electricity"
  year_axis$energy_reporting_scope[year_axis$energy_reporting_scope == "Total Operations"] <- "Reported Company\nWide Electricity"
  year_axis$energy_reporting_scope[year_axis$fuel_1_type == "Total Energy Use"] <- "Reported Company\nWide Energy"
  year_axis$energy_reporting_scope[year_axis$energy_reporting_scope == ""] <- "No Reporting\nof Data"
  
  #company_transparency <- company_transparency %>% distinct(company, data_year, .keep_all = TRUE) %>% distinct(company, energy_reporting_scope, .keep_all = TRUE)
  company_transparency <- company_transparency %>% distinct(company, data_year, .keep_all = TRUE)
  company_transparency$energy_reporting_scope[company_transparency$energy_reporting_scope == "Single Data Center"] <- "Reported Data\nCenter Electricity"
  company_transparency$energy_reporting_scope[company_transparency$energy_reporting_scope == "Multiple Data Centers"] <- "Reported Data\nCenter Electricity"
  company_transparency$energy_reporting_scope[company_transparency$energy_reporting_scope == "Total Operations"] <- "Reported Company\nWide Electricity"
  company_transparency$energy_reporting_scope[company_transparency$fuel_1_type == "Total Energy Use"] <- "Reported Company\nWide Energy"
  company_transparency$energy_reporting_scope[company_transparency$energy_reporting_scope == ""] <- "No Reporting\nof Data"
  company_transparency <- company_transparency %>% select(-c(company, fuel_1_type))
  
  company_transparency$group <- 0
  count <- 1
  company_transparency[1,3] <- count
  test_reporting <- company_transparency[1,2]
  
  for (i in 2:nrow(company_transparency)) {
    if (company_transparency[i,2] == test_reporting) {
      company_transparency[i,3] <- count
    } else {
      count <- count + 1
      test_reporting <- company_transparency[i,2]
      company_transparency[i,3] <- count
    }
  }
  
  company_transparency <- company_transparency %>% distinct(group, .keep_all = TRUE) %>% select(-c(group))
  
  company_transparency$position <- 0
  company_transparency$text_position <- 0
  for (i in 1:nrow(company_transparency)) {
    if (i %% 3 == 0) {
      company_transparency[i,3] <- 1
      company_transparency[i,4] <- 1
    } else if (i %% 3 == 1) {
      company_transparency[i,3] <- 0.5
      company_transparency[i,4] <- 0.5
    } else if (i %% 3 == 2) {
      company_transparency[i,3] <- 1.5
      company_transparency[i,4] <- 1.5
    }
    # position_val <- position_val + factor
    # company_transparency[i,3] <- position_val
    # company_transparency[i,4] <- position_val
    # factor <- factor * -1
  }
  
  status_levels <- c("Reported Data\nCenter Electricity", "Reported Company\nWide Electricity", 
                     "Reported Company\nWide Energy", "No Reporting\nof Data")
  status_colors <- c("#0070C0", "#00B050", "#FFC000", "#C00000")
  
  year_axis$energy_reporting_scope <- factor(year_axis$energy_reporting_scope, levels=status_levels, ordered=TRUE)
  year_axis$year_text <- as.character(year_axis$data_year)
  
  ggplot(data=year_axis, aes(x=data_year, y=0, color=energy_reporting_scope)) +
    geom_hline(yintercept=0, color="black", size=0.3) +
    geom_point(data=year_axis, aes(y=0), size=3) +
    geom_segment(data=company_transparency, aes(y=position,yend=0,xend=data_year), color='black', size=0.2) +
    geom_label(data=company_transparency, aes(y=text_position, label=energy_reporting_scope), fill="white", size=5, vjust="inward") +
    geom_text(data=year_axis, aes(x=year_text, y=-0.2, label=year_text), color="black", size=4) +
    scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE) +
    scale_x_discrete(expand = expansion(mult = 0.1)) +
    theme_classic() +
    labs(col="Reporting Scope") +
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.line.x=element_blank(),
          legend.position = "none"
    )
}