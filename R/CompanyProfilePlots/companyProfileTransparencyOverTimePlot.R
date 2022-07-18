buildCompanyProfileTransparencyOverTimePlot <- function(data_sheet_energy_transformed, selected_company) {
  for (year in 2007:as.integer(max(na.omit(data_sheet_energy_transformed$data_year)))) {
    # create a sub data frame that is filtered by data year and single data center scope
    company_SDC <- data_sheet_energy_transformed %>%
      filter(company %in% selected_company, data_year == year, energy_reporting_scope == "Single Data Center") %>%
      # filter(company == "NTT Group", data_year == year, energy_reporting_scope == "Single Data Center") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    # create a sub data frame that is filtered by data year and multiple data centers scope
    company_MDC <- data_sheet_energy_transformed %>%
      filter(company %in% selected_company, data_year == year, energy_reporting_scope == "Multiple Data Centers") %>%
      # filter(company == "NTT Group", data_year == year, energy_reporting_scope == "Multiple Data Centers") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type)
    
    # create a sub data frame that is filtered by data year / company wide electricity scope
    company_CW <- data_sheet_energy_transformed %>%
      filter(company %in% selected_company, data_year == year, energy_reporting_scope == "Total Operations") %>%
      # filter(company == "NTT Group", data_year == year, energy_reporting_scope == "Total Operations") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    company_EN <- data_sheet_energy_transformed %>%
      filter(company %in% selected_company, data_year == year, fuel_1_type == "Total Energy Use") %>%
      # filter(company == "NTT Group", data_year == year, fuel_1_type == "Total Energy Use") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    if (year == 2007) {
      company_transparency <- rbind(company_SDC, company_MDC, company_CW, company_EN)
    } else {
      company_transparency <- company_transparency %>% rbind(company_SDC, company_MDC, company_CW, company_EN)
    }
  }
  
  timeline <- 2007:as.integer(max(na.omit(data_sheet_energy_transformed$data_year)))
  missing_years <- which(!(timeline %in% company_transparency$data_year))
  if (length(missing_years) != 0) {
    for (i in 1:length(missing_years)) {
      company_transparency[nrow(company_transparency)+1, ] <- c(company_transparency[1,1], missing_years[i]+2006, "", "")
    }
    company_transparency <- company_transparency[order(company_transparency[,"data_year"]), ]
  }

  year_axis <- company_transparency %>% distinct(company, data_year, .keep_all = TRUE)
  year_axis$energy_reporting_scope[year_axis$energy_reporting_scope == "Single Data Center"] <- "Reported Data\nCenter Electricity"
  year_axis$energy_reporting_scope[year_axis$energy_reporting_scope == "Multiple Data Centers"] <- "Reported Data\nCenter Electricity"
  year_axis$energy_reporting_scope[year_axis$energy_reporting_scope == "Total Operations"] <- "Reported Company\nWide Electricity"
  year_axis$energy_reporting_scope[year_axis$fuel_1_type == "Total Energy Use"] <- "Reported Company\nWide Energy"
  year_axis$energy_reporting_scope[year_axis$energy_reporting_scope == ""] <- "No Reporting\nof Publicly\nAvailable Data"
  if (year_axis$energy_reporting_scope[year_axis$data_year == max(na.omit(data_sheet_energy_transformed$data_year))] == "No Reporting\nof Publicly\nAvailable Data") {
    year_axis$energy_reporting_scope[year_axis$data_year == max(na.omit(data_sheet_energy_transformed$data_year))] <- "Pending Data\nSubmission"
  }
  year_axis <- year_axis %>% select(-c(company, fuel_1_type))
  
  company_transparency <- company_transparency %>% distinct(company, data_year, .keep_all = TRUE)
  company_transparency$energy_reporting_scope[company_transparency$energy_reporting_scope == "Single Data Center"] <- "Reported Data\nCenter Electricity"
  company_transparency$energy_reporting_scope[company_transparency$energy_reporting_scope == "Multiple Data Centers"] <- "Reported Data\nCenter Electricity"
  company_transparency$energy_reporting_scope[company_transparency$energy_reporting_scope == "Total Operations"] <- "Reported Company\nWide Electricity"
  company_transparency$energy_reporting_scope[company_transparency$fuel_1_type == "Total Energy Use"] <- "Reported Company\nWide Energy"
  company_transparency$energy_reporting_scope[company_transparency$energy_reporting_scope == ""] <- "No Reporting\nof Publicly\nAvailable Data"
  company_transparency$energy_reporting_scope[company_transparency$energy_reporting_scope == "No Reporting\nof Publicly\nAvailable Data" & company_transparency$data_year == max(na.omit(data_sheet_energy_transformed$data_year))] <- "Pending Data\nSubmission"
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
      company_transparency[i,3] <- 1.0
      company_transparency[i,4] <- 1.0
    } else if (i %% 3 == 1) {
      company_transparency[i,3] <- 0.2
      company_transparency[i,4] <- 0.2
    } else if (i %% 3 == 2) {
      company_transparency[i,3] <- 0.6
      company_transparency[i,4] <- 0.6
    }
  }
  
  status_levels <- c("Reported Data\nCenter Electricity", "Reported Company\nWide Electricity",
                     "Reported Company\nWide Energy", "No Reporting\nof Publicly\nAvailable Data", "Pending Data\nSubmission")
  status_colors <- c("#0070C0", "#00B050", "#FFC000", "#C00000", "#B88C8C")

  year_axis$energy_reporting_scope <- factor(year_axis$energy_reporting_scope, levels=status_levels, ordered=TRUE)
  year_axis$year_text <- as.character(year_axis$data_year)
  
  ggplot(data=year_axis, aes(x=data_year, y=0, color=energy_reporting_scope)) +
    geom_hline(yintercept=0, color="black", size=0.3) +
    geom_point(data=year_axis, aes(y=0), size=3) +
    geom_segment(data=company_transparency, aes(y=position,yend=0,xend=data_year), color='black', size=0.2) +
    geom_label(data=company_transparency, aes(y=text_position, label=energy_reporting_scope), fill="white", size=5, vjust="inward", label.padding = unit(0.6,"lines")) +
    geom_text(data=year_axis, aes(x=data_year, y=-0.05, label=year_text), color="black", size=6) +
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