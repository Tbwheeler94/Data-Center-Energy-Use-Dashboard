buildIndustryTrendsEnergyDataPlot <- function(data_sheet_energy_transformed, selected_year, selected_scope, selected_scale) {
  plot_scale <- 0
  plot_TWH <- ""
  plot_breaks <- 0
  plot_labels <- ""
  if ("Up to 500 GWh" %in% selected_scale) {
    plot_scale <- 500000000
    plot_TWH <- "Below"
    plot_breaks <- c(0, 100000000, 200000000, 300000000, 400000000, 500000000)
    plot_labels <- c("0", "100 GWh", "200 GWh", "300 GWh", "400 GWh", "500 GWh")
  }
  if ("Up to 1 TWh" %in% selected_scale) {
    plot_scale <- 1000000000
    plot_TWH <- "Below"
    plot_breaks <- c(0, 200000000, 400000000, 600000000, 800000000, 1000000000)
    plot_labels <- c("0", "200 GWh", "400 GWh", "600 GWh", "800 GWh", "1 TWh")
  }
  if ("Up to 10 TWh" %in% selected_scale) {
    plot_scale <- 10000000000
    plot_TWH <- "At or Above"
    plot_breaks <- c(0, 1000000000, 2000000000, 3000000000, 4000000000, 5000000000, 
                     6000000000, 7000000000, 8000000000, 9000000000, 10000000000)
    plot_labels <- c("0", "1 TWh", "2 TWh", "3 TWh", "4 TWh", "5 TWh", "6 TWh", 
                     "7 TWh", "8 TWh", "9 TWh", "10 TWh")
  }
  if ("10+ TWh" %in% selected_scale) {
    plot_scale <- 10000000000
    plot_TWH <- "At or Above"
    plot_breaks <- c(0, 1000000000, 2000000000, 3000000000, 4000000000, 5000000000, 
                     6000000000, 7000000000, 8000000000, 9000000000, 10000000000)
    plot_labels <- c("0", "1 TWh", "2 TWh", "3 TWh", "4 TWh", "5 TWh", "6 TWh", 
                     "7 TWh", "8 TWh", "9 TWh", "10 TWh")
  }
  
  # for (year in 2007:as.integer(max(na.omit(data_sheet_energy_transformed$data_year)))) {
  #   # create a sub data frame that is filtered by data year and single data center scope
  #   energy_use_SDC <- data_sheet_energy_transformed %>%
  #     filter(data_year == year, energy_reporting_scope == "Single Data Center") %>%
  #     select(company, data_year, energy_reporting_scope, level_of_ownership, electricity_converted) 
  #   
  #   # create a sub data frame that is filtered by data year and multiple data centers scope
  #   energy_use_MDC <- data_sheet_energy_transformed %>%
  #     filter(data_year == year, energy_reporting_scope == "Multiple Data Centers") %>%
  #     select(company, data_year, energy_reporting_scope, level_of_ownership, electricity_converted)
  #   
  #   company_DC <- c(energy_use_SDC$company, energy_use_MDC$company)
  #   company_DC <- company_DC %>% unique()
  #   
  #   # create a sub data frame that is filtered by data year / company wide electricity scope
  #   energy_use_TO <- data_sheet_energy_transformed %>%
  #     filter(data_year == year, energy_reporting_scope == "Total Operations") %>%
  #     select(company, data_year, energy_reporting_scope, level_of_ownership, electricity_converted) 
  #   
  #   energy_use_TO <- energy_use_TO[!(energy_use_TO$company %in% company_DC),]
  #   
  #   if (year == 2007) {
  #     energy_use_graph <- rbind(energy_use_SDC, energy_use_MDC, energy_use_TO)
  #   } else {
  #     energy_use_graph <- energy_use_graph %>% rbind(energy_use_SDC, energy_use_MDC, energy_use_TO)
  #   }
  # }
  # 
  # # create another data frame that is filtered by company founding year
  # data_founding_year <- data_sheet_company_raw %>% select(company_name, 
  #                                                         checked_back_to_founding_year_or_2007,
  #                                                         company_founding_year)
  # # drop all the excess rows that have NA data
  # data_founding_year <- na.omit(data_founding_year)
  # data_founding_year <- data_founding_year[data_founding_year$checked_back_to_founding_year_or_2007=="Yes", ]
  # energy_use_graph <- subset(energy_use_graph, company %in% data_founding_year$company_name) 
  # 
  # energy_use_graph$energy_reporting_scope[energy_use_graph$energy_reporting_scope == "Single Data Center"] <- "Data Centers"
  # energy_use_graph$energy_reporting_scope[energy_use_graph$energy_reporting_scope == "Multiple Data Centers"] <- "Data Centers"
  # energy_use_graph$energy_reporting_scope[energy_use_graph$energy_reporting_scope == "Total Operations"] <- "Company Wide"
  # 
  # # stack single data center/multiple data center data frames on top of each other
  # energy_use_graph <- energy_use_graph[!(energy_use_graph$energy_reporting_scope == "Data Centers" && energy_use_graph$level_of_ownership == ""),]
  # energy_use_graph <- na.omit(energy_use_graph)
  # energy_use_final <- energy_use_graph %>%
  #   group_by(company,data_year,energy_reporting_scope,level_of_ownership) %>%
  #   dplyr::summarise(electricity_converted = sum(electricity_converted)) %>%
  #   as.data.frame()
  # energy_use_final <- energy_use_final %>%
  #   group_by(company,data_year,energy_reporting_scope,level_of_ownership)
  # 
  # energy_use_final$TWH_level <- ""
  # 
  # for (i in 1:nrow(energy_use_final)) {
  #   if (i != nrow(energy_use_final) 
  #       && energy_use_final[i,"company"] == energy_use_final[i+1,"company"]
  #       && energy_use_final[i,"data_year"] == energy_use_final[i+1,"data_year"]) {
  #     if (energy_use_final[i,"electricity_converted"] + energy_use_final[i+1,"electricity_converted"] < 1000000000) {
  #       energy_use_final[i,"TWH_level"] <- "Below"
  #       energy_use_final[i+1,"TWH_level"] <- "Below"
  #     } else {
  #       energy_use_final[i,"TWH_level"] <- "At or Above"
  #       energy_use_final[i+1,"TWH_level"] <- "At or Above"
  #     }
  #   } else if (energy_use_final[i,"TWH_level"] == "" && energy_use_final[i,"electricity_converted"] < 1000000000) {
  #     energy_use_final[i,"TWH_level"] <- "Below"
  #   } else if (energy_use_final[i,"TWH_level"] == "" && energy_use_final[i,"electricity_converted"] >= 1000000000) {
  #     energy_use_final[i,"TWH_level"] <- "At or Above"
  #   }
  # }
  
  energy_use_final <- energy_use_final %>%
    filter(data_year %in% selected_year, energy_reporting_scope %in% selected_scope, 
           electricity_converted <= plot_scale)
  
  if (plot_TWH == "Below") {
    energy_use_final <- filter(energy_use_final, TWH_level == plot_TWH)
  }
  
  energy_use_final$energy_reporting_scope <- factor(energy_use_final$energy_reporting_scope, 
                                                        levels=c("Data Centers",
                                                                 "Company Wide"))
  
  dc_color <- c("#3BCA6D")
  cw_color <- c("#77945C")
  dc_and_cw <- c("#3BCA6D", "#77945C")
  color_labels <- ""
  if ("Data Centers" %in% selected_scope && "Company Wide" %in% selected_scope) {
    color_labels <- dc_and_cw
  } else if ("Data Centers" %in% selected_scope) {
    color_labels <- dc_color
  } else if ("Company Wide" %in% selected_scope) {
    color_labels <- cw_color
  }
  
  ggplot(energy_use_final, aes(x=electricity_converted)) + 
    geom_bar(aes(y=company, fill=energy_reporting_scope), 
             position=position_stack(reverse = TRUE), stat="identity") +
    scale_x_continuous(breaks = plot_breaks, 
                       label = plot_labels,
                       position='top', 
                       expand = expansion(mult=c(0.01,0))) + 
    scale_fill_manual(values=color_labels) +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          legend.text=element_text(size = 14),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_text(size = 12))
}