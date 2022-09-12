buildIndustryTrendsEnergyDataPlot <- function(selected_year, selected_scope, selected_scale) {
  # prep plot details from selected scale
  plot_scale <- 0
  plot_breaks <- 0
  plot_labels <- ""
  if ("Up to 500 GWh" %in% selected_scale) {
    plot_scale <- 500000000
    plot_breaks <- c(0, 100000000, 200000000, 300000000, 400000000, 500000000)
    plot_labels <- c("0", "100 GWh", "200 GWh", "300 GWh", "400 GWh", "500 GWh")
  }
  if ("Up to 1 TWh" %in% selected_scale) {
    plot_scale <- 1000000000
    plot_breaks <- c(0, 200000000, 400000000, 600000000, 800000000, 1000000000)
    plot_labels <- c("0", "200 GWh", "400 GWh", "600 GWh", "800 GWh", "1 TWh")
  }
  if ("Up to 10 TWh" %in% selected_scale) {
    plot_scale <- 10000000000
    plot_breaks <- c(0, 1000000000, 2000000000, 3000000000, 4000000000, 5000000000, 
                     6000000000, 7000000000, 8000000000, 9000000000, 10000000000)
    plot_labels <- c("0", "1 TWh", "2 TWh", "3 TWh", "4 TWh", "5 TWh", "6 TWh", 
                     "7 TWh", "8 TWh", "9 TWh", "10 TWh")
  }
  if ("10+ TWh" %in% selected_scale) {
    plot_scale <- 30000000000
    plot_breaks <- c(0, 5000000000, 10000000000, 15000000000, 20000000000, 25000000000, 30000000000)
    plot_labels <- c("0", "5 TWh", "10 TWh", "15 TWh", "20 TWh", "25 TWh", "30 TWh")
  }
  
  # filter based on reactive inputs
  energy_use_final <- energy_use_final %>%
    filter(data_year %in% selected_year, energy_reporting_scope %in% selected_scope, 
           electricity_converted <= plot_scale)
  
  # change dc_and_cw value if both a company's data center and company wide data fit in the selected scale
  energy_use_final$dc_and_cw[energy_use_final$dc_and_cw == "Yes but CW at different scale" & !(energy_use_final$scale %in% selected_scale)] <- "Yes"
  
  # factor energy reporting scope values to assign colors properly in plot generation
  energy_use_final$energy_reporting_scope <- factor(energy_use_final$energy_reporting_scope, 
                                                        levels=c("Data Centers",
                                                                 "Company Wide"))
  
  dc_color <- c("#3BCA6D")
  cw_color <- c("#77945C")
  dc_and_cw <- c(`Data Centers` = "#3BCA6D", `Company Wide` = "#77945C")
  color_labels <- ""
  if ("Data Centers" %in% selected_scope && "Company Wide" %in% selected_scope) {
    color_labels <- dc_and_cw
    dc_and_cw_same_scale <- energy_use_final %>% filter(energy_reporting_scope == "Data Centers", dc_and_cw == "Yes")
    dc_and_cw_diff_scale <- energy_use_final %>% filter(energy_reporting_scope == "Data Centers", dc_and_cw == "Yes but CW at different scale")
    data_centers_data <- energy_use_final %>% filter(dc_and_cw == "", energy_reporting_scope == "Data Centers")
    company_wide_data <- energy_use_final %>% filter(energy_reporting_scope == "Company Wide")
    return(ggplot() + 
      geom_bar(company_wide_data, mapping = aes(x=electricity_converted, y=company, fill=energy_reporting_scope), 
               position=position_stack(reverse = TRUE), stat="identity") +
      geom_bar(data_centers_data, mapping = aes(x=electricity_converted, y=company, fill=energy_reporting_scope),
               position=position_stack(reverse = TRUE), stat="identity") +
      geom_bar(dc_and_cw_diff_scale, mapping = aes(x=electricity_converted, y=company, fill=energy_reporting_scope),
               position=position_stack(reverse = TRUE), stat="identity") +
      geom_bar(dc_and_cw_same_scale, mapping = aes(x=electricity_converted, y=company, fill=energy_reporting_scope),
               position=position_stack(reverse = TRUE), stat="identity", width = 0.4) +
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
            axis.text.y=element_text(size = 12)))
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