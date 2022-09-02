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