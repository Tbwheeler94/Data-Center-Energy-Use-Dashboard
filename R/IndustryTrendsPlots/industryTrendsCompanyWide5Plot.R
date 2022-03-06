buildIndustryTrendsCompanyWide5Plot <- function(energy_use_L2_5) {
  
  plot_ly() %>% 
    add_trace(x=energy_use_L2_5$electricity_converted, 
              y=reorder(energy_use_L2_5$company, energy_use_L2_5$electricity_converted),
              type="bar", width = 0.5, marker = list(color = "rgb(128,0,128)")) 
  
}