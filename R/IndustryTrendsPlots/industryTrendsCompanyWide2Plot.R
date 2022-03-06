buildIndustryTrendsCompanyWide2Plot <- function(energy_use_L2_2) {
  
  plot_ly() %>% 
    add_trace(x=energy_use_L2_2$electricity_converted, 
              y=reorder(energy_use_L2_2$company, energy_use_L2_2$electricity_converted), 
              type="bar", width = 0.5, marker = list(color = "rgb(255,127,80)")) 
  
}