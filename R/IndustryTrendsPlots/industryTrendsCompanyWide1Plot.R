buildIndustryTrendsCompanyWide1Plot <- function(energy_use_L2_1) {
  
  plot_ly() %>% 
    add_trace(x=energy_use_L2_1$electricity_converted, 
              y=reorder(energy_use_L2_1$company, energy_use_L2_1$electricity_converted), 
              type="bar", width = 0.5, marker = list(color = "rgb(255,127,80)")) 
  
}