buildIndustryTrendsCompanyWide3Plot <- function(energy_use_L2_3) {
  
  plot_ly() %>% 
    add_trace(x=energy_use_L2_3$electricity_converted, 
              y=reorder(energy_use_L2_3$company, energy_use_L2_3$electricity_converted),
              type="bar", width = 0.8, marker = list(color = "rgb(0,250,154)"))
  
}