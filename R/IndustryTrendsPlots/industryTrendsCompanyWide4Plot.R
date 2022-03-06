buildIndustryTrendsCompanyWide4Plot <- function(energy_use_L2_4) {
  
  plot_ly() %>% 
    add_trace(x=energy_use_L2_4$electricity_converted, 
              y=reorder(energy_use_L2_4$company, energy_use_L2_4$electricity_converted),
              type="bar", width = 0.5, marker = list(color = "rgb(0,191,255)")) 
  
}