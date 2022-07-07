buildIndustryTrendsCompanyWide2Plot <- function(energy_use_L2_2) {
  
  ggplot(data = energy_use_L2_2) +
    geom_bar(mapping = aes(x = electricity_converted/1000000, y=reorder(energy_use_L2_2$company, energy_use_L2_2$electricity_converted)),
             stat = 'identity',
             fill = '#ff7f50') +
    scale_x_continuous(breaks = c(0,25,50,75,100), 
                       label = c("0", "25 GWh", "50 GWh", "75 GWh", "100 GWh"), 
                       position = 'top',
                       expand = expansion(mult=c(0.01,0))) +
    theme_classic() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y= element_text(size = 12))
  
}

#plot_ly() %>% 
#  add_trace(x=energy_use_L2_2$electricity_converted, 
#            y=reorder(energy_use_L2_2$company, energy_use_L2_2$electricity_converted), 
#            type="bar", width = 0.5, marker = list(color = "rgb(255,127,80)")) 