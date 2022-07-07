buildIndustryTrendsCompanyWide1Plot <- function(energy_use_L2_1) {
  
  ggplot(data = energy_use_L2_1) +
    geom_bar(mapping = aes(x = electricity_converted/1000000, y=reorder(energy_use_L2_1$company, energy_use_L2_1$electricity_converted)),
             stat = 'identity',
             fill = '#fff703') +
    scale_x_continuous(position = 'top', expand = expansion(mult=c(0.01,0))) +
    theme_classic() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y= element_text(size = 12))
  
}

#plot_ly() %>% 
#  add_trace(x=energy_use_L2_1$electricity_converted, 
#            y=reorder(energy_use_L2_1$company, energy_use_L2_1$electricity_converted), 
#            type="bar", width = 0.5, marker = list(color = "rgb(255,127,80)"))