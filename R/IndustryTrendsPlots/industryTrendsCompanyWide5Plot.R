

buildIndustryTrendsCompanyWide5Plot <- function(energy_use_L2_5) {
  
  ggplot(data = energy_use_L2_5) +
    geom_bar(mapping = aes(x = electricity_converted/1000000000, y=reorder(energy_use_L2_5$company, energy_use_L2_5$electricity_converted)),
             stat = 'identity',
             fill = '#800080') +
    scale_x_continuous(breaks = c(0,5,10,15,20), 
                       label = c("0", "5 TWh", "10 TWh", "15 TWh", "20 TWh"), 
                       position = 'top',
                       expand = expansion(mult=c(0.01,0))) +
    theme_classic() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y= element_text(size = 12))
  
}

#plot_ly() %>% 
#  add_trace(x=energy_use_L2_5$electricity_converted, 
#            y=reorder(energy_use_L2_5$company, energy_use_L2_5$electricity_converted),
#            type="bar", width = 0.5, marker = list(color = "rgb(128,0,128)")) 