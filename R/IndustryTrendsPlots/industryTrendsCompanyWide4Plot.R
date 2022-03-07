buildIndustryTrendsCompanyWide4Plot <- function(energy_use_L2_4) {
  
  ggplot(data = energy_use_L2_4) +
    geom_bar(mapping = aes(x = electricity_converted, y=reorder(energy_use_L2_4$company, energy_use_L2_4$electricity_converted)),
             stat = 'identity',
             fill = 'lightblue') +
    scale_x_continuous(position = 'top', 
                       expand = expansion(mult = c(0, 0.05)), 
                       labels = scales::label_number_si()) +
    theme_classic() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y= element_text(size = 12))
  
}

#plot_ly() %>% 
#  add_trace(x=energy_use_L2_4$electricity_converted, 
#            y=reorder(energy_use_L2_4$company, energy_use_L2_4$electricity_converted),
#            type="bar", width = 0.5, marker = list(color = "rgb(0,191,255)")) 