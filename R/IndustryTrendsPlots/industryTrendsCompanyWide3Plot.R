buildIndustryTrendsCompanyWide3Plot <- function(energy_use_L2_3) {
  
  ggplot(data = energy_use_L2_3) +
    geom_bar(mapping = aes(x = electricity_converted, y=reorder(energy_use_L2_3$company, energy_use_L2_3$electricity_converted)),
             stat = 'identity',
             fill = '#00fa9a') +
    scale_x_continuous(position = 'top', 
                       expand = expansion(mult = c(0, 0.05)), 
                       labels = scales::label_number_si()) +
    theme_classic() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y= element_text(size = 12))
}

#plot_ly() %>% 
#  add_trace(x=energy_use_L2_3$electricity_converted, 
#            y=reorder(energy_use_L2_3$company, energy_use_L2_3$electricity_converted),
#            type="bar", width = 0.8, marker = list(color = "rgb(0,250,154)"))