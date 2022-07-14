buildIndustryTrendsDataCenter1Plot <- function(energy_use_final) {
  # install.packages('gdata')
  # library(gdata)
  
  if ("Data Centers" %in% energy_use_final$energy_reporting_scope){
    # create plot for data center electricity usage
    ggplot(energy_use_final, aes(x=electricity_converted)) + 
      geom_bar(aes(y=company, fill=level_of_ownership), 
               position=position_stack(reverse = TRUE), stat="identity") +
      theme_classic() +
      scale_fill_brewer(palette = "Greens") +
      scale_y_discrete(position = "left") +
      scale_x_continuous(breaks = c(0, 200000000, 400000000,600000000, 800000000), 
                         label = c("0", "200 GWh", "400 GWh", "600 GWh", "800 GWh"), 
                         position = 'top',
                         expand = expansion(mult=c(0.01,0))) + 
                         #breaks = energy_use_final$electricity_converted, 
                         #expand = expansion(mult = c(0, 0.05)), 
                         #labels = humanReadable(energy_use_final$electricity_converted, standard = "Unix", sep = "")) +
                         #labels = scales::label_number_si()) +
      theme(legend.position = "right",
            legend.title=element_text(size = 18),
            legend.text=element_text(size = 16),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_text(size = 12)) +
      labs(fill = "Level Of Ownership") 
  }
  
  else{
    ggplot() +
      theme_void() +
      geom_text(aes(0,0,label='No Data Reported for This Year'), size=7) +
      xlab(NULL)
  }
  
}