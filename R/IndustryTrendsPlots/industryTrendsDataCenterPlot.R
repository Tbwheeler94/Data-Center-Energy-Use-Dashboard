buildIndustryTrendsDataCenterPlot <- function(energy_use_final) {
  
  if ("Data Centers" %in% energy_use_final$energy_reporting_scope){
    # create plot for data center electricity usage
    ggplot(energy_use_final, aes(x=electricity_converted)) + 
      geom_bar(aes(y=company, fill=level_of_ownership), 
               position=position_stack(reverse = TRUE), stat="identity") +
      theme_classic() +
      scale_fill_brewer(palette = "PuBu") +
      scale_y_discrete(position = "left") +
      scale_x_continuous() +
      theme(legend.position = "right") +
      xlab("Electricity Value (KWh)") +
      ylab("Companies") +
      labs(fill = "Level Of Ownership") 
  }
  
  else{
    ggplot() +
      theme_void() +
      geom_text(aes(0,0,label='No Data Reported for This Year')) +
      xlab(NULL)
  }
  
}