buildcompanyfuelOutput <- function(by_fuel_type_data, company_selection) {
  
  graph_fuel_data <- reactive({ by_fuel_type_data %>% 
      filter(company == company_selection) })
  
  # Stack bar graph output
  ggplot(data = graph_fuel_data(), aes(fill=fuel_type, y=value, x=data_year)) + 
    geom_bar(position="stack", stat="identity")
  
}
