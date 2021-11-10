energyusereportedOutput <- function(aggregate_data) {
  
  aggregate_data <- aggregate_data %>% 
    filter(grepl("Data Centers", company))
  
  paste(round(sum(aggregate_data$value)/1000000000, 1), "TWh")
  
}