library(tidyverse)
library(lubridate)

buildAggregateOutput <- function(aggregate_data) {
  
  # Stack bar graph output
  ggplot(data = aggregate_data, aes(fill=company, y=value, x=data_year)) + 
    geom_bar(stat="identity") +
    theme(legend.position="none")
  
}