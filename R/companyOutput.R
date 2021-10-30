library(tidyverse)
library(lubridate)

buildCompanyOutput <- function(data_sheet) {
  
  # Stack bar graph output
  ggplot(data = data_sheet, aes(fill=company, y=value, x=data_year)) + 
    geom_bar(stat="identity") +
    theme(legend.position="none")
  
}