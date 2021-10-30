library(here)
library(janitor)
library(tidyverse)
library(lubridate)
library(shiny)

data_sheet <- read.csv(here('data', 'practice_data.csv'))


ui <- htmlTemplate(filename = "www/index.html",
                   company_graph = plotOutput("companyPlot"))

#Server code
server <- function(input, output) {
  
  # Annual impact globe output
  output$companyPlot <- renderPlot({
    ggplot(data = data_sheet, aes(fill=company, y=value, x=data_year)) + 
      geom_bar(stat="identity")
  })
}

## Run app
shinyApp(ui, server)