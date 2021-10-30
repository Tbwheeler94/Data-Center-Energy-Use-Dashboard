library(here)
library(janitor)
library(tidyverse)
library(lubridate)
library(shiny)

## Sources
#source("ui.R")
#source("server.R")

## Source files
#source(here("R", "companyOutput.R"))

data_sheet <- read.csv(here('data', 'practice_data.csv'))


ui <- htmlTemplate(filename = "www/index.html",
                   company_graph = plotOutput(outputId = "companyPlot"))

#Server code
server <- function(input, output) {
  
  # Annual impact globe output
  output$companyPlot <- renderPlot({
    p1 <- ggplot(data = data_sheet, aes(fill=company, y=value, x=data_year)) + 
      geom_bar(stat="identity") +
      theme(legend.position="none")
    
    p1
  })
}

## Run app
shinyApp(ui, server)