library(here)
library(janitor)
library(tidyverse)
library(lubridate)
library(shiny)

data_sheet <- read.csv("practice_data.csv")

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

ui <- htmlTemplate(filename = "www/index.html",
                   company_graph = plotOutput(outputId = "companyPlot"))

## Run app
shinyApp(ui, server)