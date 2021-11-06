source(here("R", "aggregateOutput.R"))
source(here("R", "companyfuelOutput.R"))

#Server code
server <- function(input, output) {
  
  # Annual impact globe output
  output$aggregatePlot <- renderPlot({
    buildAggregateOutput(aggregate_data)
  })
  
  output$companyfuelPlot <- renderPlot({ 
    buildcompanyfuelOutput(by_fuel_type_data, input$company_selection)
  })
}