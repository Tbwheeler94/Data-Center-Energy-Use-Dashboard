source(here("R", "aggregateOutput.R"))
source(here("R", "companyfuelOutput.R"))
source(here("R", "yearsreportingOutput.R"))
source(here("R", "companiesreportingOutput.R"))


#Server code
server <- function(input, output) {
  
  # Aggregate plot output
  output$aggregatePlot <- renderPlot({
    buildAggregateOutput(aggregate_data)
  })
  
  # Company profile output
  output$companyfuelPlot <- renderPlot({ 
    buildcompanyfuelOutput(by_fuel_type_data, input$company_selection)
  })
  
  # Years reported output
  output$years_reported <- renderText({buildyearsreportedOutput(by_fuel_type_data)})
  
  # Companies reporting output
  output$companies_reporting <- renderText({buildcompaniesreportingOutput(by_fuel_type_data)})
  
  #Energy use reported
  output$energy_reported <- renderText({energyusereportedOutput(aggregate_data)})
  
}