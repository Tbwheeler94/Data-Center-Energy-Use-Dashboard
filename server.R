source(here("R", "aggregateOutput.R"))
source(here("R", "companyfuelOutput.R"))
source(here("R", "yearsreportingOutput.R"))
source(here("R", "companiesreportingOutput.R"))
source(here("ui.R"))

#Server code
server <- function(input, output, session) {
  
  #Tab 1
  output$years_reported <- renderUI({
    years_reported <- buildyearsreportedOutput(by_fuel_type_data)}
  )
  
  output$companies_reporting <- renderUI({
    buildcompaniesreportingOutput(by_fuel_type_data)}
  )
  
  output$energy_reported <- renderText({
    energyusereportedOutput(aggregate_data)}
  )
  
  #Tab 2: Industry Trends
  
  #Tab 3: Company Analysis
  
  #Filter company profiles page by selected company name
  
  data_sheet_selected_company <- reactive({
    data_sheet_company %>% filter(company_name == input$selected_company)
  })
  
  #External Service Providers
  
  output$external_service_provider_1 <- renderText({
    data_sheet_selected_company()[1, "provider_1"]
  })
  
  output$external_service_provider_2 <- renderText({
    data_sheet_selected_company()[1, "provider_2"]
  })
  
  output$external_service_provider_3 <- renderText({
    data_sheet_selected_company()[1, "provider_3"]
  })
  
  output$external_service_provider_4 <- renderText({
    data_sheet_selected_company()[1, "provider_4"]
  })
  #
  #output$external_service_provider_5 <- renderText({
  #  data_sheet_selected_company()[1, "provider_5"]
  #})
  
  #Data Center Overview Tab
  output$company_data_center_overview <- renderText({
    data_sheet_selected_company()[1, "company_data_center_overview"]
  })
  
  #Energy Reporting Assessment Tab
  output$energy_reporting_assessment <- renderText({
    data_sheet_selected_company()[1, "energy_reporting_assessment"]
  })
  
  # Company profile output
  #output$companyfuelPlot <- renderPlot({
  #  buildcompanyfuelOutput(by_fuel_type_data, input$company_selection)
  #})
  
  router$server(input, output, session)
}

##################
##Return to Code##
##################

# # Aggregate plot output
# output$aggregatePlot <- renderPlot({
#   buildAggregateOutput(aggregate_data)
# })
# 
# # Company profile output
# output$companyfuelPlot <- renderPlot({
# 
#   buildcompanyfuelOutput(by_fuel_type_data, input$company_selection)
# 
# })
# 
# # Years reported output
# output$years_reported <- renderText({buildyearsreportedOutput(by_fuel_type_data)})
# 
# # Companies reporting output
# output$companies_reporting <- renderText({buildcompaniesreportingOutput(by_fuel_type_data)})
# 
# #Energy use reported
# output$energy_reported <- renderText({energyusereportedOutput(aggregate_data)})