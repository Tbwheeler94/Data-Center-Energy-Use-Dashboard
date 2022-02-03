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
  
  #External Service Providers
  output$external_service_providers <- renderText({
    external_service_providers <- reactive({
      external_service_providers <- data_sheet_company %>% filter(company_name == input$selected_company)
      external_service_providers[1, "provider_1"]
    })
    external_service_providers()
  })
  
  #Data Center Overview Tab
  output$company_data_center_overview <- renderText({
    company_data_center_overview <- reactive({
      company_data_center_overview <- data_sheet_company %>% filter(company_name == input$selected_company)
      company_data_center_overview[1, "company_data_center_overview"]
    })
    company_data_center_overview()
  })
  
  #Energy Reporting Assessment Tab
  output$energy_reporting_assessment <- renderText({
    energy_reporting_assessment <- reactive({
      energy_reporting_assessment <- data_sheet_company %>% filter(company_name == input$selected_company)
      energy_reporting_assessment[1, "energy_reporting_assessment"]
    })
    energy_reporting_assessment()
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