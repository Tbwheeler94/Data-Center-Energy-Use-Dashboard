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
  
  company_list_newest <- list()
  companies <- order(unique(data_sheet_company$company_name)) #order function doesn't work
  
  for (i in 1:length(unique(data_sheet_company$company_name))) {
    
    company_list_newest[[i]]<- list(key = {companies[i]}, 
                                    text = {companies[i]})
  }
  
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