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
  
  ########################################################
  ###### Generate reactive datasets for sub-rendering ####
  ########################################################
  
  company_sheet_selected_company <- reactive({
    data_sheet_company %>% filter(company_name == input$selected_company)
  })
  
  #This dataset is used to check for SASB, GRI, and CDP reporting
  energy_sheet_selected_company_current_year <- reactive({
    data_sheet_energy %>% filter(company == input$selected_company) %>% 
    mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
    filter(period_covered_start_date == max(period_covered_start_date))
  })
  
  #This dataset is use to check if a company is reporting data center electricity use
  energy_sheet_selected_company_current_year_dc_electricity <- reactive({
    data_sheet_energy %>% filter(company == input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers") %>% 
      drop_na(electricity_value)
  })
  
  #This dataset is use to check if a company is reporting data center fuel use
  energy_sheet_selected_company_current_year_dc_fuel <- reactive({
    data_sheet_energy %>% filter(company == input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers") %>% 
      drop_na(fuel_1_value)
  })
  
  #This dataset is use to check if a company is reporting company-wide electricity use
  energy_sheet_selected_company_current_year_te <- reactive({
    data_sheet_energy %>% filter(company == input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Total Operations") %>% 
      drop_na(electricity_value)
  })
  
  ########################################################
  ############### Render datatables for UI ###############
  ########################################################
  
  #Selected company stats
  
  output$selected_company_stats <- renderDataTable({
    
  #Does Company X report any energy use?
    
  energy_reporting_status <- ifelse(sum(energy_sheet_selected_company_current_year()$electricity_value > 0, na.rm = TRUE) | sum(energy_sheet_selected_company_current_year()$fuel_1_value > 0, na.rm = TRUE), "Yes", "No")
  
  #Year of most recent data
  year_of_most_recent_data <- energy_sheet_selected_company_current_year()[1, "period_covered_start_date"]  
  
  #Render list of external service providers  
  external_service_provider_list <- paste(company_sheet_selected_company()[1, "provider_1"], company_sheet_selected_company()[1, "provider_2"],
    company_sheet_selected_company()[1, "provider_3"], company_sheet_selected_company()[1, "provider_4"],
    company_sheet_selected_company()[1, "provider_5"], company_sheet_selected_company()[1, "provider_6"],
    company_sheet_selected_company()[1, "provider_7"], company_sheet_selected_company()[1, "provider_8"],
    company_sheet_selected_company()[1, "provider_9"], company_sheet_selected_company()[1, "provider_10"],sep = ", ") %>% 
    str_remove_all(", NA") %>% str_remove_all(", ,") %>% str_remove_all(" , ") %>% str_remove_all("[:punct:]*\\s*$")
  
  selected_company_stats <- data.frame(A = c("Does company report energy use?", "Year of most recent data", "Lease/Cloud Providers"),
             B = c(energy_reporting_status, year_of_most_recent_data, ifelse(external_service_provider_list == "", "No External Providers Reported", external_service_provider_list)), check.names = FALSE)
  
  datatable(selected_company_stats, rownames = FALSE, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){",
                                                                                  "  $(thead).remove();",
                                                                                  "}")))
  })
  
  #Data Center Overview Box
  output$company_data_center_overview <- renderText({
    company_sheet_selected_company()[1, "company_data_center_overview"]
  })
  #Energy Reporting Assessment Box
  output$energy_reporting_assessment <- renderText({
    company_sheet_selected_company()[1, "energy_reporting_assessment"]
  })
  
  #Currently Reported Energy Use Levels Box
  
 # energy_sheet_selected_company_current_year_dc_fuel <- 
 #   data_sheet_energy %>% filter(company == "Google") %>% 
 #     mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
 #     filter(period_covered_start_date == max(period_covered_start_date)) %>% 
 #     filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers") %>% 
 #     drop_na(fuel_1_value)
 # 
 # energy_sheet_selected_company_current_year_dc_electricity <- 
 #   data_sheet_energy %>% filter(company == input$selected_company) %>% 
 #     mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
 #     filter(period_covered_start_date == max(period_covered_start_date)) %>% 
 #     filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers") %>% 
 #     drop_na(electricity_value)
 # 
 # energy_sheet_selected_company_current_year_te <- 
 #   data_sheet_energy %>% filter(company == "China Unicom") %>% 
 #     mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
 #     filter(period_covered_start_date == max(period_covered_start_date)) %>% 
 #     filter(energy_reporting_scope == "Total Operations") %>% 
 #     drop_na(electricity_value)
  
  output$reported_energy_levels <- renderDataTable({
    
    #options = list(pageLength = 10)
    
    data_center_electricity_reporting_status <- ifelse(sum(energy_sheet_selected_company_current_year_dc_electricity()$electricity_value > 0), "Yes", "No")
    data_center_self_managed_reporting_status <- ifelse("Self-managed" %in% energy_sheet_selected_company_current_year()$level_of_ownership, "Yes", "No")
    data_center_leased_reporting_status <- ifelse("Leased" %in% energy_sheet_selected_company_current_year()$level_of_ownership, "Yes", "No")
    data_center_cloud_reporting_status <- ifelse("Cloud" %in% energy_sheet_selected_company_current_year()$level_of_ownership, "Yes", "No")
    data_center_other_fuel_use_reporting_status <- ifelse(sum(energy_sheet_selected_company_current_year_dc_fuel()$fuel_1_value > 0), "Yes", "No")
    total_energy_reporting_status <- ifelse(sum(energy_sheet_selected_company_current_year_te()$electricity_value > 0), "Yes", "No")
    
    reported_energy_levels_data <- data.frame(Level = c("Data center electricity use", "Self-managed", "Leased", "Cloud", "Data center other fuel use", "Company-wide electricty use"),
               " Reporting Status " = c(data_center_electricity_reporting_status, data_center_self_managed_reporting_status, data_center_leased_reporting_status, data_center_cloud_reporting_status, data_center_other_fuel_use_reporting_status, total_energy_reporting_status), check.names = FALSE)
    
    datatable(reported_energy_levels_data, options = list(dom = 't'))
  })
  
  #Data Standards Box
  output$data_standards <- renderDataTable({
    
    #options = list(pageLength = 10)
    
    sasb_reporting_status <- ifelse("Yes" %in% energy_sheet_selected_company_current_year()$sasb, "Yes", "No")
    cdp_reporting_status <- ifelse("Yes" %in% energy_sheet_selected_company_current_year()$cdp, "Yes", "No")
    gri_reporting_status <- ifelse("Yes" %in% energy_sheet_selected_company_current_year()$gri, "Yes", "No")
    
    data_standards_data <- data.frame(Organization = c("SASB", "GRI", "CDP"),
               " Reporting Status " = c(sasb_reporting_status, cdp_reporting_status, gri_reporting_status), check.names = FALSE)
    
    datatable(data_standards_data, options = list(dom = 't'))
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