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
  
  #This dataset filters the raw company profiles sheet by the selected company's current year
  company_sheet_selected_company <- reactive({
    data_sheet_company_raw %>% filter(company_name == input$selected_company)
  })
  
  #This dataset filters the raw energy sheet by the selected company's current year
  energy_sheet_selected_company_current_year <- reactive({
    data_sheet_energy_raw %>% 
      filter(company == input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date))
  })
  
  #This dataset is used to check for SASB, GRI, CDP reporting
  sasb_cdp_gri_status <- reactive({
    
    energy_sheet_selected_company_current_year_scg <- data_sheet_energy_raw %>% 
      filter(company == input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date))
    
    sasb_reporting_status <- ifelse("Yes" %in% energy_sheet_selected_company_current_year_scg$sasb, "Yes", "No")
    cdp_reporting_status <- ifelse("Yes" %in% energy_sheet_selected_company_current_year_scg$cdp, "Yes", "No")
    gri_reporting_status <- ifelse("Yes" %in% energy_sheet_selected_company_current_year_scg$gri, "Yes", "No")
    
    data.frame(Organization = c("SASB", "GRI", "CDP"),
               " Reporting Status " = c(sasb_reporting_status, cdp_reporting_status, gri_reporting_status), check.names = FALSE)
  })
  
  #This dataset is used to check for PUE, WUE, Renewables reporting
  pue_wue_renewables_status <- reactive({
    
    energy_sheet_selected_company_current_year_pwr <- data_sheet_energy_raw %>% 
      filter(company == input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date))
    
    pue_reporting_status <- ifelse("Yes" %in% energy_sheet_selected_company_current_year_pwr$pue, "Yes", "No")
    wue_reporting_status <- ifelse("Yes" %in% energy_sheet_selected_company_current_year_pwr$wue, "Yes", "No")
    renewable_reporting_status <- ifelse("Yes" %in% energy_sheet_selected_company_current_year_pwr$renewable_energy, "Yes", "No")
    
    data.frame(Metrics = c("PUE", "WUE", "Renewable Energy"),
                                      " Reporting Status " = c(pue_reporting_status, wue_reporting_status, renewable_reporting_status), check.names = FALSE)
    
  })
  
  #This dataset is use to check if a company is reporting data center electricity use
  energy_sheet_selected_company_current_year_dc_electricity <- reactive({
    data_sheet_energy_raw %>% filter(company == input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers") %>% 
      drop_na(electricity_value)
  })
  
  #This dataset is use to check if a company is reporting data center fuel use
  energy_sheet_selected_company_current_year_dc_fuel <- reactive({
    data_sheet_energy_raw %>% filter(company == input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers") %>% 
      drop_na(fuel_1_value)
  })
  
  #This dataset is used to check what level of data center management the company is reporting (filters out total operations rows as sometimes total operations rows are marked as "self-managed)
  energy_sheet_selected_company_current_year_dc <- reactive({
    data_sheet_energy_raw %>% filter(company == input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers")
  })
  
  #This dataset is use to check if a company is reporting company-wide electricity use
  energy_sheet_selected_company_current_year_te <- reactive({
    data_sheet_energy_raw %>% filter(company == input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Total Operations") %>% 
      drop_na(electricity_value)
  })
  
  #####################################################################
  ########## Render datatables, box text, and plots for UI ############
  #####################################################################
  
  ########################
  #Table 1################
  #Selected company stats#
  ########################
  
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
  
  ##############################
  #Box 2########################
  #Company data center overview#
  ##############################
  
  output$company_data_center_overview <- renderText({
    company_sheet_selected_company()[1, "company_data_center_overview"]
  })
  
  ##############################
  #Box 3########################
  #Energy reporting assessment##
  ##############################
  
  output$energy_reporting_assessment <- renderText({
    company_sheet_selected_company()[1, "energy_reporting_assessment"]
  })
  
  #######################################
  #Table 4###############################
  #Currently reported energy use levels##
  #######################################
  
  output$reported_energy_levels <- renderDataTable({
    
    data_center_electricity_reporting_status <- ifelse(sum(energy_sheet_selected_company_current_year_dc_electricity()$electricity_value > 0), "Yes", "No")
    data_center_self_managed_reporting_status <- ifelse("Self-managed" %in% energy_sheet_selected_company_current_year_dc()$level_of_ownership, "Yes", "No")
    data_center_leased_reporting_status <- ifelse("Leased" %in% energy_sheet_selected_company_current_year_dc()$level_of_ownership, "Yes", "No")
    data_center_cloud_reporting_status <- ifelse("Cloud" %in% energy_sheet_selected_company_current_year_dc()$level_of_ownership, "Yes", "No")
    data_center_other_fuel_use_reporting_status <- ifelse(sum(energy_sheet_selected_company_current_year_dc_fuel()$fuel_1_value > 0), "Yes", "No")
    total_energy_reporting_status <- ifelse(sum(energy_sheet_selected_company_current_year_te()$electricity_value > 0), "Yes", "No")
    
    reported_energy_levels_data <- data.frame(Level = c("Data center electricity use", "Self-managed", "Leased", "Cloud", "Data center other fuel use", "Company-wide electricty use"),
               " Reporting Status " = c(data_center_electricity_reporting_status, data_center_self_managed_reporting_status, data_center_leased_reporting_status, data_center_cloud_reporting_status, data_center_other_fuel_use_reporting_status, total_energy_reporting_status), check.names = FALSE)
    
    datatable(reported_energy_levels_data, rownames = FALSE, options = list(dom = 't'))
  })
  
  #######################################
  #Table 5###############################
  #Data standards reported###############
  #######################################
  
  #UI datatable output
  output$data_standards <- renderDataTable({
    datatable(sasb_cdp_gri_status(), rownames = FALSE, options = list(dom = 't'))
  })
  
  #UI downloadable CSV
  output$download_standards <- downloadHandler(
    filename = function(){"data_standards.csv"}, 
    content = function(fname){
      write.csv(sasb_cdp_gri_status(), fname)
    }
  )
  
  #######################################
  #Table 6###############################
  #Other metrics reported################
  #######################################
  
  #UI datatable output
  
  output$other_metrics <- renderDataTable({
    datatable(pue_wue_renewables_status(), rownames = FALSE, options = list(dom = 't'))
  })
  
  #######################################
  #Table 7###############################
  #Electricity Use (TWh/yr)##############
  #######################################
  
  #NOTE: Need to figure out scenarios where the code breaks (i.e. Mastercard displaying 0 for energy)
  #Format values in last row as a percent, move "Total Company" row to the correct location, determine how many decimals to show
  
  selected_company_electricity_use <- reactive({
    data_sheet_energy_transformed %>% 
      filter(company == input$selected_company) %>% 
      mutate_at(vars(electricity_converted), ~replace_na(., 0)) %>%
      filter(electricity_converted != 0) %>% 
      select("data_year", "energy_reporting_scope", "level_of_ownership", "electricity_converted") %>% 
      mutate(energy_reporting_scope = case_when(
        energy_reporting_scope %in% c("Multiple Data Centers", "Single Data Center") ~ "Data center electricity use",
        energy_reporting_scope %in% "Total Operations"                               ~ "Company-wide electricity use")) %>% 
      group_by(data_year, energy_reporting_scope, level_of_ownership) %>% 
      summarize(value = sum(electricity_converted)) %>% 
      mutate(value = value/1000000000) %>%
      unite(energy_reporting_scope, energy_reporting_scope:level_of_ownership, sep = " | ") %>% 
      mutate(energy_reporting_scope = case_when(
        energy_reporting_scope %in% "Data center electricity use | Leased" ~ "Leased",
        energy_reporting_scope %in% "Data center electricity use | Cloud" ~ "Cloud",
        energy_reporting_scope %in% "Data center electricity use | Self-managed" ~ "Self_managed",
        energy_reporting_scope %in% "Data center electricity use | " ~ "Self_managed", #If no level of ownership is given, assume self managed
        energy_reporting_scope %in% "Data center electricity use | NA" ~ "Self_managed", #If no level of ownership is given, assume self managed
        energy_reporting_scope %in% "Company-wide electricity use | Self-managed" ~ "Total_company",
        energy_reporting_scope %in% "Company-wide electricity use | " ~ "Total_company",
        energy_reporting_scope %in% "Company-wide electricity use | NA" ~ "Total_company")) %>%
      pivot_wider(names_from = energy_reporting_scope, values_from = value) %>% 
      replace(is.na(.), 0) %>% 
      mutate(Leased = ifelse("Leased" %in% names(.), Leased, 0),
           Self_managed = ifelse("Self_managed" %in% names(.), Self_managed, 0),
           Total_company = ifelse("Total_company" %in% names(.), Total_company, 0)) %>%
      mutate(data_centers = Self_managed + Leased) %>% 
      mutate(data_center_percentage = (data_centers/(data_centers + Total_company))) %>%
      pivot_longer(!data_year, names_to = "category", values_to = "value") %>% 
      pivot_wider(names_from = data_year, values_from = value) %>% 
      mutate(category = case_when(
        category %in% "Total_company" ~ "Total Company",
        category %in% "Self_managed" ~ "Self-managed",
        category %in% "Leased" ~ "Leased",
        category %in% "data_centers" ~ "Data centers",
        category %in% "data_center_percentage" ~ "Data center % of total electricity")) %>% 
    mutate_if(is.numeric, ~round(., 3))
  })
  
  output$electricity_use_table <- renderDataTable({
    datatable(selected_company_electricity_use(), rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  #######################################
  #Table 9###############################
  #Other Fuel Use (TWh/yr)###############
  #######################################
  
  selected_company_fuel_use <- reactive({
    data_sheet_energy_transformed %>% 
      filter(company == input$selected_company) %>% 
      mutate_at(vars(electricity_converted, fuel_1_converted, #replace na values with 0
                     fuel_2_converted, fuel_3_converted, fuel_4_converted, 
                     fuel_5_converted), ~replace_na(., 0)) %>%
      rowwise() %>% 
      mutate(total_other_energy_use = sum(c(fuel_1_converted,
                                            fuel_2_converted, fuel_3_converted, fuel_4_converted, 
                                            fuel_5_converted))) %>% 
      select("data_year", "energy_reporting_scope", "level_of_ownership", "total_other_energy_use", "notes_3") %>% 
      filter(energy_reporting_scope == "Multiple Data Centers" | energy_reporting_scope == "Single Data Center") %>% 
      mutate(energy_reporting_scope = case_when(
        energy_reporting_scope %in% c("Multiple Data Centers", "Single Data Center") ~ "Data center other fuel use")) %>% 
      group_by(data_year, energy_reporting_scope, level_of_ownership) %>% 
      summarize(value = sum(total_other_energy_use)) %>% 
      mutate(value = value/1000000000) %>% 
      rename(c("Reporting Scope" = energy_reporting_scope, "Level of Ownership" = level_of_ownership, "Other Energy Use" = value, "Year" = data_year)) #%>% 
    #create pivot table
    #pivot_wider(names_from = data_year, values_from = value) %>% as.data.frame()
  })
  
  output$other_fuel_use_table <- renderDataTable({
    datatable(selected_company_fuel_use(), rownames = FALSE, options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20)))
  })
  
  #######################################
  #Table 10##############################
  #Non-specified energy use (TWh/yr)#####
  #######################################
  
  #selected_company_ns_energy_use <-
  #  data_sheet_energy_transformed %>%
  #    filter(company == input$selected_company, fuel_1_type == "Total Energy Use") %>% 
  #    select("data_year", "energy_reporting_scope", "fuel_1_type", "fuel_1_converted") %>% 
  #    rename(c("Year" = data_year, "Reporting Scope" = energy_reporting_scope, "Geographic Scope" = fuel_1_converted))
  
  #######################################
  #Table 11##############################
  #PUE###################################
  #######################################
  
  selected_company_pue <- reactive({
    data_sheet_pue_raw %>% 
      filter(company == input$selected_company) %>% 
      select("applicable_year", "facility_scope", "geographical_scope", "pue_value") %>% 
      rename(c("Year" = applicable_year, "Facility Scope" = facility_scope, "PUE Value" = pue_value, "Geographic Scope" = geographical_scope))
  })
  
  output$pue_table <- renderDataTable({
    datatable(selected_company_pue(), rownames = FALSE, options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20)))
  })
  
  #######################################
  #Table 12##############################
  #Sources Assessed######################
  #######################################
  
  #stack sources columns on top of each other
  source_assessed_1 <- data_sheet_energy_transformed %>% 
    filter(company == "Google") %>% 
    select(c("data_year", "report_1_type", "did_report_1_provide_electricity_or_fuel_use_data", "link_to_report_1_on_company_website")) %>% 
    rename(report_type = report_1_type, yes_no = did_report_1_provide_electricity_or_fuel_use_data, link = link_to_report_1_on_company_website)
  
  source_assessed_2 <- data_sheet_energy_transformed %>% 
    filter(company == "Google") %>% 
    select(c("data_year", "report_2_type", "did_report_2_provide_electricity_or_fuel_use_data", "link_to_report_2_on_company_website")) %>% 
    rename(report_type = report_2_type, yes_no = did_report_2_provide_electricity_or_fuel_use_data, link = link_to_report_2_on_company_website)
  
  source_assessed_3 <- data_sheet_energy_transformed %>% 
    filter(company == "Google") %>% 
    select(c("data_year", "report_3_type", "did_report_3_provide_electricity_or_fuel_use_data", "link_to_report_3_on_company_website")) %>% 
    rename(report_type = report_3_type, yes_no = did_report_3_provide_electricity_or_fuel_use_data, link = link_to_report_3_on_company_website)
  
  source_assessed_4 <- data_sheet_energy_transformed %>% 
    filter(company == "Google") %>% 
    select(c("data_year", "report_4_type", "did_report_4_provide_electricity_or_fuel_use_data", "link_to_report_4_on_company_website")) %>% 
    rename(report_type = report_4_type, yes_no = did_report_4_provide_electricity_or_fuel_use_data, link = link_to_report_4_on_company_website)
  
  source_assessed_5 <- data_sheet_energy_transformed %>% 
    filter(company == "Google") %>% 
    select(c("data_year", "report_5_type", "did_report_5_provide_electricity_or_fuel_use_data", "link_to_report_5_on_company_website")) %>% 
    rename(report_type = report_5_type, yes_no = did_report_5_provide_electricity_or_fuel_use_data, link = link_to_report_5_on_company_website)
  
  sources_assessed <- 
    rbind(source_assessed_1, source_assessed_2, source_assessed_3, source_assessed_4, source_assessed_5) %>% 
    drop_na(report_type) %>% distinct() %>% 
    mutate(row = row_number()) %>%
    pivot_wider(names_from = report_type, values_from = link) %>% 
    select(-row)
  
  jscode <- "function(settings) {
            var table = settings.oInstance.api();
            var nrows = table.rows().count();
            for(var i=0; i<nrows; i++){
            var cell3 = table.cell(i,3);
            var cell2 = table.cell(i,2);
            var cell1 = table.cell(i,1);
            var yes_no = cell1.data();
            var bgcolor;
            if(yes_no == 'Yes'){
            bgcolor = 'green';
            }else{
            bgcolor = 'red'
            }
            cell2.node().style.backgroundColor = bgcolor;
            cell3.node().style.backgroundColor = bgcolor;
            }
            }"
  
  output$sources_table <- renderDataTable({
    datatable(sources_assessed, rownames = FALSE, options = list(dom = 't', initComplete = JS(jscode), columnDefs = list(list(visible=FALSE, targets=1))))
  })
  
  router$server(input, output, session)
}

##################
##Return to Code##
##################

#######################################
#Graph 7###############################
# Change in energy use ################
#######################################

##Calculate  electricity use at each level of Data Center management (cloud, leased, self-managed) and Total Company
#selected_company_electricity_use_graph <- reactive({
#  data_sheet_energy_transformed %>% 
#    filter(company == input$selected_company) %>% 
#    mutate_at(vars(electricity_converted, fuel_1_converted, #replace na values with 0
#                   fuel_2_converted, fuel_3_converted, fuel_4_converted, 
#                   fuel_5_converted), ~replace_na(., 0)) %>%
#    select("data_year", "energy_reporting_scope", "level_of_ownership", "electricity_prepped", "unit", "notes_2") %>% 
#    mutate(energy_reporting_scope = case_when(
#      energy_reporting_scope %in% c("Multiple Data Centers", "Single Data Center") ~ "Data center electricity use",
#      energy_reporting_scope %in% "Total Operations"                               ~ "Company-wide electricity use")) %>% 
#    group_by(data_year, energy_reporting_scope, level_of_ownership) %>% 
#    summarize(value = sum(electricity_prepped)) %>% 
#    mutate(value = value/1000000000) %>% 
#    unite(energy_reporting_scope_level_of_ownership, energy_reporting_scope:level_of_ownership, sep = " | ")
#})
#
##Calculate only data center fuel use at each level of management (cloud, leased, self-managed)
#selected_company_fuel_use_graph <- reactive({
#  data_sheet_energy_transformed %>% 
#    filter(company == input$selected_company) %>% 
#    mutate_at(vars(electricity_converted, fuel_1_converted, #replace na values with 0
#                   fuel_2_converted, fuel_3_converted, fuel_4_converted, 
#                   fuel_5_converted), ~replace_na(., 0)) %>%
#    rowwise() %>% 
#    mutate(total_other_energy_use = sum(c(fuel_1_converted,
#                                          fuel_2_converted, fuel_3_converted, fuel_4_converted, 
#                                          fuel_5_converted))) %>% 
#    select("data_year", "energy_reporting_scope", "level_of_ownership", "total_other_energy_use", "notes_3") %>% 
#    filter(energy_reporting_scope == "Multiple Data Centers" | energy_reporting_scope == "Single Data Center") %>% 
#    mutate(energy_reporting_scope = case_when(
#      energy_reporting_scope %in% c("Multiple Data Centers", "Single Data Center") ~ "Data center other fuel use")) %>% 
#    group_by(data_year, energy_reporting_scope, level_of_ownership) %>% 
#    summarize(value = sum(total_other_energy_use)) %>% 
#    mutate(value = value/1000000000) %>% 
#    unite(energy_reporting_scope_level_of_ownership, energy_reporting_scope:level_of_ownership, sep = " | ")
#})
#
##Combine total company electricity use and data center electricity and fuel use at each level of management 
##7 possible categories (Total Company Electricity Use, Data Center Electricity Use - cloud, self-managed, leased, Data Center Other Fuel Use - cloud#, self-managed, leased )
#selected_company_total_energy_graph <- reactive({ rbind(selected_company_electricity_use_graph(), selected_company_fuel_use_graph()) })
#
##Calculate the total energy used by all data centers across electricity and fuel use at all scopes for each year
#dc_total_energy_use_by_year <- reactive({
#  data_sheet_energy_transformed %>% 
#    filter(company == input$selected_company) %>% 
#    mutate_at(vars(electricity_converted, fuel_1_converted, #replace na values with 0
#                   fuel_2_converted, fuel_3_converted, fuel_4_converted, 
#                   fuel_5_converted), ~replace_na(., 0)) %>%
#    rowwise() %>% 
#    mutate(total_other_energy_use = sum(c(electricity_converted, fuel_1_converted,
#                                          fuel_2_converted, fuel_3_converted, fuel_4_converted, 
#                                          fuel_5_converted))) %>% 
#    select("data_year", "energy_reporting_scope", "level_of_ownership", "total_other_energy_use", "notes_3") %>% 
#    filter(energy_reporting_scope == "Multiple Data Centers" | energy_reporting_scope == "Single Data Center") %>% 
#    mutate(energy_reporting_scope = case_when(energy_reporting_scope %in% c("Multiple Data Centers", "Single Data Center") ~ "Data center total #energy use")) %>% 
#    group_by(data_year, energy_reporting_scope) %>% 
#    summarise(total_dc_energy_use = sum(total_other_energy_use))
#})
#
##Calculate the total electricity used by company operations for each year
#total_electricity_use_by_year <- reactive({
#  data_sheet_energy_transformed %>% 
#    filter(company == input$selected_company) %>% 
#    mutate_at(vars(electricity_converted), ~replace_na(., 0)) %>%
#    select("data_year", "energy_reporting_scope", "level_of_ownership", "electricity_converted", "notes_3") %>% 
#    filter(energy_reporting_scope == "Total Operations") %>%  
#    group_by(data_year, energy_reporting_scope) %>% 
#    summarise(total_electricity_use = sum(electricity_converted))
#})
#
##Calculate the percentage of total data center energy use relative to total company electricity and format as a percent
#dc_divided_by_total_electricity <- reactive({ full_join(dc_total_energy_use_by_year(), total_electricity_use_by_year(), by = 'data_year') %>%
#    mutate_at(vars(total_dc_energy_use, total_electricity_use), ~replace_na(., 0)) %>% 
#    mutate(dc_total_percentage = percent(total_dc_energy_use/(total_dc_energy_use+total_electricity_use), accuracy=0.1)) })
#
##Calculate the total stacked bar height of the possible 7 categories by year (used to determine where to place the percentage values)
#total_stacked_bar_height_by_year <- reactive({ selected_company_total_energy_graph() %>%
#    group_by(data_year) %>%
#    summarise(max_pos = sum(value))
#})
#
##Join the 2 previous tables which contain the percent of data center energy use relative to total electricity and the height of the total energy use #across all uses
#selected_company_total_energy_plot <- reactive({ selected_company_total_energy_graph() %>% 
#    full_join(dc_divided_by_total_electricity(), by = 'data_year') %>% 
#    full_join(total_stacked_bar_height_by_year(), by = 'data_year')
#})
#
#output$companyfuelPlot <- renderPlotly({
#  ggplot(data = selected_company_total_energy_plot(), aes(fill=energy_reporting_scope_level_of_ownership, y=value, x=data_year)) + 
#    geom_bar(position="stack", stat="identity") +
#    geom_text(aes(y = max_pos +.1, label = dc_total_percentage), size = 4) +
#    scale_fill_brewer()
#})


#######################################
#######################################
# OLD #################################
#######################################

# # Years reported output
# output$years_reported <- renderText({buildyearsreportedOutput(by_fuel_type_data)})
# 
# # Companies reporting output
# output$companies_reporting <- renderText({buildcompaniesreportingOutput(by_fuel_type_data)})
# 
# #Energy use reported
# output$energy_reported <- renderText({energyusereportedOutput(aggregate_data)})