source(here("ui.R"))
source(here("R", "CompanyProfilePlots", "companyProfileElectricityUsePlot.R"))
source(here("R", "CompanyProfilePlots", "companyProfileFuelUsePlot.R"))
source(here("R", "CompanyProfilePlots", "companyProfileNonSpecifiedEnergyUsePlot.R"))
source(here("R", "CompanyProfilePlots", "companyProfilePUEPlot.R"))
source(here("R", "CompanyProfilePlots", "companyProfileMethodsTable.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsDataCenterPlot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsCompanyWide1Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsCompanyWide2Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsCompanyWide3Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsCompanyWide4Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsCompanyWide5Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsTransparencyPlot.R"))

#Server code
server <- function(input, output, session) {
  
  ########################################################
  ########################################################
  ###### Pre-render setup  ###############################
  ########################################################
  ########################################################
  
  ########################################################
  ###### Initialize waiter loading bar on full page ######
  ########################################################
  
  #add full screen 3 second waiter
  #w = Waiter$new()
  #w$show()
  #Sys.sleep(3)
  #w$hide() 
  
  ########################################################
  ###### Check login credentials (authetication) #########
  ########################################################
  
  #result_auth <- secure_server(check_credentials = check_credentials(credentials))
  #
  #output$res_auth <- renderPrint({
  #  reactiveValuesToList(result_auth)
  #})
  
  ########################################################
  ########################################################
  ###### Tab 1: Home  #################################### 
  ########################################################
  ########################################################
  
  #####################################################################
  ###### Card 1.2: Dynamically generate number of years reported ######
  #####################################################################
  
  number_of_reporting_years <- length(unique(data_sheet_energy_raw$report_year))
  
  output$years_reported <- renderUI({
    #output length of vector of unique values from report_year column
    number_of_reporting_years
    })
  
  output$companies_reporting <- renderUI({
    
    #generate complete list of companies reporting from Company Profiles sheet
    
    number_of_companies_reporting <- 
      data_sheet_company_raw %>% 
      
      #filter by only companies that have been checked back to 2007
      filter(checked_back_to_founding_year_or_2007 == "Yes")
    
    #output length of vector of unique values in the company column
    length(unique(number_of_companies_reporting$company_name))
    })
  
  output$energy_reported <- renderText({
  
    energy_reported <- data_sheet_energy_transformed %>% 
      mutate_at(vars(electricity_converted), ~replace_na(., 0)) %>%
      select("company", "data_year", "energy_reporting_scope", "level_of_ownership", "electricity_converted") %>% 
      filter(energy_reporting_scope == "Multiple Data Centers" | energy_reporting_scope == "Single Data Center" )
    
    paste(round(sum(energy_reported$electricity_converted)/1000000000, 1), "TWh")
  
  })
  
  ########################################################
  ########################################################
  ###### Tab 2: Industry Trends  ######################### 
  ########################################################
  ########################################################
  
  ########################################################
  ###### Generate transparency graph #####################
  ########################################################
  
  output$transparency_graph <- renderPlotly({
    buildIndustryTrendsTransparencyPlot(data_sheet_energy_raw)
  })
  
  ###############################################################
  ###### Generate reactive industry energy use graph ############
  ###############################################################
  
  ########################################
  # Generate reactive dataset ############
  ########################################
  
  energy_use_final <- reactive({
  
  for (year in 2007:as.integer(max(na.omit(data_sheet_energy_raw$report_year)))) {
    # create a sub data frame that is filtered by data year and single data center scope
    energy_use_SDC <- data_sheet_energy_transformed %>%
      filter(data_year == year, energy_reporting_scope == "Single Data Center") %>%
      select(company, data_year, energy_reporting_scope, level_of_ownership, electricity_converted) 
    
    # create a sub data frame that is filtered by data year and multiple data centers scope
    energy_use_MDC <- data_sheet_energy_transformed %>%
      filter(data_year == year, energy_reporting_scope == "Multiple Data Centers") %>%
      select(company, data_year, energy_reporting_scope, level_of_ownership, electricity_converted)
    
    # create a sub data frame that is filtered by data year / company wide electricity scope
    energy_use_TO <- data_sheet_energy_transformed %>%
      filter(data_year == year, energy_reporting_scope == "Total Operations") %>%
      select(company, data_year, energy_reporting_scope, level_of_ownership, electricity_converted) 
    
    if (year == 2007) {
      energy_use_graph <- rbind(energy_use_SDC, energy_use_MDC, energy_use_TO)
    } else {
      energy_use_graph <- energy_use_graph %>% rbind(energy_use_SDC, energy_use_MDC, energy_use_TO)
    }
  }
    
  # create another data frame that is filtered by company founding year
  data_founding_year <- data_sheet_company_raw %>% select(company_name, 
                                                      checked_back_to_founding_year_or_2007,
                                                      company_founding_year)
  # drop all the excess rows that have NA data
  data_founding_year <- na.omit(data_founding_year)
  data_founding_year <- data_founding_year[data_founding_year$checked_back_to_founding_year_or_2007=="Yes", ]
  energy_use_graph <- subset(energy_use_graph, company %in% data_founding_year$company_name)
  
  for (i in 1:nrow(energy_use_graph)) {
    if (energy_use_graph[i,3] == "Single Data Center" || energy_use_graph[i,3] == "Multiple Data Centers") {
      energy_use_graph[i,3] <- "Data Centers"
    } else {
      energy_use_graph[i,3] <- "Company Wide"
    }
  }
  
  # stack single data center/multiple data center data frames on top of each other
  energy_use_graph <- energy_use_graph[!(energy_use_graph$level_of_ownership == ""),]
  energy_use_graph <- na.omit(energy_use_graph)
  energy_use_final <- energy_use_graph %>%
    group_by(company,data_year,energy_reporting_scope,level_of_ownership) %>%
    dplyr::summarise(electricity_converted = sum(electricity_converted)) %>%
    as.data.frame()
  energy_use_final <- energy_use_final %>%
    group_by(company,data_year,energy_reporting_scope,level_of_ownership)
  
  energy_use_final <- energy_use_final %>%
    filter(data_year %in% input$input_year, energy_reporting_scope %in% input$input_reporting_scope)
  
  energy_use_final
  
  })
  
  ###########################################################
  # Generate Data Center and Company Wide Plots ##############
  ###########################################################
  
  #create plot pops up if there is no data
  void_plot <- ggplot() +
                theme_void() +
                xlab(NULL)
  
  ##################################
  ### Generate Data Center Plot ####
  ##################################
  
  output$data_centerplot <- renderPlot({
      buildIndustryTrendsDataCenterPlot(energy_use_final())
  })
    
  ##################################################
  ### Generate Company Wide Subdataset and Plots ###
  ##################################################
  
  company_wide_graph_height <- function(input) {
    if(nrow(input) == 0) {
      350
    } else if (nrow(input) == 1) {
      125 
    } else if (nrow(input) == 2) {
      200
    } else if (nrow(input) == 3) {
      300
    } else if (nrow(input) == 4) {
      400
    } else if (nrow(input) == 5) {
      500
    } else {
      500 + nrow(input)*10
    }
  }
  ###########################
  ### Company Wide Plot 1 ###
  ###########################
  
  energy_use_L2_1 <- reactive({ 
    energy_use_final() %>% 
      filter(electricity_converted < 10000000)
  })
  
  company_wide_plot_1_height <- reactive({
    company_wide_graph_height(energy_use_L2_1())
  })
  
  output$company_wide_plot_1 <- renderPlot({
    
    if (nrow(energy_use_L2_1()) != 0) {
      
      buildIndustryTrendsCompanyWide1Plot(energy_use_L2_1())
      
    } else {
      
      void_plot +
        geom_text(aes(0,0,label='No Data Reported At This Scale'), size = 7)
      
    }
    
  }, height = reactive({company_wide_plot_1_height()}))
  
  ###########################
  ### Company Wide Plot 2 ###
  ###########################
  
  energy_use_L2_2 <- reactive({ 
    energy_use_final() %>% 
      filter(electricity_converted > 10000000, electricity_converted < 100000000)
  })
  
  company_wide_plot_2_height <- reactive({
    company_wide_graph_height(energy_use_L2_2())
  })
  
  output$company_wide_plot_2 <- renderPlot({
    
    if (nrow(energy_use_L2_2()) != 0) {
      
      buildIndustryTrendsCompanyWide2Plot(energy_use_L2_2())
      
    } else {
      
      void_plot +
        geom_text(aes(0,0,label='No Data Reported At This Scale'), size = 7)
      
    }
  
  }, height = reactive({company_wide_plot_2_height()}))
  
  ###########################
  ### Company Wide Plot 3 ###
  ###########################
  
  energy_use_L2_3 <- reactive({ 
    energy_use_final() %>% 
      filter(electricity_converted > 100000000, electricity_converted < 1000000000)
  })
  
  company_wide_plot_3_height <- reactive({
    company_wide_graph_height(energy_use_L2_3())
  })
  
  output$company_wide_plot_3 <- renderPlot({
    
    if (nrow(energy_use_L2_3()) != 0) {
      
      buildIndustryTrendsCompanyWide3Plot(energy_use_L2_3())
      
    } else {
      
      void_plot +
        geom_text(aes(0,0,label='No Data Reported At This Scale'), size = 7)
      
    }
  
  }, height = reactive({company_wide_plot_3_height()}))
  
  ###########################
  ### Company Wide Plot 4 ###
  ###########################
  
  energy_use_L2_4 <- reactive({ 
    energy_use_final() %>% 
    filter(electricity_converted > 1000000000, electricity_converted < 10000000000)
  })
  
  company_wide_plot_4_height <- reactive({
    company_wide_graph_height(energy_use_L2_4())
  })
  
  output$company_wide_plot_4 <- renderPlot({
    
    if (nrow(energy_use_L2_4()) != 0) {
      
      buildIndustryTrendsCompanyWide4Plot(energy_use_L2_4())
      
    } else {
      
      void_plot +
        geom_text(aes(0,0,label='No Data Reported At This Scale'), size = 7)
      
    }
    
  }, height = reactive({company_wide_plot_4_height()}))
  
  ###########################
  ### Company Wide Plot 5 ###
  ###########################
  
  energy_use_L2_5 <- reactive({ 
    energy_use_final() %>% 
      filter(electricity_converted > 10000000000)
  })
  
  company_wide_plot_5_height <- reactive({
    company_wide_graph_height(energy_use_L2_5())
  })
  
  output$company_wide_plot_5 <- renderPlot({
    
    if (nrow(energy_use_L2_5()) != 0) {
      
      buildIndustryTrendsCompanyWide5Plot(energy_use_L2_5())
      
    } else {
      
      void_plot +
        geom_text(aes(0,0,label='No Data Reported At This Scale'), size = 7)
      
    }
    
  }, height = reactive({company_wide_plot_5_height()}))
  
  #Conditionally show either 1 data center plot or 5 company wide plots depending on user selection
  observeEvent(input$input_reporting_scope, {
    
    if(input$input_reporting_scope == "Company Wide") { 
      shinyjs::hide(selector = "div#data-center-plots")
      shinyjs::show(selector = "div#company-wide-plots")
    } else if (input$input_reporting_scope == "Data Centers") {
      shinyjs::show(selector = "div#data-center-plots")
      shinyjs::hide(selector = "div#company-wide-plots")
    }
    
  })
  
  ########################################################
  ########################################################
  ###### Tab 3: Company Analysis  ######################## 
  ########################################################
  ########################################################
  
  ########################################################
  ###### Generate reactive datasets for sub-rendering ####
  ########################################################
  
  #This dataset filters the raw company profiles sheet by the selected company's current year
  company_sheet_selected_company <- reactive({
    data_sheet_company_raw %>% filter(company_name %in% input$selected_company)
  })
  
  #This dataset filters the raw energy sheet by the selected company's most recent year of data reporting
  energy_sheet_selected_company_current_year <- reactive({
    
    data_sheet_energy_raw %>% 
      filter(company %in% input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date, na.rm = TRUE)) 
  })
  
  #This dataset is used to check for SASB, GRI, CDP reporting
  sasb_cdp_gri_status <- reactive({
    
    energy_sheet_selected_company_current_year_scg <- data_sheet_energy_raw %>% 
      filter(company %in% input$selected_company) %>% 
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
      filter(company %in% input$selected_company) %>% 
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
    data_sheet_energy_raw %>% filter(company %in% input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers") %>% 
      drop_na(electricity_value)
  })
  
  #This dataset is use to check if a company is reporting data center fuel use
  energy_sheet_selected_company_current_year_dc_fuel <- reactive({
    data_sheet_energy_raw %>% filter(company %in% input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers") %>% 
      drop_na(fuel_1_value)
  })
  
  #This dataset is used to check what level of data center management the company is reporting (filters out total operations rows as sometimes total operations rows are marked as "self-managed)
  energy_sheet_selected_company_current_year_dc <- reactive({
    data_sheet_energy_raw %>% filter(company %in% input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers")
  })
  
  #This dataset is use to check if a company is reporting company-wide electricity use
  energy_sheet_selected_company_current_year_te <- reactive({
    data_sheet_energy_raw %>% filter(company %in% input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Total Operations") %>% 
      drop_na(electricity_value)
  })
  
  #########################################################
  ####### Table 3.1: Select company quick stats ###########
  #########################################################
  
  #Does Company X report any energy use?
  
  energy_reporting_status <- reactive({
    ifelse(sum(energy_sheet_selected_company_current_year()$electricity_value > 0, na.rm = TRUE) | 
             sum(energy_sheet_selected_company_current_year()$fuel_1_value > 0, na.rm = TRUE), "Yes", "No")
  })
  
  #Year of most recent data
  year_of_most_recent_data <- reactive({
    energy_sheet_selected_company_current_year()[1, "period_covered_start_date"]
  })  
  
  #Render list of external service providers  
  external_service_provider_list <- reactive({ paste(company_sheet_selected_company()[1, "provider_1"], company_sheet_selected_company()[1, "provider_2"],
                                          company_sheet_selected_company()[1, "provider_3"], company_sheet_selected_company()[1, "provider_4"],
                                          company_sheet_selected_company()[1, "provider_5"], company_sheet_selected_company()[1, "provider_6"],
                                          company_sheet_selected_company()[1, "provider_7"], company_sheet_selected_company()[1, "provider_8"],
                                          company_sheet_selected_company()[1, "provider_9"], company_sheet_selected_company()[1, "provider_10"],sep = ", ") %>%
                                          str_remove_all(", NA") %>% str_remove_all(", ,") %>% str_remove_all(" , ") %>% str_remove_all("[:punct:]*\\s*$") %>% 
                                          str_remove_all("NA")
  })
  
  selected_company_stats <- reactive({ data.frame(A = c("Does company report energy use?", "Year of most recent data", "Lease/Cloud Providers"),
                                       B = c(energy_reporting_status(), year_of_most_recent_data(), ifelse(external_service_provider_list() == "", "No External Providers Reported",
                                                                                                           external_service_provider_list())), check.names = FALSE)
  })
  
  output$selected_company_stats <- renderDataTable({
  
  datatable(selected_company_stats(), rownames = FALSE, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){", "  $(thead).remove();","}")))
  
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
  
  data_center_electricity_reporting_status <- reactive({ ifelse(sum(energy_sheet_selected_company_current_year_dc_electricity()$electricity_value > 0), "Yes", "No") })
  data_center_self_managed_reporting_status <- reactive({ ifelse("Self-managed" %in% energy_sheet_selected_company_current_year_dc()$level_of_ownership, "Yes", "No") })
  data_center_leased_reporting_status <- reactive({ ifelse("Leased" %in% energy_sheet_selected_company_current_year_dc()$level_of_ownership, "Yes", "No") })
  data_center_cloud_reporting_status <- reactive({ ifelse("Cloud" %in% energy_sheet_selected_company_current_year_dc()$level_of_ownership, "Yes", "No") })
  data_center_other_fuel_use_reporting_status <- reactive({ ifelse(sum(energy_sheet_selected_company_current_year_dc_fuel()$fuel_1_value > 0), "Yes", "No") })
  total_energy_reporting_status <- reactive({ ifelse(sum(energy_sheet_selected_company_current_year_te()$electricity_value > 0), "Yes", "No") })
  
  reported_energy_levels_data <- reactive({
    data.frame(Level = c("Data center electricity use", "Self-managed", "Leased", "Cloud", "Data center other fuel use", "Company-wide electricty use"),
               " Reporting Status " = c(data_center_electricity_reporting_status(), 
                                        data_center_self_managed_reporting_status(), 
                                        data_center_leased_reporting_status(), 
                                        data_center_cloud_reporting_status(), 
                                        data_center_other_fuel_use_reporting_status(), 
                                        total_energy_reporting_status()), check.names = FALSE) %>% 
    add_column(format = c(1,0,0,0,1,1), .before = 'Level')
  })
    
  output$reported_energy_levels <- renderDataTable({
    
    datatable(reported_energy_levels_data(), rownames = FALSE, options = list(dom = 't', columnDefs = list(list(visible=FALSE, targets=0)))) %>% 
      formatStyle(
        'Level', 'format',
        textAlign = styleEqual(c(0, 1), c('right', 'left')),
        fontStyle = styleEqual(c(0, 1), c('italic', 'normal'))
      )
  })
  
  #######################################
  #Table 5###############################
  #Data standards reported###############
  #######################################
  
  #UI datatable output
  output$data_standards <- renderDataTable({
    datatable(sasb_cdp_gri_status(), rownames = FALSE, options = list(dom = 't'))
  })
  
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
  
  output$electricity_use_table <- renderDataTable({
    
    selected_company_electricity_use_filter <- buildCompanyProfileElectricityUsePlot(input$selected_company)
    
    #If the dataframe output from the reactive electricity dataset is not empty then insert the dataset into a datatable, else show the no_data datatable
    if(nrow(selected_company_electricity_use_filter) != 0) {
      datatable(selected_company_electricity_use_filter, rownames = FALSE, options = list(columnDefs = list(list(visible=FALSE, targets=0)), scrollX = TRUE)) %>% 
        formatStyle('category', 'format', textAlign = styleEqual(c(0, 1), c('right', 'left')), fontStyle = styleEqual(c(0, 1), c('italic', 'normal')))
    } else {
      datatable(no_data, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){", "  $(thead).remove();", "}")), rownames = FALSE)
    }
  })
  
  #######################################
  #Table 9###############################
  #Other Fuel Use (TWh/yr)###############
  #######################################
  
  output$other_fuel_use_table <- renderDataTable({
    
    selected_company_fuel_use_filter <- buildCompanyProfileFuelUsePlot(input$selected_company)
    
    if(nrow(selected_company_fuel_use_filter) != 0) {
      #shinyjs::show(selector = "div#fuel-use-table")
      datatable(selected_company_fuel_use_filter, rownames = FALSE, options = list(columnDefs = list(list(visible=FALSE, targets=0)), scrollX = TRUE)) %>% 
        formatStyle('category', 'format', textAlign = styleEqual(c(0, 1), c('right', 'left')), fontStyle = styleEqual(c(0, 1), c('italic', 'normal')))
    } else {
      #shinyjs::hide(selector = "div#fuel-use-table")
      datatable(no_data, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}")), rownames = FALSE)
    }
  })
  
  #######################################
  #Table 10##############################
  #Non-specified energy use (TWh/yr)#####
  #######################################
  
  output$ns_energy_use_table <- renderDataTable({
    
    selected_company_ns_energy_use_filter <- buildCompanyProfileNonSpecifiedEnergyUsePlot(input$selected_company)
    
    if(nrow(selected_company_ns_energy_use_filter) != 0) {
      datatable(selected_company_ns_energy_use_filter, rownames = FALSE, options = list(columnDefs = list(list(visible=FALSE, targets=0)), scrollX = TRUE)) %>% 
        formatStyle('category', 'format', textAlign = styleEqual(c(0, 1), c('right', 'left')), fontStyle = styleEqual(c(0, 1), c('italic', 'normal')))
    } else {
      datatable(no_data, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){", "  $(thead).remove();", "}")), rownames = FALSE)
    }
  })
  
  #######################################
  #Table 11##############################
  #PUE###################################
  #######################################
  
  output$pue_table <- renderDataTable({

    selected_company_pue_filter <- buildCompanyProfilePUEPlot(input$selected_company)
    
    if(nrow(selected_company_pue_filter) != 0) {
      datatable(selected_company_pue_filter, rownames = FALSE, options = list(pageLength = 5, autoWidth = TRUE, columnDefs = list(list(width = '300px',targets = c(0)))))
    } else {
      datatable(no_data, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}")), rownames = FALSE)
    }
    
  })
  
  #######################################
  #Table 12##############################
  # Methodology ######################
  #######################################

  #selected_nav <- 'home'
  
  #change to methods page when the learn more button is clicked
  #onclick('learn-more', selected_nav <- 'method')
  onclick('learn-more', change_page('/methods', session = shiny::getDefaultReactiveDomain(), mode = "push"))
  
  #output$selected_nav <- renderText({ selected_nav })
  
  #environment(navigation[["children"]][[2]])[["data"]][["props"]][["value"]][["selectedKey"]] <- "method"
  
  #change to contact us page when the report issue button is clicked
  onclick('report-issue', change_page('/contact-us', session = shiny::getDefaultReactiveDomain(), mode = "push"))
  #onclick('learnmore', Nav(selectedKey = 'methods'))
  
  #######################################
  #Table 13##############################
  #Sources Assessed######################
  #######################################
  
  output$sources_table <- renderDataTable({
    buildCompanyProfileMethodsTable(data_sheet_energy_transformed, input$selected_company)
  })
  
  ##############################################################################################################
  ##### Generate downloadable csv of full company profile ######################################################
  ##############################################################################################################
  
  #UI downloadable CSV
  output$download_standards <- downloadHandler(
    filename = function(){sprintf("%s_profile_download.csv", input$selected_company)}, 
    content = function(fname){
      write.table(data.frame(x = c("Exported company profile page from movingbits.com","License XX"))[1:3,] %>% replace(is.na(.), ""), fname, col.names = FALSE, sep = ',', row.names = F)
      write.table(data.frame(x = c("Section 1: Company Overview","Note: These values apply only to the company's most recent year of data reporting"))[1:3,] %>% replace(is.na(.), ""), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
      write.table(data.frame(x = "Company name", y = input$selected_company), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
      write.table(selected_company_stats(), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
      write.table(data.frame(x = "Company data center overview", y = company_sheet_selected_company()[1, "company_data_center_overview"]), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
      write.table(data.frame(x = "Energy report assessment", y = company_sheet_selected_company()[1, "energy_reporting_assessment"])[1:2,] %>% replace(is.na(.), ""), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
      write.table(data.frame(x = c("Section 2: Reported Energy Use Levels","Note: These values apply only to the company's most recent year of data reporting"))[1:3,] %>% replace(is.na(.), ""), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
      write.table(reported_energy_levels_data()[1:7,2:3] %>% replace(is.na(.), ""), fname, sep = ',', append = TRUE, row.names = F)
      write.table(data.frame(x = c("Section 3: Data Standards","Note: These values apply only to the company's most recent year of data reporting"))[1:3,] %>% replace(is.na(.), ""), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
      write.table(sasb_cdp_gri_status()[1:4,] %>% replace(is.na(.), ""), fname, sep = ',', append = TRUE, row.names = F)
      write.table(data.frame(x = c("Section 4: Other Metrics Reported","Note: These values apply only to the company's most recent year of data reporting"))[1:3,] %>% replace(is.na(.), ""), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
      write.table(pue_wue_renewables_status()[1:4,] %>% replace(is.na(.), ""), fname, sep = ',', append = TRUE, row.names = F)
      write.table(data.frame(x = c("Section 5: Historical Energy Use Trend & Data")), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
      if(nrow(buildCompanyProfileElectricityUsePlot(input$selected_company) != 0)) {
        write.table(data.frame(x = c("", "Electricity Use (TWh/yr)")), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
        write.table(buildCompanyProfileElectricityUsePlot(input$selected_company)[-1], fname, col.names = TRUE, sep = ',', append = TRUE, row.names = F)}
      if(nrow(buildCompanyProfileFuelUsePlot(input$selected_company) != 0)) {
        write.table(data.frame(x = c("", "Other fuel use (TWh/yr)")), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
        write.table(buildCompanyProfileFuelUsePlot(input$selected_company)[-1], fname, col.names = TRUE, sep = ',', append = TRUE, row.names = F)}
      if(nrow(buildCompanyProfileNonSpecifiedEnergyUsePlot(input$selected_company) != 0)) {
        write.table(data.frame(x = c("", "Non-specified energy use (TWh/yr)")), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
        write.table(buildCompanyProfileNonSpecifiedEnergyUsePlot(input$selected_company)[-1], fname, col.names = TRUE, sep = ',', append = TRUE, row.names = F)}
      if(nrow(buildCompanyProfilePUEPlot(input$selected_company) != 0)) {
        write.table(data.frame(x = c("", "PUE")), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
        write.table(buildCompanyProfilePUEPlot(input$selected_company), fname, col.names = TRUE, sep = ',', append = TRUE, row.names = F)}
    }
  )
  
  ####################################
  ##### Additional interactivity #####
  ####################################
  
  ##########################################
  ##### Display or hide fuel-use-table #####
  ##########################################
  
  #Conditionally show data tables depending on data availability
  #observeEvent(selected_company_fuel_use_filter(), {
  #  
  # if(!is.null(selected_company_fuel_use_filter())) { 
  #   shinyjs::hide(selector = "div#fuel-use-table")
  # } else if (!is.null(selected_company_fuel_use_filter())) {
  #   shinyjs::show(selector = "div#fuel-use-table")
  # }
  # 
  #})

  ########################################################
  ########################################################
  ###### Tab 5: Methodology  ############################# 
  ########################################################
  ########################################################
  
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