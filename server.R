source(here("ui.R"))
source(here("R", "CompanyProfilePlots", "companyProfileElectricityUsePlot.R"))
source(here("R", "CompanyProfilePlots", "companyProfileFuelUsePlot.R"))
source(here("R", "CompanyProfilePlots", "companyProfileNonSpecifiedEnergyUsePlot.R"))
source(here("R", "CompanyProfilePlots", "companyProfilePUEPlot.R"))
source(here("R", "CompanyProfilePlots", "companyProfileMethodologyTable.R"))
source(here("R", "CompanyProfilePlots", "companyProfileSourcesAssessedTable.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsDataCenter1Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsDataCenter2Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsCompanyWide1Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsCompanyWide2Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsCompanyWide3Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsCompanyWide4Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsCompanyWide5Plot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsTransparencyPlot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsTimelinePlot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsLeaseCloudNetworkPlot.R"))
source(here("R", "IndustryTrendsPlots", "industryTrendsPUETrendsPlot.R"))
source(here("R", "CompanyProfilePlots", "companyProfileTransparencyOverTimePlot.R"))

#Server code
server <- function(input, output, session) {
  
  ########################################################
  ########################################################
  ###### Pre-render setup  ###############################
  ########################################################
  ########################################################
  
  ###################################################################################
  ###### Dynamically update nav bar selection when user clicks back or forward ######
  ###################################################################################
  
  links_lookup <- data.frame(
    route = c("/", "data-center-energy", "reporting-trends", "energy-data-trends", "reporting-timeline", "lease-cloud-network", "pue-trends", "company-analysis", "methods", "contact-us", "about-us", "isal"),
    name = c('Home', 'Data Center Energy 101', 'Energy Reporting Trends', 'Energy Data Trends', 'Reporting Timeline', 'Industry Relationships', 'PUE Trends', 'Single Company Analysis', 'Methods', 'Contact', 'About', 'ISA Lab Website')
  )
  
  shiny::observeEvent(shiny.router::get_page(), {
    page_title <- shiny.router::get_page()
    page_title <- links_lookup %>%
      filter(route == page_title) %>%
      pull(name)
    runjs(glue("$('.ms-Nav-link[title=\"{page_title}\"]')[0].click()"))
  })
  
  ########################################################
  ###### Initialize waiter loading bar on full page ######
  ########################################################
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  
  on.exit(progress$close())
  
  progress$set(message = "Initializing application", value = 0)
  
  # Close the progress when this reactive exits (even if there's an error)
  
  ########################################################
  ###### Check login credentials (authetication) #########
  ########################################################
  
  #result_auth <- secure_server(check_credentials = check_credentials(credentials))
  #
  #output$res_auth <- renderPrint({
  #  reactiveValuesToList(result_auth)
  #})
  
###########################################################################################################################################################
###########################################################################################################################################################
###### Tab 1: Home  #######################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
  
  #####################################################################
  ###### Card 1.2: Dynamically generate number of years reported ######
  #####################################################################
  
  #calculate number of years reported by finding the length of a vector of unique report year values
  #number_of_reporting_years <- length(unique(data_sheet_energy_transformed$data_year))
  
  #assign function to start value for report year
  #number_of_reporting_years_start_val <- reactiveVal(0)
  
  #this section creates a number incrementing animation on the home page it works by
  #1. observing if the start value above is less than the number_of_reporting_years values
  #2. if it is, then one is added to the number_of_reporting_years_start_val, this new value is assigned to number_of_reporting_years_new_val
  #3. number_of_reporting_years_new_val replaces the start value
  #4. this repeats itself, but is delayed by 100 milliseconds each time using invalidateLater
  #observe({
  #  invalidateLater(100, session)
  #  isolate({
  #    if(number_of_reporting_years_start_val() < number_of_reporting_years) {
  #      number_of_reporting_years_new_val <- number_of_reporting_years_start_val()+1
  #      number_of_reporting_years_start_val(number_of_reporting_years_new_val)
  #    }
  #  })
  #})
  
  output$years_reported <- renderText({
    #output length of vector of unique values from report_year column
    #paste(number_of_reporting_years_start_val())
    
    paste(length(unique(data_sheet_energy_transformed$data_year)))
    })
  
  ##########################################################################
  ###### Card 1.3: Dynamically generate number of companies reporting ######
  ##########################################################################
  
  #calculate the number of companies reporting by getting the length of the companies vector from global
  #number_of_companies_reporting <- length(unique_companies)
  
  #assign function for counter
  #number_of_companies_reporting_start_val <- reactiveVal(0)
  
  #observe({
  #  invalidateLater(100, session)
  #  isolate({
  #    if(number_of_companies_reporting_start_val() < number_of_companies_reporting) {
  #      number_of_companies_reporting_new_val <- number_of_companies_reporting_start_val()+1
  #      number_of_companies_reporting_start_val(number_of_companies_reporting_new_val)
  #    }
  #  })
  #})
  
  output$companies_reporting <- renderText({
    #paste(number_of_companies_reporting_start_val())
    
    #remove below and uncomment above to readd counter
    paste(length(unique_companies))
    })
  
  ########################################################################################
  ###### Card 1.4: Dynamically generate total amount of data center energy reported ######
  ########################################################################################
  
  total_data_center_electricity_use_reported <- data_sheet_energy_transformed %>% 
    mutate_at(vars(electricity_converted), ~replace_na(., 0)) %>%
    select("company", "data_year", "energy_reporting_scope", "level_of_ownership", "electricity_converted") %>% 
    filter(energy_reporting_scope == "Multiple Data Centers" | energy_reporting_scope == "Single Data Center" ) %>% 
    filter(data_year == (max(data_year)-1))
  
  #total_data_center_electricity_use_reported <- round(sum(total_data_center_electricity_use_reported$electricity_converted)/1000000000, 1)
  
  #assign function for counter
  #total_data_center_electricity_use_reported_start_val <- reactiveVal(0)
  
  #observe({
  #  invalidateLater(25, session)
  #  isolate({
  #    if(total_data_center_electricity_use_reported_start_val() < total_data_center_electricity_use_reported) {
  #      total_data_center_electricity_use_reported_new_val <- total_data_center_electricity_use_reported_start_val()+1
  #      total_data_center_electricity_use_reported_start_val(total_data_center_electricity_use_reported_new_val)
  #    }
  #  })
  #})
  
  #Render total data center energy use reported in second most recent year in UI"
  output$energy_reported <- renderText({
    #paste(total_data_center_electricity_use_reported_start_val(), "TWh")
    
    paste(round(sum(total_data_center_electricity_use_reported$electricity_converted)/1000000000, 1), "TWh")
  })
  
  #Render "Data Center Electricity Use Reported In <1 year minus the latest year in reporting> in UI"
  output$energy_reported_text <- renderText({
    paste("Data Center Electricity Use Reported In", (max(data_sheet_energy_transformed$data_year)-1))
  })
  
###########################################################################################################################################################
###########################################################################################################################################################
###### Tab 2: Industry Trends  ############################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
  
  ########################################################
  ###### Generate transparency graph #####################
  ########################################################
  
  output$transparency_graph <- renderGirafe({
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
    
    company_DC <- c(energy_use_SDC$company, energy_use_MDC$company)
    company_DC <- company_DC %>% unique()
    
    # create a sub data frame that is filtered by data year / company wide electricity scope
    energy_use_TO <- data_sheet_energy_transformed %>%
      filter(data_year == year, energy_reporting_scope == "Total Operations") %>%
      select(company, data_year, energy_reporting_scope, level_of_ownership, electricity_converted) 
    
    energy_use_TO <- energy_use_TO[!(energy_use_TO$company %in% company_DC),]
    
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
  
  energy_use_graph$energy_reporting_scope[energy_use_graph$energy_reporting_scope == "Single Data Center"] <- "Data Centers"
  energy_use_graph$energy_reporting_scope[energy_use_graph$energy_reporting_scope == "Multiple Data Centers"] <- "Data Centers"
  energy_use_graph$energy_reporting_scope[energy_use_graph$energy_reporting_scope == "Total Operations"] <- "Company Wide"
  
  # stack single data center/multiple data center data frames on top of each other
  energy_use_graph <- energy_use_graph[!(energy_use_graph$energy_reporting_scope == "Data Centers" && energy_use_graph$level_of_ownership == ""),]
  energy_use_graph <- na.omit(energy_use_graph)
  energy_use_final <- energy_use_graph %>%
    group_by(company,data_year,energy_reporting_scope,level_of_ownership) %>%
    dplyr::summarise(electricity_converted = sum(electricity_converted)) %>%
    as.data.frame()
  energy_use_final <- energy_use_final %>%
    group_by(company,data_year,energy_reporting_scope,level_of_ownership)
  
  energy_use_final_by_year <- energy_use_final %>%
    group_by(company,data_year) %>%
    dplyr::summarise(electricity_converted = sum(electricity_converted)) %>%
    as.data.frame()
  energy_use_final_by_year <- energy_use_final_by_year %>%
    group_by(company,data_year)
  
  energy_use_final$TWH_level <- ""
  
  for (i in 1:nrow(energy_use_final)) {
    if (i != nrow(energy_use_final) 
        && energy_use_final[i,"company"] == energy_use_final[i+1,"company"]
        && energy_use_final[i,"data_year"] == energy_use_final[i+1,"data_year"]) {
      if (energy_use_final[i,"electricity_converted"] + energy_use_final[i+1,"electricity_converted"] < 1000000000) {
        energy_use_final[i,"TWH_level"] <- "Below"
        energy_use_final[i+1,"TWH_level"] <- "Below"
      } else {
        energy_use_final[i,"TWH_level"] <- "At or Above"
        energy_use_final[i+1,"TWH_level"] <- "At or Above"
      }
    } else if (energy_use_final[i,"TWH_level"] == "" && energy_use_final[i,"electricity_converted"] < 1000000000) {
        energy_use_final[i,"TWH_level"] <- "Below"
    } else if (energy_use_final[i,"TWH_level"] == "" && energy_use_final[i,"electricity_converted"] >= 1000000000) {
      energy_use_final[i,"TWH_level"] <- "At or Above"
    }
  }
  
  energy_use_final <- energy_use_final %>%
    filter(data_year %in% input$input_year, energy_reporting_scope %in% input$input_reporting_scope)
  
  energy_use_final
  
  })
  
  ##################################################################################################
  #Conditionally show either 1 data center plot or 5 company wide plots depending on user selection#
  ##################################################################################################
  
  observeEvent(input$input_reporting_scope, {
    
    if(input$input_reporting_scope == "Company Wide") { 
      shinyjs::hide(selector = "div#data-center-plot-1")
      shinyjs::hide(selector = "div#data-center-plot-2")
      shinyjs::show(selector = "div#company-wide-plot-1")
      shinyjs::show(selector = "div#company-wide-plot-2")
      shinyjs::show(selector = "div#company-wide-plot-3")
      shinyjs::show(selector = "div#company-wide-plot-4")
      shinyjs::show(selector = "div#company-wide-plot-5")
    } else if (input$input_reporting_scope == "Data Centers") {
      shinyjs::show(selector = "div#data-center-plot-1")
      shinyjs::show(selector = "div#data-center-plot-2")
      shinyjs::hide(selector = "div#company-wide-plot-1")
      shinyjs::hide(selector = "div#company-wide-plot-2")
      shinyjs::hide(selector = "div#company-wide-plot-3")
      shinyjs::hide(selector = "div#company-wide-plot-4")
      shinyjs::hide(selector = "div#company-wide-plot-5")
    }
    
  })
  
  observeEvent(input$input_year, {
    
    if(input$input_reporting_scope == "Company Wide") { 
      shinyjs::show(selector = "div#company-wide-plot-1")
      shinyjs::show(selector = "div#company-wide-plot-2")
      shinyjs::show(selector = "div#company-wide-plot-3")
      shinyjs::show(selector = "div#company-wide-plot-4")
      shinyjs::show(selector = "div#company-wide-plot-5")
      shinyjs::hide(selector = "div#data-center-plot-1")
      shinyjs::hide(selector = "div#data-center-plot-2")
    } else if(input$input_reporting_scope == "Data Centers") { 
      shinyjs::hide(selector = "div#company-wide-plot-1")
      shinyjs::hide(selector = "div#company-wide-plot-2")
      shinyjs::hide(selector = "div#company-wide-plot-3")
      shinyjs::hide(selector = "div#company-wide-plot-4")
      shinyjs::hide(selector = "div#company-wide-plot-5")
      shinyjs::show(selector = "div#data-center-plot-1")
      shinyjs::show(selector = "div#data-center-plot-2")
    }
    
  })
  
  ##################################################
  ### Generate Company Wide Subdataset and Plots ###
  ##################################################
  
  company_wide_graph_height <- function(input) {
    if(nrow(input) == 0) {
      350
    } else if (nrow(input) == 1) {
      129 
    } else if (nrow(input) == 2) {
      215
    } else if (nrow(input) == 3) {
      300
    } else if (nrow(input) == 4) {
      392
    } else if (nrow(input) == 5) {
      490
    } else {
      89.9*nrow(input) + 35.5
    }
  }
  
  ##################################
  ### Generate Data Center Plot ####
  ##################################
  
  energy_use_L1_1 <- reactive({ 
    energy_use_final() %>% 
      filter(TWH_level == "Below")
  })
  
  data_center_plot_1_height <- reactive({
    company_wide_graph_height(energy_use_L1_1())
  })
  
  output$data_centerplot_1 <- renderPlot({
      if (nrow(energy_use_L1_1()) != 0) {
        buildIndustryTrendsDataCenter1Plot(energy_use_L1_1())
      } else {
        shinyjs::hide(selector = "div#data-center-plot-1")
      }
    }, height = reactive({data_center_plot_1_height()}))
  
  energy_use_L1_2 <- reactive({ 
    energy_use_final() %>% 
      filter(TWH_level == "At or Above")
  })
  
  data_center_plot_2_height <- reactive({
    company_wide_graph_height(energy_use_L1_2())
  })
  
  output$data_centerplot_2 <- renderPlot({
    if (nrow(energy_use_L1_2()) != 0) {
      buildIndustryTrendsDataCenter2Plot(energy_use_L1_2())
    } else {
      shinyjs::hide(selector = "div#data-center-plot-2")
    }
  }, height = reactive({data_center_plot_2_height()}))
    
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
      shinyjs::hide(selector = "div#company-wide-plot-1")
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
      
      shinyjs::hide(selector = "div#company-wide-plot-2")
      
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
      
      shinyjs::hide(selector = "div#company-wide-plot-3")
      
    }
  
  }, height = reactive({company_wide_plot_3_height()}))
  
  progress$inc(.5, detail = "Updating graphs")
  
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
      
      shinyjs::hide(selector = "div#company-wide-plot-4")
      
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
      shinyjs::hide(selector = "div#company-wide-plot-5")
    }
    
  }, height = reactive({company_wide_plot_5_height()}))
  
  ###############################
  ### Reporting Timeline Plot ###
  ###############################
  
  output$reporting_timeline <- renderPlotly({
    buildIndustryTrendsTimelinePlot(data_sheet_energy_transformed)
  })
  
  ##########################################
  ### Cloud/Lease Providers Network Plot ###
  ##########################################
  
  output$lease_cloud_network <- renderVisNetwork({
    buildIndustryTrendsLeaseCloudNetworkPlot()
  })
  
  #######################
  ### PUE Trends Plot ###
  #######################
  
  data_sheet_pue_filtered <- reactive({
    data_sheet_pue_raw %>% filter(company == input$selected_company_pue)
  })
  
  output$pue_trends_plot <- renderPlotly({
    buildIndustryTrendsPUETrends(data_sheet_pue_raw, input$selected_company_pue, input$selected_scope_pue)
  })
  
###########################################################################################################################################################
###########################################################################################################################################################
###### Tab 3: Company Analysis  ###########################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
  
  ###############################################################
  ###### Generate reactive titles based on user selection #######
  ###############################################################
  
  output$company_profiles_title_1 <- renderText({
    paste("Current Year Energy Reporting Snapshot For", input$selected_company)
  })
  
  output$company_profiles_title_2 <- renderText({
    paste("Historical Energy Use Trend & Data For", input$selected_company)
  })
  
  output$company_profiles_title_3 <- renderText({
    paste("Methodological Notes For", input$selected_company)
  })
  
  output$company_profiles_title_4 <- renderText({
    paste("Sources Assessed For", input$selected_company)
  })
  
  ########################################################
  ###### Generate reactive datasets for subsetting #######
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
      slice_max(period_covered_start_date) 
      #filter(period_covered_start_date == max(period_covered_start_date)) 
  })
  
  #This dataset is used to check for SASB, GRI, CDP reporting
  sasb_cdp_gri_status <- reactive({
    
    energy_sheet_selected_company_current_year_scg <- data_sheet_energy_raw %>% 
      filter(company %in% input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      slice_max(period_covered_start_date)
      #filter(period_covered_start_date == max(period_covered_start_date))
    
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
      slice_max(period_covered_start_date)
      
      #filter(period_covered_start_date == max(period_covered_start_date))
    
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
      slice_max(period_covered_start_date) %>% 
      #filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers") %>% 
      drop_na(electricity_value)
  })
  
  #This dataset is use to check if a company is reporting data center fuel use
  energy_sheet_selected_company_current_year_dc_fuel <- reactive({
    data_sheet_energy_raw %>% filter(company %in% input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      slice_max(period_covered_start_date) %>% 
      #filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers") %>% 
      drop_na(fuel_1_value)
  })
  
  #This dataset is used to check what level of data center management the company is reporting (filters out total operations rows as sometimes total operations rows are marked as "self-managed)
  energy_sheet_selected_company_current_year_dc <- reactive({
    data_sheet_energy_raw %>% filter(company %in% input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      slice_max(period_covered_start_date) %>% 
      #filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Single Data Center" | energy_reporting_scope == "Multiple Data Centers")
  })
  
  #This dataset is use to check if a company is reporting company-wide electricity use
  energy_sheet_selected_company_current_year_te <- reactive({
    data_sheet_energy_raw %>% filter(company %in% input$selected_company) %>% 
      mutate(period_covered_start_date = year(period_covered_start_date)) %>% 
      slice_max(period_covered_start_date) %>% 
      # filter(period_covered_start_date == max(period_covered_start_date)) %>% 
      filter(energy_reporting_scope == "Total Operations") %>% 
      drop_na(electricity_value)
  })
  
  #########################################################
  ####### Table 1: Select company quick stats #############
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
  
  selected_company_stats <- reactive({ data.frame(A = c("Does company report any energy use?", "Year of most recent data", "Lease/cloud providers"),
                                       B = c(energy_reporting_status(), year_of_most_recent_data(), ifelse(external_service_provider_list() == "", "No External Providers Reported",
                                                                                                           external_service_provider_list())), check.names = FALSE)
  })
  
  #removed reactive table from "Select a Company To View" header box
  
  #output$selected_company_stats <- renderDataTable({
  #
  #datatable(selected_company_stats(), rownames = FALSE, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){", "  $(thead).remove();","}"))) %>% 
  #    formatStyle(columns = c(2), fontSize = '16pt', textAlign = 'center') %>% 
  #    formatStyle(columns = c(1), fontSize = '14pt', fontWeight = 'bold')
  #
  #})
  
  ##############################
  #Box 2########################
  #Company data center overview#
  ##############################
  
  data_center_modal_visible <- reactiveVal(FALSE)
  observeEvent(input$show_company_data_center_overview, data_center_modal_visible(TRUE))
  observeEvent(input$hide_company_data_center_overview, data_center_modal_visible(FALSE))
  
  output$company_data_center_overview <- renderReact({
    
    Modal(isOpen = data_center_modal_visible(),
          Stack(tokens = list(padding = "25px", childrenGap = "10px"),
                div(style = list(display = "flex"),
                    Text("Data Center Overview", variant = "xLarge"),
                    div(style = list(flexGrow = 1)),
                    IconButton.shinyInput("hide_company_data_center_overview", iconProps = list(iconName = "Cancel")),
                ),
                div(Text(company_sheet_selected_company()[1, "company_data_center_overview"], variant = "large")
                )))
  })
  
  ##############################
  #Box 3########################
  #Energy reporting assessment##
  ##############################
  
  energy_assessement_modal_visible <- reactiveVal(FALSE)
  observeEvent(input$show_company_energy_reporting_assessment_overview, energy_assessement_modal_visible(TRUE))
  observeEvent(input$hide_company_energy_reporting_assessment_overview, energy_assessement_modal_visible(FALSE))
  
  output$company_energy_reporting_assessment_overview <- renderReact({
    
    Modal(isOpen = energy_assessement_modal_visible(),
          Stack(tokens = list(padding = "25px", childrenGap = "10px"),
                div(style = list(display = "flex"),
                    Text("Energy Reporting Assessment", variant = "xLarge"),
                    div(style = list(flexGrow = 1)),
                    IconButton.shinyInput("hide_company_energy_reporting_assessment_overview", iconProps = list(iconName = "Cancel")),
                ),
                div(Text(company_sheet_selected_company()[1, "energy_reporting_assessment"], variant = "large")
                )))
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
    data.frame(Level = c("Data center electricity use", "Self-managed", "Leased", "Cloud", "Data center other fuel use", "Company-wide electricity use"),
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
  #Table 7-10 Prep. #####################
  #Interactivity and Method Table Lookup
  #######################################
  
  methodology_table_lookup <- reactive({buildCompanyProfileMethodologyTable(input$selected_company)})
  
  #Show all 5 tables to start
  observeEvent(input$selected_company, {
    shinyjs::show(selector = "div#electricity-use-table")
    shinyjs::show(selector = "div#other-fuel-use-table")
    shinyjs::show(selector = "div#ns-energy-use-table")
    shinyjs::show(selector = "div#pue-table")
    shinyjs::show(selector = "div#methodology-table")
  })
  
  #######################################
  #Table 7###############################
  #Electricity Use (TWh/yr)##############
  #######################################
  
  output$electricity_use_table <- renderDataTable({
    
    selected_company_electricity_use_filter <- buildCompanyProfileElectricityUsePlot(input$selected_company, methodology_table_lookup())
    
    #If the dataframe output from the reactive electricity dataset is not empty then insert the dataset into a datatable, else show the no_data datatable
    if(nrow(selected_company_electricity_use_filter) != 0) {
      datatable(selected_company_electricity_use_filter, escape = FALSE, rownames = FALSE, options = list(dom = 't', columnDefs = list(list(visible=FALSE, targets=0)), scrollX = TRUE)) %>% 
        formatStyle('category', 'format', textAlign = styleEqual(c(0, 1), c('right', 'left')), fontStyle = styleEqual(c(0, 1), c('italic', 'normal')))
    } else {
      shinyjs::hide(selector = "div#electricity-use-table")
    }
  })
  
  #######################################
  #Table 9###############################
  #Other Fuel Use (TWh/yr)###############
  #######################################
  
  output$other_fuel_use_table <- renderDataTable({
    
    selected_company_fuel_use_filter <- buildCompanyProfileFuelUsePlot(input$selected_company, methodology_table_lookup())
    
    if(nrow(selected_company_fuel_use_filter) != 0) {
      datatable(selected_company_fuel_use_filter, escape = FALSE, rownames = FALSE, options = list(dom = 't', columnDefs = list(list(visible=FALSE, targets=0)), scrollX = TRUE)) %>% 
        formatStyle('category', 'format', textAlign = styleEqual(c(0, 1), c('right', 'left')), fontStyle = styleEqual(c(0, 1), c('italic', 'normal')))
    } else {
      shinyjs::hide(selector = "div#other-fuel-use-table")
    }
  })
  
  #######################################
  #Table 10##############################
  #Non-specified energy use (TWh/yr)#####
  #######################################
  
  output$ns_energy_use_table <- renderDataTable({
    
    selected_company_ns_energy_use_filter <- buildCompanyProfileNonSpecifiedEnergyUsePlot(input$selected_company, methodology_table_lookup())
    
    if(nrow(selected_company_ns_energy_use_filter) != 0) {
      datatable(selected_company_ns_energy_use_filter, escape = FALSE, rownames = FALSE, options = list(dom = 't', columnDefs = list(list(visible=FALSE, targets=0)), scrollX = TRUE)) %>% 
        formatStyle('category', 'format', textAlign = styleEqual(c(0, 1), c('right', 'left')), fontStyle = styleEqual(c(0, 1), c('italic', 'normal')))
    } else {
      shinyjs::hide(selector = "div#ns-energy-use-table")
    }
  })
  
  #######################################
  #Table 11##############################
  #PUE###################################
  #######################################
  
  output$pue_table <- renderDataTable({

    selected_company_pue_filter <- buildCompanyProfilePUEPlot(input$selected_company, methodology_table_lookup())
    
    if(nrow(selected_company_pue_filter) != 0) {
      datatable(selected_company_pue_filter, escape = FALSE, rownames = FALSE, options = list(dom = 't', autoWidth = TRUE, columnDefs = list(list(width = '300px',targets = c(0))), scrollY=200, scrollCollapse=TRUE))
    } else {
      shinyjs::hide(selector = "div#pue-table")
    }
    
  })
  
  #########################################
  ### Company Transparency over Time Plot ###
  #########################################
  
  output$transparency_over_time_plot <- renderPlot({
    buildCompanyProfileTransparencyOverTimePlot(data_sheet_energy_transformed, input$selected_company)
  })
  
  #######################################
  #Table 12##############################
  # Methodology #########################
  #######################################
  
  output$methodology_table <- renderDataTable({
    
    selected_company_methodology_filter <- buildCompanyProfileMethodologyTable(input$selected_company)
    
    if(nrow(selected_company_methodology_filter) != 0) {
      
      #add "pageLength" = 100 to ensure datatable is showing up to 100 lines of methodological notes (default is 10 and any comments beyond that are hidden from the user)
      datatable(selected_company_methodology_filter, rownames = FALSE, options = list(dom = 't', autoWidth = TRUE, columnDefs = list(list(className = 'dt-center', targets = c(0, 1, 3))), scrollY=200, scrollCollapse=TRUE))
      
    } else {
      shinyjs::hide(selector = "div#methodology-table")
    }

  })
  
  #add interactivity (change page and update nav bar selection) to learn more and contact us buttons when clicked
  
  change_to_methods <- function() {
    #change page to methods
    change_page('/methods', session = shiny::getDefaultReactiveDomain(), mode = "push")
    #update selected nav
    runjs(glue("$('.ms-Nav-link[title={'Methods'}]')[0].click()"))
  }
  onclick('learn-more', change_to_methods())
  
  #change to contact us page when the report issue button is clicked
  change_to_contact_us <- function() {
    #change page to contact-us
    change_page('/contact-us', session = shiny::getDefaultReactiveDomain(), mode = "push")
    #update selected nav
    runjs(glue("$('.ms-Nav-link[title={'Contact'}]')[0].click()"))
  }
  
  onclick('report-issue', change_to_contact_us())
  
  #######################################
  #Table 13##############################
  #Sources Assessed######################
  #######################################
  
  output$sources_table <- renderDataTable({
    buildCompanyProfileSourcesAssessedTable(data_sheet_energy_transformed, input$selected_company)
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
      
      #conditionally add energy data to csv download
      if(nrow(buildCompanyProfileElectricityUsePlot(input$selected_company, methodology_table_lookup()) != 0)) {
        write.table(data.frame(x = c("", "Electricity Use (TWh/yr)")), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
        write.table(buildCompanyProfileElectricityUsePlot(input$selected_company, methodology_table_lookup())[-1], fname, col.names = TRUE, sep = ',', append = TRUE, row.names = F)}
      
      if(nrow(buildCompanyProfileFuelUsePlot(input$selected_company, methodology_table_lookup()) != 0)) {
        write.table(data.frame(x = c("", "Other fuel use (TWh/yr)")), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
        write.table(buildCompanyProfileFuelUsePlot(input$selected_company, methodology_table_lookup())[-1], fname, col.names = TRUE, sep = ',', append = TRUE, row.names = F)}
      
      if(nrow(buildCompanyProfileNonSpecifiedEnergyUsePlot(input$selected_company, methodology_table_lookup()) != 0)) {
        write.table(data.frame(x = c("", "Non-specified energy use (TWh/yr)")), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
        write.table(buildCompanyProfileNonSpecifiedEnergyUsePlot(input$selected_company, methodology_table_lookup())[-1], fname, col.names = TRUE, sep = ',', append = TRUE, row.names = F)}
      
      if(nrow(buildCompanyProfilePUEPlot(input$selected_company, methodology_table_lookup()) != 0)) {
        write.table(data.frame(x = c("", "PUE")), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
        write.table(buildCompanyProfilePUEPlot(input$selected_company, methodology_table_lookup()), fname, col.names = TRUE, sep = ',', append = TRUE, row.names = F)}
      
      if(nrow(buildCompanyProfileMethodologyTable(input$selected_company)) != 0) {
        write.table(data.frame(x = c("", "Methodological Notes")), fname, col.names = FALSE, sep = ',', append = TRUE, row.names = F)
        write.table(buildCompanyProfileMethodologyTable(input$selected_company), fname, col.names = TRUE, sep = ',', append = TRUE, row.names = F)}
      
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
  
  ########################################################
  ########################################################
  ###### Tab 6: Contact Us  ############################## 
  ########################################################
  ########################################################
  
  #Step 1: Generate and store authetication token in .secrets folder this will allow app to access to google drive drive account and write on spreadsheet (NOTE: make sure you enable Sheets API on google cloud console beforehand)
  #see this tutorial for google drive authetication process: https://www.jdtrat.com/blog/connect-shiny-google/#fn1
  
  # Set authentication token to be stored in a folder called `.secrets`
  options(gargle_oauth_cache = ".secrets")
  
  #If credentials have not been set up yet and stored in .secrets, uncomment this code and run, it should open a browser window where you give the package permission to access
  #gs4_auth()
  
  # Authenticate using stored tokens. If no browser opens, the authentication works.
  gs4_auth(cache = ".secrets", email = "isaldatacenterdashboard@gmail.com")
  
  #get spreadsheet using spreadsheet id (part of spreadsheet link)
  contact_form_gspreadsheet <- gs4_get("1IFKl40N4QqBREfU-qDecwBXZhzvq6SQKV17awtv2y_E")
  
  #sheet_append(ss, data = tibble(first_name = 'test',
  #                                        last_name = 'test',
  #                                        email = 'test',
  #                                        how_did_you_hear = 'test',
  #                                        message = 'test'))
  
  #Step 2: Collection submission from form and send to google drive
  
  contact_form_submission <- reactive({
    tibble(first_name = input$first_name_input,
           last_name = input$last_name_input,
           email = input$user_email_input,
           how_did_you_hear = input$referral_input,
           message = input$user_message_input,
           date_time_added = lubridate::now(tzone = "Canada/Pacific"))
  })

  #Step 3: Generate function to execute when user clicks submission form
  contactFormSubmission <- function() {
    
    #check if first name is blank
    if (input$first_name_input == "") { 
      show(selector = "div#missing-fields")
      addCssClass(id = "first_name_input", class = "red-border")
    } 
    
    #check if valid email is used by checking to see if an @ or . are included in the submission
    else if (!grepl("@", input$user_email_input, fixed = TRUE) || !grepl(".", input$user_email_input, fixed = TRUE)) { 
      hide(selector = "div#missing-fields")
      show(selector = "div#invalid-email")
      addCssClass(id = "user_email_input", class = "red-border")
    } 
    
    #check if message is blank
    else if (input$user_message_input == "") { 
      hide(selector = "div#invalid-email")
      show(selector = "div#missing-fields")
      addCssClass(id = "user_message_input", class = "red-border")
    } 
    
    #if no errors, submit form and hide
    else {
    hide(selector = "form#submission-form")
    hide(selector = "div#missing-fields")
    hide(selector = 'div#invalid-email')
    show(selector = "div#thank-you-for-submission")
    sheet_append(contact_form_gspreadsheet, data = contact_form_submission())
    }
    
  }
  
  #Hide thank you for submission text and missing fields text
  hide(selector = 'div#invalid-email')
  hide(selector = 'div#thank-you-for-submission')
  hide(selector = 'div#missing-fields')
  
  #when Submit button is click, run function above
  onclick('contact-form-submit', contactFormSubmission())
  
  progress$inc(.5, detail = "Final touches")
  
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