source(here("ui.R"))

#Server code
server <- function(input, output, session) {
  
  #add full screen 3 second waiter
  #w = Waiter$new()
  #w$show()
  #Sys.sleep(3)
  #w$hide() 
  
  #Tab 1
  output$years_reported <- renderUI({
    #output length of vector of unique values from report_year column
    length(unique(data_sheet_energy_raw$report_year))
    })
  
  output$companies_reporting <- renderUI({
    
    number_of_companies_reporting <- 
      data_sheet_company_raw %>% 
      filter(checked_back_to_founding_year_or_2007 == "Yes") #filter by only companies that have been checked back to 2007
    
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
  
  #Tab 2: Industry Trends
  
  ########################################################
  ###### Generate reactive datasets for sub-rendering ####
  ########################################################
  
  
  ########################
  #Dataset 1##############
  #Reporting Transparency#
  ########################
  
  # loop through all data years to create data frame for ggplot
  # IMPORTANT: manually change years in for loop conditions for future use
  
  data_of_transparency <- reactive({
  
  for (year_count in 2007:as.integer(max(na.omit(data_sheet_energy_raw$report_year)))) {
    # create a sub data frame that is filtered by data year and single data center scope
    data_of_transparency_SDC <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "Single Data Center") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_SDC <- distinct(data_of_transparency_SDC, .keep_all = TRUE)
    
    # create a sub data frame that is filtered by data year and multiple data centers scope
    data_of_transparency_MDC <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "Multiple Data Centers") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type)
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_MDC <- distinct(data_of_transparency_MDC, .keep_all = TRUE)
    
    # create a sub data frame that is filtered by data year and company wide electricity scope
    data_of_transparency_TO <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "Total Operations") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_TO <- distinct(data_of_transparency_TO, .keep_all = TRUE)
    
    # create a sub data frame that is filtered by data year (***LOOK BACK AT THIS***)
    # IMPORTANT: leave energy reporting scope column blank for this transparency level!
    data_of_transparency_ND <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type)
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_ND <- distinct(data_of_transparency_ND, .keep_all = TRUE)
    
    # stack single data center/multiple data center data frames on top of each other
    data_of_transparency_L1 <- rbind(data_of_transparency_SDC, data_of_transparency_MDC)
    # since single/multiple data centers fall under same level, diminish to 1 row per company
    data_of_transparency_L1 <- distinct(data_of_transparency_L1, company, .keep_all = TRUE)
    
    # stack rest of the data frames together
    data_of_transparency_L1 <- rbind(data_of_transparency_L1, data_of_transparency_TO,
                                     data_of_transparency_ND)
    data_of_transparency_L1 <- distinct(data_of_transparency_L1, company, .keep_all = TRUE)
    data_of_transparency_L1$value <- nrow(data_of_transparency_L1)
    
    # create the master data frame from the first index of the for loop
    # else: add the existing master data frame to the recently computed data frames
    if (year_count == 2007) {
      data_of_transparency <- rbind(data_of_transparency_L1)
    } else {
      data_of_transparency <- data_of_transparency %>% rbind(data_of_transparency_L1)
    }
    # sort graph in alphabetical order of company in order to make summation loop easier
    data_of_transparency <- data_of_transparency[order(data_of_transparency[,"data_year"]), ]
    #data_of_transparency <- distinct(data_of_transparency, company, .keep_all = TRUE)
  }
  
  # create another data frame that is filtered by company founding year
  data_founding_year <- data_sheet_company_raw %>% select(company_name, 
                                                      checked_back_to_founding_year_or_2007,
                                                      company_founding_year)
  # drop all the excess rows that have NA data
  data_founding_year <- na.omit(data_founding_year)
  data_founding_year <- data_founding_year[data_founding_year$checked_back_to_founding_year_or_2007 == "Yes", ]
  
  data_of_transparency <- subset(data_of_transparency, 
                                 company %in% data_founding_year$company_name)
  
  # assessing if there are companies in data_founding_year that ARE NOT in data_of_transparency
  for (i in 1:nrow(data_founding_year)) {
    if (!any(data_of_transparency == data_founding_year[i,1])) {
      data_of_transparency[nrow(data_of_transparency)+1, ] <- 
        c(data_founding_year[i,1], 2007, 0, 0, 0)
    }
  }
  
  # create new rows for companies that do not have L1/L2/L3 transparency in that given year
  # in other words, create rows for level 4 companies (no report found)
  data_of_transparency <- dummy_rows(data_of_transparency, 
                                     select_columns = c("company", "data_year"),
                                     dummy_value = 0)
  data_of_transparency <- data_of_transparency[order(data_of_transparency[,"data_year"]), ]
  
  # NOW we have data for some companies that didn't even exist yet!
  # take each company in data_founding year, compare with ALL rows in data_of_transparency,
  # and if the founding year of the company is greater than the data year of the company,
  # then keep track of that index in deleted_rows vector 
  deleted_rows <- vector()
  z = 1
  for (i in 1:nrow(data_founding_year)) {
    for (j in 1:nrow(data_of_transparency)) {
      if (data_founding_year[i,1] == data_of_transparency[j,1]) {
        if (data_founding_year[i,3] > data_of_transparency[j,2]) {
          deleted_rows[z] <- j
          z <- z + 1
        }
      }
    }
  }
  
  # drop all rows that have company data prior to their founding year
  if (length(deleted_rows) != 0) {
    data_of_transparency <- data_of_transparency[-c(deleted_rows), ]
  }
  
  data_of_transparency$value <- 0
  data_of_transparency$number_of_companies <- 0
  data_of_transparency <- data_of_transparency %>% select(data_year, energy_reporting_scope, 
                                                          fuel_1_type, value, number_of_companies)
  
  # temporary garbage values 
  year_dummy = 2006
  report_dummy = "Hi"
  
  # loop through all rows and rename energy reporting scopes
  j = 0
  for (i in 1:nrow(data_of_transparency)) {
    if (data_of_transparency[i,2] == "Single Data Center" || data_of_transparency[i,2] == "Multiple Data Centers") {
      data_of_transparency[i,2] <- "Reported Data Center Electricity"
    } else if (data_of_transparency[i,2] == "Total Operations" && data_of_transparency[i,3] != "Total Energy Use") {
      data_of_transparency[i,2] <- "Reported Company Wide Electricity"
    } else if (data_of_transparency[i,2] == "Total Operations" && data_of_transparency[i,3] == "Total Energy Use") {
      data_of_transparency[i,2] <- "Reported Company Wide Total Energy"
    } else {
      data_of_transparency[i,2] <- "No Reporting of Data"
    }
  }
  
  data_of_transparency <- data_of_transparency[order(data_of_transparency[,"data_year"],
                                                     data_of_transparency[,"energy_reporting_scope"]), ]
  
  for (i in 1:nrow(data_of_transparency)) {
    # sum the number rows based on company name and energy reporting scope
    # keeping track of the summation at the first instance of a distinct row
    if (data_of_transparency[i,1] == year_dummy && data_of_transparency[i,2] == report_dummy) {
      data_of_transparency[i-j,4] <- data_of_transparency[i-j,4] + 1
      j <- j + 1
    } else {
      year_dummy <- data_of_transparency[i,1]
      report_dummy <- data_of_transparency[i,2]
      data_of_transparency[i,4] <- 1
      j <- 1
    }
  }
  
  # drop the rest of the rows with value of 0
  #data_of_transparency <- data_of_transparency[order(data_of_transparency[,""]), ]
  #data_of_transparency <- distinct(data_of_transparency, data_year, .keep_all = TRUE)
  
  # find line graph values
  year_dummy = 2006
  j = 0
  for (i in 1:nrow(data_of_transparency)) {
    if (data_of_transparency[i,1] == year_dummy) {
      data_of_transparency[i-j,5] = data_of_transparency[i-j,5] + data_of_transparency[i,4]
      j = j + 1
    } else {
      year_dummy = data_of_transparency[i,1]
      data_of_transparency[i,5] = data_of_transparency[i,4]
      j = 1
    }
  }
  
  # assign line graph values to each row with same year
  year_dummy = 2006
  for (i in 1:nrow(data_of_transparency)) {
    if (data_of_transparency[i,1] == year_dummy) {
      data_of_transparency[i,5] = data_of_transparency[i-1,5]
    } else {
      year_dummy = data_of_transparency[i,1]
    }
  }
  
  data_of_transparency
  })
  
  ########################
  # Dataset 2 ############
  # Reported Energy Use ##
  ########################
  
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
    filter(data_year == input$input_year, energy_reporting_scope == input$input_reporting_scope)
  
  energy_use_final
  
  })
  
  ########################
  #Graph 1################
  #Reporting Transparency#
  ########################
  
  output$transparency_graph <- renderPlotly({
  
  p <- ggplot(data_of_transparency(), aes(x=data_year)) + 
    geom_bar(aes(y=value, fill=energy_reporting_scope),
             position=position_stack(reverse = TRUE), stat="identity") +
    #geom_line(aes(y=number_of_companies), size=1.2, color="black") +
    ggtitle("Data Center Transparency") +
    scale_fill_manual(values = c("#E9967A", "#8FBC8F", "#00CED1", "#1E90FF")) +
    xlab("Year") +
    ylab("Number of Companies") +
    labs(fill = "Energy Reporting Scope")
  
  ggplotly(p)
  
  })
  
  ########################
  #Graph 2################
  # Reported Energy Use ##
  ########################
  
  output$energy_use_aggregated <- renderPlotly({
    
    if ("Data Centers" %in% energy_use_final()$energy_reporting_scope) {
      
      # create plot for data center electricity usage
      dc <- ggplot(energy_use_final(), aes(x=electricity_converted)) + 
        geom_bar(aes(y=company, fill=level_of_ownership), 
                 position=position_stack(reverse = TRUE), stat="identity") +
        ggtitle("Data Center Electricity Use by Year") +
        theme_classic() +
        scale_y_discrete(position = "right") +
        scale_x_continuous() +
        theme(legend.position = "left") +
        xlab("Electricity Value (KWh)") +
        ylab("Companies") +
        labs(fill = "Level Of Ownership") 
      ggplotly(dc)
    } 
    
    else {
      energy_use_final <- energy_use_final()
      deleted_rows_1 <- vector()
      deleted_rows_2 <- vector()
      deleted_rows_3 <- vector()
      deleted_rows_4 <- vector()
      deleted_rows_5 <- vector()
      a <- 1
      b <- 1
      c <- 1
      d <- 1
      e <- 1
      for (i in 1:nrow(energy_use_final)) {
        if (energy_use_final[i,5] > 10000000000) {
          deleted_rows_1[a] <- i
          a <- a + 1
        } else if (energy_use_final[i,5] > 1000000000 && energy_use_final[i,5] < 10000000000) {
          deleted_rows_2[b] <- i
          b <- b + 1
        } else if (energy_use_final[i,5] > 100000000 && energy_use_final[i,5] < 1000000000) {
          deleted_rows_3[c] <- i
          c <- c + 1
        } else if (energy_use_final[i,5] > 10000000 && energy_use_final[i,5] < 100000000) {
          deleted_rows_4[d] <- i
          d <- d + 1
        } else {
          deleted_rows_5[e] <- i
          e <- e + 1
        }
      }
      
      energy_use_L2_1 <- energy_use_final[c(deleted_rows_1), ]
      energy_use_L2_2 <- energy_use_final[c(deleted_rows_2), ]
      energy_use_L2_3 <- energy_use_final[c(deleted_rows_3), ]
      energy_use_L2_4 <- energy_use_final[c(deleted_rows_4), ]
      energy_use_L2_5 <- energy_use_final[c(deleted_rows_5), ]
      
      p3a <- plot_ly() %>% 
        add_trace(x=energy_use_L2_1$electricity_converted, 
                  y=reorder(energy_use_L2_1$company, energy_use_L2_1$electricity_converted), 
                  type="bar", width = 0.5, marker = list(color = "rgb(255,127,80)")) 
      
      p3b <- plot_ly() %>% 
        add_trace(x=energy_use_L2_2$electricity_converted, 
                  y=reorder(energy_use_L2_2$company, energy_use_L2_2$electricity_converted),
                  type="bar", width = 0.8, marker = list(color = "rgb(255,255,0)"))
      
      p3c <- plot_ly() %>% 
        add_trace(x=energy_use_L2_3$electricity_converted, 
                  y=reorder(energy_use_L2_3$company, energy_use_L2_3$electricity_converted),
                  type="bar", width = 0.8, marker = list(color = "rgb(0,250,154)"))
      
      p3d <- plot_ly() %>% 
        add_trace(x=energy_use_L2_4$electricity_converted, 
                  y=reorder(energy_use_L2_4$company, energy_use_L2_4$electricity_converted),
                  type="bar", width = 0.5, marker = list(color = "rgb(0,191,255)")) 
      
      p3e <- plot_ly() %>% 
        add_trace(x=energy_use_L2_5$electricity_converted, 
                  y=reorder(energy_use_L2_5$company, energy_use_L2_5$electricity_converted),
                  type="bar", width = 0.5, marker = list(color = "rgb(128,0,128)"))
      
      p3 <- subplot(p3a,p3b,p3c,p3d,p3e, nrows = 5, margin = 0.05)
      
      p3 <- p3 %>% add_annotations(
        x = 0.5,  
        y = 0.77,  
        text = "10s of TWh",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE) %>%
        add_annotations(
          x = 0.5,  
          y = 0.575,  
          text = "1s of TWh",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE
        ) %>%
        add_annotations(
          x = 0.5,  
          y = 0.35,  
          text = "100s of GWh",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE
        ) %>%
        add_annotations(
          x = 0.5,  
          y = 0.15,  
          text = "10s of GWh",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE
        ) %>%
        add_annotations(
          x = 0.5,  
          y = -0.1,  
          text = "KWh to 1s of GWh",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE
        )
      
      ggplotly(p3)
      
    }
  })
  
  #Tab 3: Company Analysis
  
  ########################################################
  ###### Generate reactive datasets for sub-rendering ####
  ########################################################
  
  #This dataset filters the raw company profiles sheet by the selected company's current year
  company_sheet_selected_company <- reactive({
    data_sheet_company_raw %>% filter(company_name == input$selected_company)
  })
  
  #This dataset filters the raw energy sheet by the selected company's most recent year of data reporting
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
    str_remove_all(", NA") %>% str_remove_all(", ,") %>% str_remove_all(" , ") %>% str_remove_all("[:punct:]*\\s*$") %>% 
    str_remove_all("NA")
  
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
    
    reported_energy_levels_data <- 
      data.frame(Level = c("Data center electricity use", "Self-managed", "Leased", "Cloud", "Data center other fuel use", "Company-wide electricty use"),
               " Reporting Status " = c(data_center_electricity_reporting_status, data_center_self_managed_reporting_status, data_center_leased_reporting_status, data_center_cloud_reporting_status, data_center_other_fuel_use_reporting_status, total_energy_reporting_status), check.names = FALSE) %>% 
      add_column(format = c(1,0,0,0,1,1), .before = 'Level')
    
    datatable(reported_energy_levels_data, rownames = FALSE, options = list(dom = 't', columnDefs = list(list(visible=FALSE, targets=0)))) %>% 
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
  
  no_data <- data.frame(no_data_reported = "No data reported")
  `%not_in%` <- purrr::negate(`%in%`)

  selected_company_electricity_use <- reactive({
    
    selected_company_electricity_use_filter <- 
      data_sheet_energy_transformed %>% 
      filter(company == input$selected_company) %>% #filter by selected company
      mutate_at(vars(electricity_converted), ~replace_na(., 0)) %>% #replace any NA electricity values with 0
      mutate_at(vars(level_of_ownership), ~replace_na(., "")) %>% #replace any NA level of ownership values with 0
      filter(electricity_converted != 0) %>% #filter out any rows where reported electricity is equal to 0
      filter(level_of_ownership != "Cloud") %>% #filter out any rows where reported electricity is from Cloud providers
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
        energy_reporting_scope %in% "Data center electricity use | Self-managed" ~ "Self_managed",
        energy_reporting_scope %in% "Data center electricity use | " ~ "Self_managed", #If no level of ownership is given, assume self managed
        energy_reporting_scope %in% "Company-wide electricity use | Self-managed" ~ "Total_company",
        energy_reporting_scope %in% "Company-wide electricity use | " ~ "Total_company")) %>% #If no level of ownership is given, assume self managed
      pivot_wider(names_from = energy_reporting_scope, values_from = value) %>% 
      replace(is.na(.), 0) %>% 
      mutate(Leased = ifelse("Leased" %in% names(.), Leased, 0),
           Self_managed = ifelse("Self_managed" %in% names(.), Self_managed, 0),
           Total_company = ifelse("Total_company" %in% names(.), Total_company, 0)) %>%
      mutate(data_centers = Self_managed + Leased) %>% 
      mutate(data_center_percentage = data_centers/(data_centers + Total_company)) %>%
      pivot_longer(!data_year, names_to = "category", values_to = "value") %>% 
     # mutate(value = ifelse(category == 'data_center_percentage', percent(value), value))
      pivot_wider(names_from = data_year, values_from = value) %>% 
      mutate(category = case_when(
        category %in% "Total_company" ~ "Total company",
        category %in% "Self_managed" ~ "Self-managed",
        category %in% "Leased" ~ "Leased",
        category %in% "data_centers" ~ "Data centers",
        category %in% "data_center_percentage" ~ "Data center % of total electricity")) %>% 
      mutate_if(is.numeric, ~round(., 3))
    
      #If the dataframe contains at least 1 row, perform the following function
      if (nrow(selected_company_electricity_use_filter != 0)) {
        selected_company_electricity_use_filter <- 
          selected_company_electricity_use_filter %>% 
          mutate(category = factor(category, levels = c("Data centers", "Self-managed", "Leased", "Total company", "Data center % of total electricity"))) %>%
          arrange(category) %>% 
          add_column(format = c(1,0,0,1,1), .before = 'category')
        
        possible_years_electricity <- c(2007:as.integer(tail(colnames(selected_company_electricity_use_filter), n=1)))
        extra_years_electricity <- list()
        
        for (i in 1:length(possible_years_electricity)) {
          if (possible_years_electricity[i] %not_in% names(selected_company_electricity_use_filter)) {
            extra_years_electricity[i] <- possible_years_electricity[i]
          }
        }
        
        selected_company_electricity_use_filter %>% 
          add_column(!!!set_names(as.list(rep(0, length(extra_years_electricity))),nm=extra_years_electricity)) %>% 
          select(sort(tidyselect::peek_vars())) %>% 
          relocate(c(format, category), .before = '2007') %>% 
          mutate_if(is.numeric, ~ifelse(. == 0, "", .))
      }
  })
  
  output$electricity_use_table <- renderDataTable({
    
    #If the dataframe output from the reactive electricity dataset is not empty then insert the dataset into a datatable, else show the no_data datatable
    if(!is.null(selected_company_electricity_use())) {
    
    datatable(selected_company_electricity_use(), rownames = FALSE, options = list(columnDefs = list(list(visible=FALSE, targets=0)), scrollX = TRUE)) %>% 
      formatStyle(
        'category', 'format',
        textAlign = styleEqual(c(0, 1), c('right', 'left')),
        fontStyle = styleEqual(c(0, 1), c('italic', 'normal'))
      )
    } else {
      datatable(no_data, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){",
                                                                       "  $(thead).remove();",
                                                                       "}")), rownames = FALSE)
    }
    
  })
  
  #######################################
  #Table 9###############################
  #Other Fuel Use (TWh/yr)###############
  #######################################
  
  selected_company_fuel_use <- reactive({
    
    selected_company_fuel_use_filter <- 
      data_sheet_energy_transformed %>% 
      filter(company == "Facebook") %>% 
      mutate_at(vars(fuel_1_converted, #replace na values with 0
                     fuel_2_converted, fuel_3_converted, fuel_4_converted, 
                     fuel_5_converted), ~replace_na(., 0)) %>%
      rowwise() %>% 
      mutate(total_other_energy_use = sum(c(fuel_1_converted,
                                            fuel_2_converted, fuel_3_converted, fuel_4_converted, 
                                            fuel_5_converted))) %>%
      filter(level_of_ownership != "Cloud") %>% 
      select("data_year", "energy_reporting_scope", "level_of_ownership", "total_other_energy_use") %>%
      filter(total_other_energy_use != 0) %>%
      filter(energy_reporting_scope == "Multiple Data Centers" | energy_reporting_scope == "Single Data Center") %>% 
      mutate(energy_reporting_scope = case_when(
        energy_reporting_scope %in% c("Multiple Data Centers", "Single Data Center") ~ "Data center other fuel use")) %>% 
      group_by(data_year, energy_reporting_scope, level_of_ownership) %>% 
      summarize(value = sum(total_other_energy_use)) %>% 
      mutate(value = value/1000000000) %>%
      unite(energy_reporting_scope, energy_reporting_scope:level_of_ownership, sep = " | ") %>% 
      mutate(energy_reporting_scope = case_when(
        energy_reporting_scope %in% "Data center other fuel use | Leased" ~ "Leased",
        energy_reporting_scope %in% "Data center other fuel use | Cloud" ~ "Cloud",
        energy_reporting_scope %in% "Data center other fuel use | Self-managed" ~ "Self_managed",
        energy_reporting_scope %in% "Data center other fuel use | " ~ "Self_managed", #If no level of ownership is given, assume self managed
        energy_reporting_scope %in% "Data center other fuel use | NA" ~ "Self_managed")) %>%
      pivot_wider(names_from = energy_reporting_scope, values_from = value) %>% 
      replace(is.na(.), 0) %>% 
      mutate(Leased = ifelse("Leased" %in% names(.), Leased, 0),
             Self_managed = ifelse("Self_managed" %in% names(.), Self_managed, 0)) %>%
      mutate(data_centers = Self_managed + Leased) %>% 
      pivot_longer(!data_year, names_to = "category", values_to = "value") %>% 
      pivot_wider(names_from = data_year, values_from = value) %>% 
      mutate(category = case_when(
        category %in% "Self_managed" ~ "Self-managed",
        category %in% "Leased" ~ "Leased",
        category %in% "data_centers" ~ "Data centers")) %>% 
      mutate_if(is.numeric, ~round(., 3))
    
    #If the number of rows in the dataframe is not equal to 0
    if (nrow(selected_company_fuel_use_filter != 0)) {
      selected_company_fuel_use_filter <- 
        selected_company_fuel_use_filter %>% 
        mutate(category = factor(category, levels = c("Data centers", "Self-managed", "Leased"))) %>% 
        arrange(category) %>% 
        add_column(format = c(1,0,0), .before = 'category')
      
      possible_years_fuel <- c(2007:as.integer(tail(colnames(selected_company_fuel_use_filter), n=1)))
      extra_years_fuel <- list()
      
      for (i in 1:length(possible_years_fuel)) {
        if (possible_years_fuel[i] %not_in% names(selected_company_fuel_use_filter)) {
          extra_years_fuel[i] <- possible_years_fuel[i]
        }
      }
      
      selected_company_fuel_use_filter %>% 
        add_column(!!!set_names(as.list(rep(0, length(extra_years_fuel))),nm=extra_years_fuel)) %>% 
        select(sort(tidyselect::peek_vars())) %>% 
        relocate(c(format, category), .before = '2007') %>% 
        mutate_if(is.numeric, ~ifelse(. == 0, "", .))
    }
    
  })
  
  output$other_fuel_use_table <- renderDataTable({
    
    if(!is.null(selected_company_fuel_use())) {
      
      datatable(selected_company_fuel_use(), rownames = FALSE, options = list(columnDefs = list(list(visible=FALSE, targets=0)), scrollX = TRUE)) %>% 
        formatStyle(
          'category', 'format',
          textAlign = styleEqual(c(0, 1), c('right', 'left')),
          fontStyle = styleEqual(c(0, 1), c('italic', 'normal'))
        )
    } else {
      datatable(no_data, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){",
                                                                       "  $(thead).remove();",
                                                                       "}")), rownames = FALSE)
    }
    
  })
  
  #######################################
  #Table 10##############################
  #Non-specified energy use (TWh/yr)#####
  #######################################
  
  selected_company_ns_energy_use <- reactive({
    
    selected_company_ns_energy_use_filter <-
      data_sheet_energy_transformed %>%
      filter(company == input$selected_company, fuel_1_type == "Total Energy Use") %>% 
      mutate_at(vars(fuel_1_type), ~replace_na(., 0)) %>%
      filter(fuel_1_type != 0) %>%
      select("data_year", "energy_reporting_scope", "level_of_ownership", "fuel_1_type", "fuel_1_converted") %>% 
      mutate(energy_reporting_scope = case_when(
        energy_reporting_scope %in% c("Multiple Data Centers", "Single Data Center") ~ "Data center energy use",
        energy_reporting_scope %in% "Total Operations"                               ~ "Company-wide energy use")) %>% 
      group_by(data_year, energy_reporting_scope, level_of_ownership) %>% 
      summarize(value = sum(fuel_1_converted)) %>% 
      mutate(value = value/1000000000) %>%
      unite(energy_reporting_scope, energy_reporting_scope:level_of_ownership, sep = " | ") %>% 
      mutate(energy_reporting_scope = case_when(
        energy_reporting_scope %in% "Data center energy use | Leased" ~ "Leased",
        energy_reporting_scope %in% "Data center energy use | Cloud" ~ "Cloud",
        energy_reporting_scope %in% "Data center energy use | Self-managed" ~ "Self_managed",
        energy_reporting_scope %in% "Data center energy use | " ~ "Self_managed", #If no level of ownership is given, assume self managed
        energy_reporting_scope %in% "Data center energy use | NA" ~ "Self_managed", #If no level of ownership is given, assume self managed
        energy_reporting_scope %in% "Company-wide energy use | Self-managed" ~ "Total_company",
        energy_reporting_scope %in% "Company-wide energy use | " ~ "Total_company",
        energy_reporting_scope %in% "Company-wide energy use | NA" ~ "Total_company")) %>%
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
        category %in% "Total_company" ~ "Total company",
        category %in% "Self_managed" ~ "Self-managed",
        category %in% "Leased" ~ "Leased",
        category %in% "data_centers" ~ "Data centers",
        category %in% "data_center_percentage" ~ "Data center % of total electricity")) %>% 
      mutate_if(is.numeric, ~round(., 3))
    
    #If the number of rows in the dataframe is not equal to 0
    if (nrow(selected_company_ns_energy_use_filter != 0)) {
      
      selected_company_ns_energy_use_filter <- 
        selected_company_ns_energy_use_filter %>% 
        mutate(category = factor(category, levels = c("Data centers", "Self-managed", "Leased", "Total company", "Data center % of total electricity"))) %>% 
        arrange(category) %>% 
        add_column(format = c(1,0,0,1,1), .before = 'category')
      
      possible_years_ns_energy <- c(2007:as.integer(tail(colnames(selected_company_ns_energy_use_filter), n=1)))
      extra_years_ns_energy <- list()
      
      for (i in 1:length(possible_years_ns_energy)) {
        if (possible_years_ns_energy[i] %not_in% names(selected_company_ns_energy_use_filter)) {
          extra_years_ns_energy[i] <- possible_years_ns_energy[i]
        }
      }
      
      selected_company_ns_energy_use_filter %>% 
        add_column(!!!set_names(as.list(rep(0, length(extra_years_ns_energy))),nm=extra_years_ns_energy)) %>% 
        select(sort(tidyselect::peek_vars())) %>% 
        relocate(c(format, category), .before = '2007') %>% 
        mutate_if(is.numeric, ~ifelse(. == 0, "", .))
    }
    
  })
    
  output$ns_energy_use_table <- renderDataTable({
    
    if(!is.null(selected_company_ns_energy_use())) {
      
      datatable(selected_company_ns_energy_use(), rownames = FALSE, options = list(columnDefs = list(list(visible=FALSE, targets=0)), scrollX = TRUE)) %>% 
        formatStyle(
          'category', 'format',
          textAlign = styleEqual(c(0, 1), c('right', 'left')),
          fontStyle = styleEqual(c(0, 1), c('italic', 'normal'))
        )
    } else {
      datatable(no_data, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){",
                                                                       "  $(thead).remove();",
                                                                       "}")), rownames = FALSE)
    }
  })
  
  #######################################
  #Table 11##############################
  #PUE###################################
  #######################################
  
  selected_company_pue <- reactive({
    selected_company_pue_filter <-
      data_sheet_pue_raw %>% 
        filter(company == input$selected_company) %>% 
        select("applicable_year", "facility_scope", "geographical_scope", "pue_value") %>% 
        unite(pue_facility_geographic, facility_scope:geographical_scope, sep = " | ") %>% 
        pivot_wider(names_from = applicable_year, values_from = pue_value, names_sort = TRUE) %>%
        mutate_all(~replace(., is.nan(.), NA)) %>% 
        mutate_if(is.numeric, as.character) %>% 
        replace(is.na(.), "") %>% 
        rename(c("Facility Scope and Location" = pue_facility_geographic))
    
    if (nrow(selected_company_pue_filter != 0)) {
      
      possible_years_pue <- c(2007:as.integer(tail(colnames(selected_company_pue_filter), n=1)))
      extra_years_pue <- list()
      
      for (i in 1:length(possible_years_pue)) {
        if (possible_years_pue[i] %not_in% names(selected_company_pue_filter)) {
          extra_years_pue[i] <- possible_years_pue[i]
        }
      }
      
      selected_company_pue_filter %>% 
        add_column(!!!set_names(as.list(rep(0, length(extra_years_pue))),nm=extra_years_pue)) %>% 
        select(sort(tidyselect::peek_vars())) %>% 
        relocate(c("Facility Scope and Location"), .before = '2007') %>% 
        mutate_if(is.numeric, ~ifelse(. == 0, "", .))
    }
    
    
  })
  
  output$pue_table <- renderDataTable({ #Tried to change width of first column, currently not working

    if(!is.null(selected_company_pue())) {
      
      datatable(selected_company_pue(), rownames = FALSE, options = list(pageLength = 5, autoWidth = TRUE, columnDefs = list(list(width = '300px',
                                                                                                                                  targets = c(0)))))
    } else {
      datatable(no_data, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){",
                                                                       "  $(thead).remove();",
                                                                       "}")), rownames = FALSE)
    }
    
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