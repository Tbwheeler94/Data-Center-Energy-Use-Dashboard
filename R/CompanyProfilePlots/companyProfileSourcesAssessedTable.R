buildCompanyProfileSourcesAssessedTable <- function(data_sheet_energy_transformed, selected_company) {

  #stack sources columns on top of each other
  source_assessed_1 <- data_sheet_energy_transformed %>% 
    # filter(company == "Akamai") %>%
    filter(company %in% selected_company) %>%
    select(c("data_year", "report_1_type", "did_report_1_provide_electricity_or_fuel_use_data", "link_to_report_1_on_company_website", "name_of_report_1_on_box")) %>%
    rename(report_type = report_1_type, yes_no = did_report_1_provide_electricity_or_fuel_use_data, link = link_to_report_1_on_company_website, name = name_of_report_1_on_box) %>%
    mutate(name = paste0("<a href=", link, ">", name, "</a> <br/>")) %>%
    subset(report_type != "")
  
  source_assessed_2 <- data_sheet_energy_transformed %>% 
    # filter(company == "Akamai") %>%
    filter(company %in% selected_company) %>%
    select(c("data_year", "report_2_type", "did_report_2_provide_electricity_or_fuel_use_data", "link_to_report_2_on_company_website", "name_of_report_2_on_box")) %>% 
    rename(report_type = report_2_type, yes_no = did_report_2_provide_electricity_or_fuel_use_data, link = link_to_report_2_on_company_website, name = name_of_report_2_on_box) %>%
    mutate(name = paste0("<a href=", link, ">", name, "</a> <br/>")) %>%
    subset(report_type != "")
  
  source_assessed_3 <- data_sheet_energy_transformed %>% 
    # filter(company == "Akamai") %>%
    filter(company %in% selected_company) %>%
    select(c("data_year", "report_3_type", "did_report_3_provide_electricity_or_fuel_use_data", "link_to_report_3_on_company_website", "name_of_report_3_on_box")) %>% 
    rename(report_type = report_3_type, yes_no = did_report_3_provide_electricity_or_fuel_use_data, link = link_to_report_3_on_company_website, name = name_of_report_3_on_box) %>%
    mutate(name = paste0("<a href=", link, ">", name, "</a> <br/>")) %>%
    subset(report_type != "")
  
  source_assessed_4 <- data_sheet_energy_transformed %>% 
    # filter(company == "Akamai") %>%
    filter(company %in% selected_company) %>%
    select(c("data_year", "report_4_type", "did_report_4_provide_electricity_or_fuel_use_data", "link_to_report_4_on_company_website", "name_of_report_4_on_box")) %>% 
    rename(report_type = report_4_type, yes_no = did_report_4_provide_electricity_or_fuel_use_data, link = link_to_report_4_on_company_website, name = name_of_report_4_on_box) %>%
    mutate(name = paste0("<a href=", link, ">", name, "</a> <br/>")) %>%
    subset(report_type != "")
  
  source_assessed_5 <- data_sheet_energy_transformed %>% 
    # filter(company == "Akamai") %>%
    filter(company %in% selected_company) %>%
    select(c("data_year", "report_5_type", "did_report_5_provide_electricity_or_fuel_use_data", "link_to_report_5_on_company_website", "name_of_report_5_on_box")) %>% 
    rename(report_type = report_5_type, yes_no = did_report_5_provide_electricity_or_fuel_use_data, link = link_to_report_5_on_company_website, name = name_of_report_5_on_box) %>%
    mutate(name = paste0("<a href=", link, ">", name, "</a> <br/>")) %>%
    subset(report_type != "")
  
  # stack sources columns on top of each other and spread the table (using the report types as new column names)
  sources_assessed <- 
    rbind(source_assessed_1, source_assessed_2, source_assessed_3, source_assessed_4, source_assessed_5) %>%
    drop_na(report_type) %>% distinct() %>% 
    select(-c(link)) %>%
    group_by(data_year, report_type, yes_no) %>%
    summarise(name = str_c(name, collapse="\n")) %>%
    mutate(row = row_number()) %>%
    #because there are blank rows in report_type column, R does not know what to name this column after pivoting and throws the error (Error: Column 3 must be named. Use .name_repair to specify repair.)
    pivot_wider(names_from = report_type, values_from = name) %>%
    select(-c(yes_no, row)) %>%
    gather(key, value, -c(data_year)) %>%
    na.omit() %>% 
    distinct(data_year, key, .keep_all = TRUE) %>%
    spread(key, value) 
  
  for (i in 1:nrow(sources_assessed)) {
    for (j in 2:ncol(sources_assessed)) {
      if (is.na(sources_assessed[i,j])) {
        if (colnames(sources_assessed)[j] != "Other") {
          sources_assessed[i,j] <- paste0("No ", colnames(sources_assessed)[j], " Offered for this Year")
        } else {
          sources_assessed[i,j] <- paste0("No ", colnames(sources_assessed)[j], " Report Offered for this Year")
        }
      }
    }
  }
  
  # replicate the above process but for yes/no data (if company provided electricity/fuel use data)
  yes_no_table <- 
    rbind(source_assessed_1, source_assessed_2, source_assessed_3, source_assessed_4, source_assessed_5) %>%
    drop_na(report_type) %>% distinct() %>% 
    #because there are blank rows in report_type column, R does not know what to name this column after pivoting and throws the error (Error: Column 3 must be named. Use .name_repair to specify repair.)
    pivot_wider(names_from = report_type, values_from = yes_no) %>%
    select(-c(link, name)) %>%
    gather(key, value, -c(data_year)) %>%
    na.omit() %>% 
    distinct(data_year, key, .keep_all = TRUE) %>%
    spread(key, value) %>%
    rename_with(toupper)
  
  # reorder sources_assessed columns based on this order
  col_order <- c("data_year", "ESG Report", "CSR Report", "Annual Report", "Web Page", "Data Sheet", "SEC Filing", "Other")
  col_order_upper <- toupper(col_order)
  dummy_report_type <- which(!(col_order %in% colnames(sources_assessed)))
  report_not_found <- paste(col_order[dummy_report_type], collapse=", ")
  message <- paste0("List of Report Types Not Found for ", selected_company, ": ", report_not_found)
  # for (i in 1:length(dummy_report_type)) {
  #   if (col_order[dummy_report_type[i]] != "Other") {
  #     sources_assessed[col_order[dummy_report_type[i]]] <- paste0("No ", col_order[dummy_report_type[i]], " Offered for this Year")
  #   } else {
  #     sources_assessed[col_order[dummy_report_type[i]]] <- paste0("No ", col_order[dummy_report_type[i]], " Report Offered for this Year")
  #   }
  #   yes_no_table[col_order_upper[dummy_report_type[i]]] <- ""
  # }
  # sources_assessed <- sources_assessed[,col_order]
  # yes_no_table <- yes_no_table[,col_order_upper]
  
  # combine two data frames but keep track of no. of columns before and after cbind
  initial_column_count <- ncol(sources_assessed)
  sources_assessed <- cbind(sources_assessed, yes_no_table)
  final_column_count <- ncol(sources_assessed)
  
  # earliest_year <- as.integer(sources_assessed[1, 1] - 1)
  # for (i in earliest_year:2007) {
  #   sources_assessed[nrow(sources_assessed)+1, ] <- 
  #     list(i,"No ESG Report Offered for this Year",
  #          "No CSR Report Offered for this Year",
  #          "No Annual Report Offered for this Year",
  #          "No Web Page Offered for this Year",
  #          "No Data Sheet Offered for this Year",
  #          "No SEC Filing Offered for this Year",
  #          "No Other Report Offered for this Year",
  #          i,"","","","","","","")
  # }
  sources_assessed <- sources_assessed[order(sources_assessed$`data_year`, decreasing = TRUE), ]
  
  # create column index vectors to be used for formatStyle function
  columns_displayed <- 2:initial_column_count
  columns_hidden <- (initial_column_count):(final_column_count-1)
  columns_value <- (initial_column_count+2):(final_column_count)
  columns_centered <- 1:initial_column_count
  
  colnames(sources_assessed) [1] <- "Data Year"
  
  # produce HTML table widget using DT library
  datatable(sources_assessed, rownames = FALSE, options = list(dom = 't', lengthMenu = list(c(-1), c("All")), columnDefs = list(list(targets=0, className = "dt-left"), list(targets=columns_centered, className = "dt-center"), list(targets=columns_hidden, visible=FALSE)), scrollY=300, scrollCollapse=TRUE), 
            caption = paste("Green indicates if report provided electricity or fuel use data; Red indicates if report did not provide electricity or fuel use data. ", message), escape = FALSE) %>%
    formatStyle(columns = columns_displayed, 
                valueColumns = columns_value, target = 'cell',
                backgroundColor = styleEqual(c("Yes", "No"), c("#90ee90", "#ff6c70")))
}