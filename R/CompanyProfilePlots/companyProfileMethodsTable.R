buildCompanyProfileMethodsTable <- function(data_sheet_energy_transformed, selected_company) {

  #stack sources columns on top of each other
  source_assessed_1 <- data_sheet_energy_transformed %>% 
    # filter(company == "Google") %>%
    filter(company %in% selected_company) %>%
    select(c("data_year", "report_1_type", "did_report_1_provide_electricity_or_fuel_use_data", "link_to_report_1_on_company_website", "name_of_report_1_on_box")) %>%
    rename(report_type = report_1_type, yes_no = did_report_1_provide_electricity_or_fuel_use_data, link = link_to_report_1_on_company_website, name = name_of_report_1_on_box) %>%
    mutate(name = paste0("<a href=", link, ">", name, "</a> <br/>")) %>%
    subset(report_type != "")
  
  source_assessed_2 <- data_sheet_energy_transformed %>% 
    # filter(company == "Google") %>%
    filter(company %in% selected_company) %>%
    select(c("data_year", "report_2_type", "did_report_2_provide_electricity_or_fuel_use_data", "link_to_report_2_on_company_website", "name_of_report_2_on_box")) %>% 
    rename(report_type = report_2_type, yes_no = did_report_2_provide_electricity_or_fuel_use_data, link = link_to_report_2_on_company_website, name = name_of_report_2_on_box) %>%
    mutate(name = paste0("<a href=", link, ">", name, "</a> <br/>")) %>%
    subset(report_type != "")
  
  source_assessed_3 <- data_sheet_energy_transformed %>% 
    # filter(company == "Google") %>%
    filter(company %in% selected_company) %>%
    select(c("data_year", "report_3_type", "did_report_3_provide_electricity_or_fuel_use_data", "link_to_report_3_on_company_website", "name_of_report_3_on_box")) %>% 
    rename(report_type = report_3_type, yes_no = did_report_3_provide_electricity_or_fuel_use_data, link = link_to_report_3_on_company_website, name = name_of_report_3_on_box) %>%
    mutate(name = paste0("<a href=", link, ">", name, "</a> <br/>")) %>%
    subset(report_type != "")
  
  source_assessed_4 <- data_sheet_energy_transformed %>% 
    # filter(company == "Google") %>%
    filter(company %in% selected_company) %>%
    select(c("data_year", "report_4_type", "did_report_4_provide_electricity_or_fuel_use_data", "link_to_report_4_on_company_website", "name_of_report_4_on_box")) %>% 
    rename(report_type = report_4_type, yes_no = did_report_4_provide_electricity_or_fuel_use_data, link = link_to_report_4_on_company_website, name = name_of_report_4_on_box) %>%
    mutate(name = paste0("<a href=", link, ">", name, "</a> <br/>")) %>%
    subset(report_type != "")
  
  source_assessed_5 <- data_sheet_energy_transformed %>% 
    # filter(company == "Google") %>%
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
    spread(key, value) %>%
    replace(is.na(.), "No Report Type Offered For This Year")
  
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
  indices <- which(col_order %in% colnames(sources_assessed))
  col_order <- col_order[indices]
  sources_assessed <- sources_assessed[,col_order]
  
  # reorder yes_no_table columns based on this order
  col_order <- c("DATA_YEAR", "ESG REPORT", "CSR REPORT", "ANNUAL REPORT", "WEB PAGE", "DATA SHEET", "SEC FILING", "OTHER")
  indices <- which(col_order %in% colnames(yes_no_table))
  col_order <- col_order[indices]
  yes_no_table <- yes_no_table[,col_order]
  
  # combine two data frames but keep track of no. of columns before and after cbind
  initial_column_count <- ncol(sources_assessed)
  sources_assessed <- cbind(sources_assessed, yes_no_table)
  final_column_count <- ncol(sources_assessed)
  
  # create column index vectors to be used for formatStyle function
  columns_displayed <- 2:initial_column_count
  columns_hidden <- (initial_column_count):(final_column_count-1)
  columns_value <- (initial_column_count+2):(final_column_count)
  columns_centered <- 1:initial_column_count
  sources_assessed <- sources_assessed[order(sources_assessed$`data_year`, decreasing = TRUE), ]
  
  colnames(sources_assessed) [1] <- "Data Year"
  
  # produce HTML table widget using DT library
  datatable(sources_assessed, rownames = FALSE, options = list(dom = 't', lengthMenu = list(c(15, -1), c("15", "All")), columnDefs = list(list(targets=0, className = "dt-left"), list(targets=columns_centered, className = "dt-center"), list(targets=columns_hidden, visible=FALSE))), 
            caption = "Green indicates if report provided electricity or fuel use data; Red indicates if report did not provide electricity or fuel use data", escape = FALSE) %>%
    formatStyle(columns = columns_displayed, 
                valueColumns = columns_value, target = 'cell',
                backgroundColor = styleEqual(c("Yes", "No"), c("#90ee90", "#ff6c70")))
}