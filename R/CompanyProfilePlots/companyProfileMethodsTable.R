buildCompanyProfileMethodsTable <- function(data_sheet_energy_transformed) {
  
  #colnames(data_sheet_energy_transformed) [3] <- 'period_covered_start_date'

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
    #because there are blank rows in report_type column, R does not know what to name this column after pivoting and throws the error (Error: Column 3 must be named. Use .name_repair to specify repair.)
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
  
  datatable(sources_assessed, rownames = FALSE, options = list(dom = 't', initComplete = JS(jscode), columnDefs = list(list(visible=FALSE, targets=1))))
  
}