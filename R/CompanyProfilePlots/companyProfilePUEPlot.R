buildCompanyProfilePUEPlot <- function(selected_company, methodology_table_lookup) {
  
  #filter methodology table for only values with a note associated with them in the "notes_2" column, reserved for methodological notes about an electricity value.
  #convert values in the reporting scope column to match data_centers or Total_company
  methodology_table_lookup_filtered <- methodology_table_lookup %>% 
    filter(`Methodological Notes Category` == "PUE") %>% 
    rename("data_year" = `Data Year`, "category" = `Reporting Scope`)
  
  selected_company_pue_filter <-
    data_sheet_pue_raw %>% 
    filter(company %in% selected_company) %>% 
    select("applicable_year", "facility_scope", "geographical_scope", "pue_value") %>% 
    unite(pue_facility_geographic, facility_scope:geographical_scope, sep = " | ") %>% 
    mutate(pue_value = ifelse(applicable_year %in% methodology_table_lookup_filtered$data_year & pue_facility_geographic %in% methodology_table_lookup_filtered$category, 
                          paste(pue_value,"*", sep = ""), pue_value)) %>% 
    pivot_wider(names_from = applicable_year, values_from = pue_value, names_sort = TRUE) %>%
    mutate_all(~replace(., is.nan(.), NA)) %>% 
    mutate_if(is.numeric, as.character) %>% 
    replace(is.na(.), "") %>% 
    rename(c("Facility Scope and Location" = pue_facility_geographic))
  
  if (nrow(selected_company_pue_filter) != 0) {
    
    possible_years_pue <- c(2007:as.integer(tail(colnames(selected_company_pue_filter), n=1)))
    extra_years_pue <- list()
    
    j <- 1
    for (i in 1:length(possible_years_pue)) {
      if (!(possible_years_pue[i] %in% names(selected_company_pue_filter))) {
        extra_years_pue[j] <- possible_years_pue[i]
        j <- j + 1
      }
    }
    
    selected_company_pue_filter <-
    selected_company_pue_filter %>% 
      add_column(!!!set_names(as.list(rep(0, length(extra_years_pue))),nm=extra_years_pue)) %>% 
      select(sort(tidyselect::peek_vars())) %>% 
      relocate(c("Facility Scope and Location"), .before = '2007') %>% 
      mutate_if(is.numeric, ~ifelse(. == 0 | . == "0*", "", .))
  }
  
  selected_company_pue_filter

}