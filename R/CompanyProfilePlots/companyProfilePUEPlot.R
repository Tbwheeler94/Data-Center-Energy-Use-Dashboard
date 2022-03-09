buildCompanyProfilePUEPlot <- function(selected_company) {
  
  selected_company_pue_filter <-
    data_sheet_pue_raw %>% 
    filter(company == "Oracle") %>% 
    select("applicable_year", "facility_scope", "geographical_scope", "pue_value") %>% 
    unite(pue_facility_geographic, facility_scope:geographical_scope, sep = " | ") %>% 
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
      mutate_if(is.numeric, ~ifelse(. == 0, "", .))
  }
  
  if(nrow(selected_company_pue_filter) != 0) {
    
    datatable(selected_company_pue_filter, rownames = FALSE, options = list(pageLength = 5, autoWidth = TRUE, columnDefs = list(list(width = '300px',
                                                                                                                                targets = c(0)))))
  } else {
    datatable(no_data, options = list(dom = 't', headerCallback = JS("function(thead, data, start, end, display){",
                                                                     "  $(thead).remove();",
                                                                     "}")), rownames = FALSE)
  }

}