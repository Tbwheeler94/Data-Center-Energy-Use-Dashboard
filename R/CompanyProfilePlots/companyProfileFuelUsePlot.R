buildCompanyProfileFuelUsePlot <- function(selected_company, methodology_table_lookup) {
  
  #filter methodology table for only values with a note associated with them in the "notes_2" column, reserved for methodological notes about an electricity value.
  #convert values in the reporting scope column to match data_centers or Total_company
  methodology_table_lookup_filtered <- methodology_table_lookup %>% 
    filter(`Methodological Notes Category` == "Other Fuel Use") %>% 
    mutate(`Reporting Scope` = case_when(
      `Reporting Scope` %in% "Data center" ~ "data_centers",
      `Reporting Scope` %in% "Total company" ~ "Total_company")) %>% 
    rename("data_year" = `Data Year`, "category" = `Reporting Scope`)
  
  selected_company_fuel_use_filter <- 
    data_sheet_energy_transformed %>% 
    filter(company %in% selected_company) %>% 
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
    #add asterisk to fuel data that has a note associated with it
    mutate(value = ifelse(data_year %in% methodology_table_lookup_filtered$data_year & category %in% methodology_table_lookup_filtered$category, 
                          paste(value,"*", sep = ""), value)) %>% 
    pivot_wider(names_from = data_year, values_from = value) %>% 
    mutate(category = case_when(
      category %in% "Self_managed" ~ "Self-managed",
      category %in% "Leased" ~ "Leased",
      category %in% "data_centers" ~ "Data centers")) %>% 
    mutate_if(is.numeric, ~round(., 3))
  
  #If the number of rows in the dataframe is not equal to 0
  if (nrow(selected_company_fuel_use_filter) != 0) {
    
    selected_company_fuel_use_filter <- 
      selected_company_fuel_use_filter %>% 
      mutate(category = factor(category, levels = c("Data centers", "Self-managed", "Leased"))) %>% 
      arrange(category) %>% 
      add_column(format = c(1,0,0), .before = 'category')
    
    possible_years_fuel <- c(2007:as.integer(tail(colnames(selected_company_fuel_use_filter), n=1)))
    extra_years_fuel <- list()
    
    j <- 1
    for (i in 1:length(possible_years_fuel)) {
      if (possible_years_fuel[i] %not_in% names(selected_company_fuel_use_filter)) {
        extra_years_fuel[j] <- possible_years_fuel[i]
        j <- j+1
      }
    }
    
    selected_company_fuel_use_filter <- 
    selected_company_fuel_use_filter %>% 
      add_column(!!!set_names(as.list(rep(0, length(extra_years_fuel))),nm=extra_years_fuel)) %>% 
      select(sort(tidyselect::peek_vars())) %>% 
      relocate(c(format, category), .before = '2007') %>% 
      mutate_if(is.numeric, ~ifelse(. == 0 | . == "0*", "", .)) %>% 
      mutate(format = c(1,0,0))
  }
  selected_company_fuel_use_filter
}