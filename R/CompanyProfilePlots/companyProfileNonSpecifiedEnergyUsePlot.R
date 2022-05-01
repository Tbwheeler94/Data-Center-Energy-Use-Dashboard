buildCompanyProfileNonSpecifiedEnergyUsePlot <- function(selected_company, methodology_table_lookup) {
  
  #filter methodology table for only values with a note associated with them in the "notes_2" column, reserved for methodological notes about an electricity value.
  #convert values in the reporting scope column to match data_centers or Total_company
  methodology_table_lookup_filtered <- methodology_table_lookup %>% 
    filter(`Methodological Notes Category` == "Non-specified Energy Use") %>% 
    mutate(`Reporting Scope` = case_when(
      `Reporting Scope` %in% "Data center" ~ "data_centers",
      `Reporting Scope` %in% "Total company" ~ "Total_company")) %>% 
    rename("data_year" = `Data Year`, "category" = `Reporting Scope`)
  
  selected_company_ns_energy_use_filter <-
    data_sheet_energy_transformed %>%
    filter(company %in% selected_company, fuel_1_type == "Total Energy Use") %>% 
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
    pivot_wider(names_from = energy_reporting_scope, values_from = value)
  
  #If the number of rows in the dataframe is not equal to 0
  if (nrow(selected_company_ns_energy_use_filter) != 0) {
    selected_company_ns_energy_use_filter <- selected_company_ns_energy_use_filter %>%
      replace(is.na(.), 0) %>% 
      mutate(Leased = ifelse("Leased" %in% names(.), Leased, 0),
             Self_managed = ifelse("Self_managed" %in% names(.), Self_managed, 0),
             Total_company = ifelse("Total_company" %in% names(.), Total_company, 0)) %>% 
      mutate(data_centers = Self_managed + Leased) %>% 
      mutate(data_center_percentage = percent(data_centers/Total_company)) %>%
      mutate_if(is.numeric, ~round(., 3)) %>%
      mutate(Total_company = as.character(Total_company), Self_managed = as.character(Self_managed), 
             Leased = as.character(Leased), data_centers = as.character(data_centers)) %>%
      pivot_longer(!data_year, names_to = "category", values_to = "value") %>% 
      #add asterisk to ns energy data that has a note associated with it
      mutate(value = ifelse(data_year %in% methodology_table_lookup_filtered$data_year & category %in% methodology_table_lookup_filtered$category, 
                            paste(value,"*", sep = ""), value)) %>% 
      pivot_wider(names_from = data_year, values_from = value) %>% 
      mutate(category = case_when(
        category %in% "Total_company" ~ "Total company",
        category %in% "Self_managed" ~ "Self-managed",
        category %in% "Leased" ~ "Leased",
        category %in% "data_centers" ~ "Data centers",
        category %in% "data_center_percentage" ~ "Data center % of total electricity"))
    
    selected_company_ns_energy_use_filter <- 
      selected_company_ns_energy_use_filter %>% 
      mutate(category = factor(category, levels = c("Data centers", "Self-managed", "Leased", "Total company", "Data center % of total electricity"))) %>% 
      arrange(category) %>% 
      add_column(format = c(1,0,0,1,1), .before = 'category')
    
    possible_years_ns_energy <- c(2007:as.integer(tail(colnames(selected_company_ns_energy_use_filter), n=1)))
    extra_years_ns_energy <- list()
    
    j <- 1
    for (i in 1:length(possible_years_ns_energy)) {
      if (possible_years_ns_energy[i] %not_in% names(selected_company_ns_energy_use_filter)) {
        extra_years_ns_energy[j] <- possible_years_ns_energy[i]
        j <- j + 1
      }
    }
    
    selected_company_ns_energy_use_filter <- selected_company_ns_energy_use_filter %>% 
      add_column(!!!set_names(as.list(rep(0, length(extra_years_ns_energy))),nm=extra_years_ns_energy)) %>% 
      select(sort(tidyselect::peek_vars())) %>% 
      relocate(c(format, category), .before = '2007') %>% 
      mutate_if(is.numeric, ~ifelse(. == 0, "", .)) %>% 
      mutate_if(is.character, ~ifelse(. == "0" | . == "0*", "", .)) %>% 
      mutate_if(is.character, ~ifelse(. == "0%" | . == "Inf", "NA", .)) %>% 
      mutate(format = c(1,0,0,1,1))
  }
  
  selected_company_ns_energy_use_filter
}  