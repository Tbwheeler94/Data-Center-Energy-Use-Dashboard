buildCompanyProfileElectricityUsePlot <- function(selected_company) {

  selected_company_electricity_use_filter <- 
    data_sheet_energy_transformed %>% 
    filter(company == selected_company) %>% #filter by selected company
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
      energy_reporting_scope %in% "Company-wide electricity use | Leased" ~ "Total_company",
      energy_reporting_scope %in% "Company-wide electricity use | Self-managed" ~ "Total_company",
      energy_reporting_scope %in% "Company-wide electricity use | " ~ "Total_company")) %>% #If no level of ownership is given, assume self managed
    pivot_wider(names_from = energy_reporting_scope, values_from = value)
  
  #If the dataframe contains at least 1 row, perform the following function
  if (nrow(selected_company_electricity_use_filter) != 0) { 
    selected_company_electricity_use_filter <- selected_company_electricity_use_filter %>%
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
      # mutate(value = ifelse(category == 'data_center_percentage', percent(value), value))
      pivot_wider(names_from = data_year, values_from = value) %>% 
      mutate(category = case_when(
        category %in% "Total_company" ~ "Total company",
        category %in% "Self_managed" ~ "Self-managed",
        category %in% "Leased" ~ "Leased",
        category %in% "data_centers" ~ "Data centers",
        category %in% "data_center_percentage" ~ "Data center % of total electricity"))
    
    selected_company_electricity_use_filter <- 
      selected_company_electricity_use_filter %>% 
      mutate(category = factor(category, levels = c("Data centers", "Self-managed", "Leased", "Total company", "Data center % of total electricity"))) %>%
      arrange(category) %>% 
      add_column(format = c(1,0,0,1,1), .before = 'category')
    
    possible_years_electricity <- c(2007:as.integer(tail(colnames(selected_company_electricity_use_filter), n=1)))
    extra_years_electricity <- list()
    
    j <- 1
    for (i in 1:length(possible_years_electricity)) {
      if (!(possible_years_electricity[i] %in% names(selected_company_electricity_use_filter))) {
        extra_years_electricity[j] <- possible_years_electricity[i]
        j <- j + 1
      }
    }
    
    selected_company_electricity_use_filter <- 
      selected_company_electricity_use_filter %>% 
      add_column(!!!set_names(as.list(rep(0, length(extra_years_electricity))),nm=extra_years_electricity)) %>% 
      select(sort(tidyselect::peek_vars())) %>% 
      relocate(c(format, category), .before = '2007') %>% 
      mutate_if(is.numeric, ~ifelse(. == 0, "", .)) %>% 
      mutate_if(is.character, ~ifelse(. == "0", "", .)) %>% 
      mutate_if(is.character, ~ifelse(. == "0%" | . == "Inf", "NA", .)) %>% 
      mutate(format = c(1,0,0,1,1))
  }
  selected_company_electricity_use_filter
}