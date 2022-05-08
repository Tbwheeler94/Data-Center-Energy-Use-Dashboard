buildCompanyProfileMethodologyTable <- function(selected_company) {
  
  selected_company_methodology_full_table <- 
    data_sheet_energy_transformed  %>% 
    filter(company %in% selected_company) %>% 
    #select("company", "data_year", "notes", "notes_2", "notes_3") %>% 
    #filter(notes != "" | notes_2 != "" | notes_3 != "") %>% 
    mutate(energy_reporting_scope = case_when(
            energy_reporting_scope %in% c("Multiple Data Centers", "Single Data Center") ~ "Data center",
            energy_reporting_scope %in% "Total Operations"                               ~ "Total company"))
  
  selected_company_methodology_general <- 
    selected_company_methodology_full_table %>%
    filter(notes != "") %>% 
    select("data_year", "energy_reporting_scope", "notes") %>% 
    mutate(methodological_note_category = "General") %>% 
    distinct()
  
  selected_company_methodology_electricity <- 
    selected_company_methodology_full_table %>%
    filter(notes_2 != "") %>% 
    select("data_year", "energy_reporting_scope", "notes_2") %>% 
    mutate(methodological_note_category = "Electricity Use") %>% 
    distinct() %>% 
    rename(notes = "notes_2")
  
  selected_company_methodology_fuel <- 
    selected_company_methodology_full_table %>%
    filter(notes_3 != "", fuel_1_type != "Total Energy Use") %>% 
    select("data_year", "energy_reporting_scope", "notes_3") %>% 
    mutate(methodological_note_category = "Other Fuel Use") %>% 
    distinct() %>% 
    rename(notes = "notes_3")
  
  selected_company_methodology_ns_energy <- 
    selected_company_methodology_full_table %>%
    filter(notes_3 != "", fuel_1_type == "Total Energy Use") %>% 
    select("data_year", "energy_reporting_scope", "notes_3") %>% 
    mutate(methodological_note_category = "Non-specified Energy Use") %>% 
    distinct() %>% 
    rename(notes = "notes_3")
  
  selected_company_methodology_pue <- 
    data_sheet_pue_raw %>% 
    filter(company %in% selected_company, notes != "") %>% 
    mutate(facility_location = paste(facility_scope, geographical_scope, sep = " | ")) %>% 
    select("applicable_year", "facility_location", "notes") %>% 
    mutate(methodological_note_category = "PUE") %>% 
    rename(data_year = "applicable_year", energy_reporting_scope = "facility_location")
  
  methodology_notes <- rbind(selected_company_methodology_general, 
                             selected_company_methodology_electricity, 
                             selected_company_methodology_fuel, 
                             selected_company_methodology_ns_energy,
                             selected_company_methodology_pue) %>% 
                       rename(`Data Year` = "data_year", `Reporting Scope` = "energy_reporting_scope", `Methodological Note` = "notes",
                              `Data Category` = "methodological_note_category")
  
  methodology_notes
  
  }