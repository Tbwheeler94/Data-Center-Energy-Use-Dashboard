buildIndustryTrendsPUETrends <- function(data_sheet_pue_filtered, selected_company, selected_scope) {
  data_sheet_pue_all <- data_sheet_pue_raw %>%
    select(c("company", "applicable_year", "facility_scope", "geographical_scope", "pue_value", "pue_measurement_level")) %>%
    mutate(facility_scope_clean = case_when(
      facility_scope %in% c("Fleet-wide PUE") ~ "Fleet Wide",
      facility_scope %in% c("Single location PUE") ~ "Individual Locations"
    )) %>%
    filter(!(company %in% selected_company) | !(facility_scope_clean %in% selected_scope)) %>%
    na.omit()
  
  data_sheet_pue_filtered <- data_sheet_pue_filtered %>%
    select(c("company", "applicable_year", "facility_scope", "geographical_scope", "pue_value", "pue_measurement_level")) %>%
    mutate(facility_scope_clean = case_when(
      facility_scope %in% c("Fleet-wide PUE") ~ "Fleet Wide",
      facility_scope %in% c("Single location PUE") ~ "Individual Locations"
    )) %>%
    filter(company %in% selected_company & facility_scope_clean %in% selected_scope) %>%
    na.omit()
  
  
  p <- ggplot(data=data_sheet_pue_all, aes(x=applicable_year, y=pue_value,
                                  text=paste("Company: ", company, "\nGeographical Scope: ", geographical_scope,
                                             "\nPUE Value: ", pue_value, "\n", pue_measurement_level))) +
    geom_point(data=data_sheet_pue_all, colour="black", size=1) +
    geom_point(data=data_sheet_pue_filtered, aes(color=company), size=3) +
    theme_classic() +
    xlab("Year") +
    ylab("PUE Value") +
    labs(color = "Companies")
  
  n <- ggplot() + theme_void() + annotate("text", x = 4, y=13000, label = "No Data Found. Change Company or Scope Level.")
  
  if (nrow(data_sheet_pue_filtered) != 0) {
    ggplotly(p, tooltip = "text")
  } else {
    ggplotly(n)
  }
}