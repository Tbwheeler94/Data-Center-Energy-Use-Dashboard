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
  
  data_sheet_pue_all$row_num <- 1
  data_sheet_pue_filtered$row_num <- 2
  
  p <- ggplot(data=data_sheet_pue_filtered, aes(x = applicable_year, y = pue_value, data_id = row_num)) +
    geom_point_interactive(data = data_sheet_pue_all, aes(tooltip = paste("Company: ", company, "\nGeographical Scope: ", geographical_scope,
                                                                      "\nPUE Value: ", pue_value, "\n", pue_measurement_level)), colour = "black", size = 4) +
    geom_point_interactive(data = data_sheet_pue_filtered, aes(data_id = company, color = company, tooltip = paste("Company: ", company, "\nGeographical Scope: ", geographical_scope,
                                                                              "\nPUE Value: ", pue_value, "\n", pue_measurement_level)), size = 8) +
    scale_x_continuous(breaks = pretty_breaks(n = length(unique_years))) +
    scale_y_continuous(breaks = c(0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0)) +
    theme_light() +
    theme(
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(size=10),
      axis.text.y = element_text(size=12),
      axis.line.x = element_line(colour = "black", size=1),
      axis.line.y = element_line(colour = "black", size=1),
      axis.title.x = element_blank()
    ) +
    ylab("PUE Value") +
    labs(colour = "Companies") +
    scale_fill_manual_interactive(
      data_id = function(breaks) { as.character(breaks)},
      tooltip = function(breaks) { as.character(breaks)},
      drop = FALSE,
      onclick = function(breaks) { paste0("alert(\"", as.character(breaks), "\")") },
      labels = function(breaks) {
        lapply(breaks, function(br) {
          label_interactive(
            as.character(br),
            data_id = as.character(br),
            onclick = paste0("alert(\"", as.character(br), "\")"),
            tooltip = paste0(as.character(br), " means ", printed_message)
          )
        })
      }
    )
  
  #n <- ggplot() + theme_void() + annotate("text", x = 4, y=13000, label = "No Data Found. Change Company or Scope Level.")
  
  if (nrow(data_sheet_pue_filtered) != 0) {
    x <- girafe(ggobj = p, width_svg = 10, height_svg = 8)
    x <- girafe_options(x,
                        opts_hover_inv(css = "opacity:0.2;"),
                        #opts_hover(css = "fill:black;stroke:black;r:5pt;"),
                        opts_hover(css = "stroke-width:2; cursor: crosshair;"),
                        opts_hover_key(girafe_css("stroke:blue; cursor: help;", text="stroke:none;fill:red")))
    x
  } else {
    girafe(ggobj = ggplot() + theme_void() + annotate("text", x = 4, y=13000, label = "No Data Found. Change Company or Scope Level."))
  }
}