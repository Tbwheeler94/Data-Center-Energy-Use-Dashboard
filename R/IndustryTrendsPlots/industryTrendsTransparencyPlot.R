buildIndustryTrendsTransparencyPlot <- function(data_sheet_energy_raw) {
  
  for (year_count in 2007:as.integer(max(na.omit(data_sheet_energy_transformed$data_year)))) {
    # create a sub data frame that is filtered by data year and single data center scope
    data_of_transparency_SDC <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "Single Data Center") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_SDC <- distinct(data_of_transparency_SDC, .keep_all = TRUE)
    
    # create a sub data frame that is filtered by data year and multiple data centers scope
    data_of_transparency_MDC <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "Multiple Data Centers") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type)
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_MDC <- distinct(data_of_transparency_MDC, .keep_all = TRUE)
    
    # create a sub data frame that is filtered by data year and company wide electricity scope
    data_of_transparency_TO <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "Total Operations") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type) 
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_TO <- distinct(data_of_transparency_TO, .keep_all = TRUE)
    
    # create a sub data frame that is filtered by data year (***LOOK BACK AT THIS***)
    # IMPORTANT: leave energy reporting scope column blank for this transparency level!
    data_of_transparency_ND <- data_sheet_energy_transformed %>%
      filter(data_year == year_count, energy_reporting_scope == "") %>%
      select(company, data_year, energy_reporting_scope, fuel_1_type)
    
    # we only need 1 row of a company given a data year/reporting scope so erase repeating values
    data_of_transparency_ND <- distinct(data_of_transparency_ND, .keep_all = TRUE)
    
    # stack single data center/multiple data center data frames on top of each other
    data_of_transparency_L1 <- rbind(data_of_transparency_SDC, data_of_transparency_MDC)
    # since single/multiple data centers fall under same level, diminish to 1 row per company
    data_of_transparency_L1 <- distinct(data_of_transparency_L1, company, .keep_all = TRUE)
    
    # stack rest of the data frames together
    data_of_transparency_L1 <- rbind(data_of_transparency_L1, data_of_transparency_TO,
                                     data_of_transparency_ND)
    data_of_transparency_L1 <- distinct(data_of_transparency_L1, company, .keep_all = TRUE)
    data_of_transparency_L1$value <- nrow(data_of_transparency_L1)
    
    # create the master data frame from the first index of the for loop
    # else: add the existing master data frame to the recently computed data frames
    if (year_count == 2007) {
      data_of_transparency <- rbind(data_of_transparency_L1)
    } else {
      data_of_transparency <- data_of_transparency %>% rbind(data_of_transparency_L1)
    }
    # sort graph in alphabetical order of company in order to make summation loop easier
    data_of_transparency <- data_of_transparency[order(data_of_transparency[,"data_year"]), ]
    #data_of_transparency <- distinct(data_of_transparency, company, .keep_all = TRUE)
  }
  
  # create another data frame that is filtered by company founding year
  data_founding_year <- data_sheet_company_raw %>% select(company_name, 
                                                      checked_back_to_founding_year_or_2007,
                                                      company_founding_year)
  # drop all the excess rows that have NA data
  data_founding_year <- na.omit(data_founding_year)
  data_founding_year <- data_founding_year[data_founding_year$checked_back_to_founding_year_or_2007=="Yes", ]
  data_of_transparency <- subset(data_of_transparency, company %in% data_founding_year$company_name)
  
  # assessing if there are companies in data_founding_year that ARE NOT in data_of_transparency
  for (i in 1:nrow(data_founding_year)) {
    if (!any(data_of_transparency == data_founding_year[i,1])) {
      data_of_transparency[nrow(data_of_transparency)+1, ] <- 
        c(data_founding_year[i,1], 2007, 0, 0, 0)
    }
  }
  
  # create new rows for companies that do not have L1/L2/L3 transparency in that given year
  # in other words, create rows for level 4 companies (no report found)
  data_of_transparency <- dummy_rows(data_of_transparency, 
                                     select_columns = c("company", "data_year"),
                                     dummy_value = 0)
  data_of_transparency <- data_of_transparency[order(data_of_transparency[,"data_year"]), ]
  
  # NOW we have data for some companies that didn't even exist yet!
  # take each company in data_founding year, compare with ALL rows in data_of_transparency,
  # and if the founding year of the company is greater than the data year of the company,
  # then keep track of that index in deleted_rows vector 
  deleted_rows <- vector()
  z = 1
  for (i in 1:nrow(data_founding_year)) {
    for (j in 1:nrow(data_of_transparency)) {
      if (data_founding_year[i,1] == data_of_transparency[j,1]) {
        if (data_founding_year[i,3] > data_of_transparency[j,2]) {
          deleted_rows[z] <- j
          z <- z + 1
        }
      }
    }
  }
  
  # drop all rows that have company data prior to their founding year
  if (length(deleted_rows) != 0) {
    data_of_transparency <- data_of_transparency[-c(deleted_rows), ]
  }
  
  data_of_transparency$value <- 1
  data_of_transparency <- data_of_transparency %>% select(data_year, energy_reporting_scope, 
                                                          fuel_1_type, value)
  
  # loop through all rows and rename energy reporting scopes
  data_of_transparency$energy_reporting_scope[data_of_transparency$energy_reporting_scope == "Single Data Center" ] <- "Reported Data Center Electricity"
  data_of_transparency$energy_reporting_scope[data_of_transparency$energy_reporting_scope == "Multiple Data Centers" ] <- "Reported Data Center Electricity"
  data_of_transparency$energy_reporting_scope[data_of_transparency$energy_reporting_scope == "Total Operations"] <- "Reported Company Wide Electricity"
  data_of_transparency$energy_reporting_scope[data_of_transparency$fuel_1_type == "Total Energy Use"] <- "Reported Company Wide Total Energy"
  data_of_transparency$energy_reporting_scope[data_of_transparency$energy_reporting_scope == "0"] <- "No Reporting of Publicly Available Data"
  data_of_transparency$energy_reporting_scope[data_of_transparency$energy_reporting_scope == "No Reporting of Publicly Available Data" & data_of_transparency$data_year == max(na.omit(data_sheet_energy_transformed$data_year))] <- "Pending Data Submission"
  
  # stack single data center/multiple data center data frames on top of each other
  data_of_transparency_final <- data_of_transparency %>%
    group_by(data_year,energy_reporting_scope) %>%
    dplyr::summarise(value = sum(value), .groups = "rowwise") %>%
    as.data.frame()
  data_of_transparency_final <- data_of_transparency_final %>%
    group_by(data_year,energy_reporting_scope)
  
  data_of_transparency <- data_of_transparency_final
  
  data_of_transparency$energy_reporting_scope <- factor(data_of_transparency$energy_reporting_scope, 
                                                        levels=c("Reported Data Center Electricity",
                                                                 "Reported Company Wide Electricity",
                                                                 "Reported Company Wide Total Energy",
                                                                 "No Reporting of Publicly Available Data",
                                                                 "Pending Data Submission"))
  data_of_transparency$row_num <- seq.int(nrow(data_of_transparency))
  
  # status_levels <- c("Reported Data Center Electricity", "Reported Company Wide Electricity",
  #                    "Reported Company Wide Total Energy", "No Reporting of Publicly Available Data", "Pending Data Submission")
  # status_colors <- c("#3BCA6D", "#77945C", "#FF6865", "#ED2938", "#999999")
  
  # p <- ggplot(data_of_transparency, aes(x=data_year, 
  #       text=paste("Data Year: ", data_year, "\nNumber of Companies: ", value))) + 
  #   geom_bar(aes(y=value, fill=energy_reporting_scope),
  #            position="stack", stat="identity") +
  #   scale_fill_manual(values=status_colors, labels=status_levels, drop=FALSE) +
  #   theme_classic() +
  #   theme(
  #     legend.title = element_text(size=14),
  #     legend.text = element_text(size=12)
  #   ) +
  #   scale_y_continuous(expand = expansion(mult = c(0.01,0))) +
  #   xlab("Year") +
  #   ylab("Number of Companies") +
  #   labs(fill = "Energy Reporting Scope")
  
  list_of_descriptions <- c("companies report electricity\nfor single or multiple data centers", 
                            "companies report electricity\nfor the entire company", 
                            "companies report an accumulation\nof energy (electricity, gas, etc) for the entire company", 
                            "companies do not have a report\nconsisting of quantitative energy data", 
                            "companies may not have released\na report for the previous year yet")
  
  p <- ggplot(data_of_transparency, aes(x=data_year, y=value, data_id=row_num)) +
    geom_bar_interactive(aes(fill=energy_reporting_scope, tooltip=paste0("Data Year: ", data_year, "\nNumber of Companies: ", value)), 
                         position="stack", stat="identity") +
    theme_classic() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=14),
      axis.text = element_text(size=11),
      legend.title = element_text(size=14),
      legend.text = element_text(size=12),
      legend.text.align = 0
    ) +
    scale_x_continuous(breaks = 2007:2021, expand = c(0.01,0)) +
    scale_y_continuous(expand = expansion(mult = c(0.01,0))) +
    ylab("Number of Companies") +
    scale_fill_manual_interactive(
      name = label_interactive("Energy Reporting Scope", data_id="legend.title"),
      values = c(`Reported Data Center Electricity` = "#3BCA6D", 
                 `Reported Company Wide Electricity` = "#77945C",
                 `Reported Company Wide Total Energy` = "#FF6865",
                 `No Reporting of Publicly Available Data` = "#ED2938",
                 `Pending Data Submission` = "#999999"),
      data_id = function(breaks) { as.character(breaks)},
      tooltip = function(breaks) { as.character(breaks)},
      drop = FALSE,
      onclick = function(breaks) { paste0("alert(\"", as.character(breaks), "\")") },
      labels = function(breaks) {
        lapply(breaks, function(br) {
          printed_message <- ""
          if (br == "Reported Data Center Electricity") {
            printed_message <- list_of_descriptions[1]
          } else if (br == "Reported Company Wide Electricity") {
            printed_message <- list_of_descriptions[2]
          } else if (br == "Reported Company Wide Total Energy") {
            printed_message <- list_of_descriptions[3]
          } else if (br == "No Reporting of Publicly Available Data") {
            printed_message <- list_of_descriptions[4]
          } else {
            printed_message <- list_of_descriptions[5]
          }
          label_interactive(
            as.character(br),
            data_id = as.character(br),
            onclick = paste0("alert(\"", as.character(br), "\")"),
            tooltip = paste0(as.character(br), " means ", printed_message)
          )
        })
      }
    )
  
  x <- girafe(ggobj = p, width_svg = 13, height_svg = 7)
  x <- girafe_options(x,
                      opts_hover_inv(css = "opacity:0.6;"),
                      #opts_hover(css = "fill:black;stroke:black;r:5pt;"),
                      opts_hover(css = "stroke-width:2; cursor: crosshair;"),
                      opts_hover_key(girafe_css("stroke:blue; cursor: help;", text="stroke:none;fill:red")))
  x
  
  #ggplotly(p, tooltip = "text")
  
}