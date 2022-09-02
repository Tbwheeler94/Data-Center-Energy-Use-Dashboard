buildIndustryTrendsTransparencyPlot <- function(data_sheet_energy_raw) {
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