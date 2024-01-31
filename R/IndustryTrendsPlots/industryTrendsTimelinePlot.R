buildIndustryTrendsTimelinePlot <- function(render_plot) {
  
  # status_levels <- c("Reported Data Center Electricity", "Reported Company Wide Electricity",
  #                    "Reported Company Wide Energy", "No Reporting of Publicly Available Data", 
  #                    "Company Does Not Exist Yet", "Pending Data Submission")
  # status_colors <- c("#3BCA6D", "#77945C", "#FF6865", "#ED2938", "#000000", "#999999")
  # industry_transparency$row_num <- seq.int(nrow(industry_transparency))
  # 
  p <- ggplot(industry_transparency, aes(x=data_year, y=company, data_id=row_num)) +
    geom_tile_interactive(aes(fill=energy_reporting_scope, tooltip=paste("Company: ", company, "\nData Year: ", data_year, "\nReporting Scope: ", energy_reporting_scope)), height=0.75) +
    labs(energy_reporting_scope="Reporting Scope") +
    scale_x_continuous(position = "top", breaks = pretty_breaks(n=length(unique_years)), expand = expansion(mult = c(0.01,0))) +
    theme(
      #legend.text=element_text(size=10),
      legend.position="none",
      axis.title.x=element_blank(),
      axis.text.x=element_text(size=10),
      axis.line.x=element_line(colour="black", size=1),
      axis.title.y=element_blank(),
      axis.text.y=element_text(size=12),
      axis.line.y=element_line(colour="black", size=1),
      panel.background=element_blank()
    ) +
    scale_fill_manual_interactive(
      name = label_interactive("Energy Reporting Scope", data_id="legend.title"),
      values = c(`Reported Data Center Electricity` = "#3BCA6D",
                 `Reported Company Wide Electricity` = "#77945C",
                 `Reported Company Wide Total Energy` = "#FF6865",
                 `No Reporting of Publicly Available Data` = "#ED2938",
                 `Pending Data Submission` = "#999999",
                 `Company Does Not Exist Yet` = "#000000"),
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
  
  if (render_plot == FALSE) {
    return(p)
  }
  
  
  
  # weather2 <- weather %>%
  #   filter(date <  mdy("03-31-2014"))%>% 
  #   mutate(low = datetime_to_timestamp(date), 
  #          high = datetime_to_timestamp(date+30), 
  #          id = row_number()) %>% 
  #   select(id, low, high)
  # 
  # 
  # hchart(weather2,
  #        type = "columnrange",
  #        hcaes(x = id, high=high, low=low)) %>% 
  #   hc_yAxis(type = "datetime")
  
  # hc %>%
  #   hc_chart(
  #     type = "columnrange",
  #     options3d = list(enabled = TRUE, beta = 15, alpha = 15)
  #   )
  # 
  # 
  # hc %>%
  #   hc_chart(
  #     borderColor = "#EBBA95",
  #     borderRadius = 10,
  #     borderWidth = 2,
  #     backgroundColor = list(
  #       linearGradient = c(0, 0, 500, 500),
  #       stops = list(
  #         list(0, "rgb(255, 255, 255)"),
  #         list(1, "rgb(200, 200, 255)")
  #       )
  #     )
  #   )


  x <- girafe(ggobj = p, width_svg = 13, height_svg = 7)
  x <- girafe_options(x,
                      opts_hover_inv(css = "opacity:0.6;"),
                      #opts_hover(css = "fill:black;stroke:black;r:5pt;"),
                      opts_sizing(rescale = FALSE),
                      opts_hover(css = "stroke-width:2; cursor: crosshair;"),
                      opts_hover_key(girafe_css("stroke:blue; cursor: help;", text="stroke:none;fill:red")))
  x
  
  # timeline_plot_height <- length(unique_companies) * 35
  # 
  # ggplotly(p, height=timeline_plot_height, tooltip = "text") %>% config(displayModeBar = T)  %>%
  #          # plotly::layout(legend = list(orientation = "h", x = 0, y = 1.15), xaxis = list(side ="top"))
  #            plotly::layout(xaxis = list(side ="top")) %>%
  #            hide_legend()
  
}