ui <- htmlTemplate(filename = "www/index.html",
                   company_graph = plotOutput("aggregatePlot"),
                   select_company = selectInput(
                     inputId = "company_selection",
                     label = h4("Choose Company:", style = "color:black" , align = "center"),
                     choices = unique(by_fuel_type_data$company),
                     selected = "Spark"),
                   fuel_output = plotOutput("companyfuelPlot"))