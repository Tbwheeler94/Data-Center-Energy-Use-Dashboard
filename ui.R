home_page <- htmlTemplate(filename = "www/index.html",
                          company_graph = plotOutput("aggregatePlot"),
                          company_selection = selectInput(
                            inputId = "company_selection",
                            label = h4("Choose Company:", style = "color:black" , align = "center"),
                            choices = unique(by_fuel_type_data$company),
                            selected = "Spark"),
                          fuel_output = plotOutput("companyfuelPlot"),
                          companies_reporting = textOutput("companies_reporting"),
                          years_reported = textOutput("years_reported"),
                          energy_reported = textOutput("energy_reported"))

company_page <- htmlTemplate(filename = "www/company_page.html",
                              fuel_output = plotOutput("companyfuelPlot"),
                              companies_reporting = textOutput("companies_reporting"),
                              years_reported = textOutput("years_reported"),
                              energy_reported = textOutput("energy_reported"))

router <- make_router(
  route("/", home_page),
  route("company_page", company_page)
)

ui <- fluidPage(
  theme = "main.css",
  tags$ul(
    tags$li(a(href = route_link("/"), "Home")),
    tags$li(a(href = route_link("company_page"), "Company Page"))
  ),
  router$ui
)