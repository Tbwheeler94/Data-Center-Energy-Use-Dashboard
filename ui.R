
################
#####Cards######
################

MainCard <- function(..., title = NULL) {
  
  Stack(
    class = "ms-depth-8",
    tokens = list(padding = 20, childrenGap = 5),
    if (!is.null(title)) Text(title, variant = "xxLarge"),
    ...  
  )
}

HighlightsCard <- function(...) {
  
  Stack(
    class = "ms-depth-8",
    tokens = list(padding = 20, childrenGap = 5),
    style = "text-align: center",
    ...  
  )
}

CompanyCard <- function(..., title = NULL) {
  
  Stack(
    class = "ms-depth-8",
    tokens = list(padding = 20, childrenGap = 5),
    ...  
  )
}

################################
##########Grid Layout###########
################################

Grid <- function (...) {
  div(
    class = "ms-Grid", dir = "ltr",
    style = "padding: 0px",
    ...
  )
}

GridItem <- function (..., class = "ms-sm12") {
  div(
    class = paste("ms-Grid-col", class),
    style = "padding: 10px;",
    ...
  )
}

makePage <- function (contents) {
  tagList(
  contents)
}

#######################################
############## HOME PAGE ##############
#######################################

home_page <- makePage(
  div(
    Grid(
      GridItem(class = "ms-sm12 ms-xl12",
        MainCard(title = "About the Data",
           Text("We are a team of University of California, Santa Barbara based researchers aiming to increase transparency and understanding of trends in global data center energy use. This website is a dashboard for modelers, policy-makers, and the general public to gain insight into data currently being reported by many of the world largest technology companies. Our visualization uses aggregated energy data primarily collected from publicly disclosed corporate sustainability reports.", variant = "large"))
        ),
        GridItem(class = "ms-sm12 ms-xl4",
          HighlightsCard(
            FontIcon(iconName = "Calendar", style = list(fontSize = 60)),
            Text(uiOutput("years_reported"), variant = "mega"),
            Text("Years Reporting", variant = "xxLarge")
          )
        ),
        GridItem(class = "ms-sm12 ms-xl4",
          HighlightsCard(
            FontIcon(iconName = "ClipboardList", style = list(fontSize = 60)),
            Text(uiOutput("companies_reporting"), variant = "mega"),
            Text("Companies Reporting", variant = "xxLarge")
          )
        ),
        GridItem(class = "ms-sm12 ms-xl4",
          HighlightsCard(
            FontIcon(iconName = "TableComputed", style = list(fontSize = 60)),
            Text(uiOutput("energy_reported"), variant = "mega"),
            Text("Energy Reported", variant = "xxLarge")
          )
        )
    )
  )
)

###########################################
######### DC ENERGY 101 PAGE ##############
###########################################

dc_energy_101_page <- makePage(
  div(
    Grid(
      GridItem(class = "ms-sm12 ms-xl12", style = "text-align: center",
               MainCard(title = "Page Under Construction",
                        FontIcon(iconName = "ConstructionCone", style = list(fontSize = 80)
                        )
               )
                        
      )
    )
  )
)

#############################################
######### INDUSTRY TRENDS PAGE ##############
#############################################

industry_trends_page <- makePage(
  div(
    Grid(
      GridItem(class = "ms-sm12 ms-xl12", style = "text-align: center",
               MainCard(title = "Page Under Construction",
                        FontIcon(iconName = "ConstructionCone", style = list(fontSize = 80)
                        )
               )
               
      )
    )
  )
)

##############################################
######### COMPANY ANALYSIS PAGE ##############
##############################################

company_analysis_page <- makePage(
  div(
    Grid(
      GridItem(class = "ms-sm12 ms-xl3",
               Stack(
                 horizontal = TRUE,
                 class = "ms-depth-8",
                 tokens = list(padding = 20, childrenGap = 20),
                 Text("Select company", variant = "large"),
                 ComboBox.shinyInput("selected_company", 
                                     options = unique_companies,
                                     value = "Google",
                                     placeHolder = "Google",
                                     dropdownWidth = 150)  
               ),
               Stack(
                 class = "ms-depth-8",
                 dataTableOutput("selected_company_stats")
               )
      ),
      GridItem(class = "ms-sm12 ms-xl5",
               CompanyCard(style = "height: 250px",
                           Text("Company Data Center Overview", variant = "xLarge"),
                           ScrollablePane(style = "height: 180px; position: relative;", Text(uiOutput("company_data_center_overview"), variant = "mediumPlus")))
              ),
      GridItem(class = "ms-sm12 ms-xl4",
               CompanyCard(style = "height: 250px",
                           Text("Energy Report Assessment", variant = "xLarge"), 
                           ScrollablePane(style = "height: 180px; position: relative;", Text(uiOutput("energy_reporting_assessment"), variant = "mediumPlus")))
              )
    ),
    Grid(
      Stack(style = "text-align: center;", Text("Current Energy Reporting Snapshot", variant = "xLarge", style = "color: red;")),
      GridItem(class = "ms-sm12 ms-xl4",                                               
               CompanyCard(
                 Text("Reported energy use levels", variant = "large", style = "text-align: center;"),
                 dataTableOutput("reported_energy_levels")
               )
      ),
      GridItem(class = "ms-sm12 ms-xl4",                                               
               CompanyCard(
                 Text("Data standards", variant = "large", style = "text-align: center;"),
                 dataTableOutput("data_standards")
               )
      ),
      GridItem(class = "ms-sm12 ms-xl4",
               CompanyCard(
                 Text("Other metrics reported", variant = "large", style = "text-align: center;"),
                 dataTableOutput("other_metrics")
               )
      )
    ),
    Grid(
      Stack(style = "text-align: center;", Text("Energy Use Trend & Data", variant = "xLarge", style = "color: red;")),
      GridItem(class = "ms-sm12 ms-xl12",
               CompanyCard(
                 plotlyOutput("companyfuelPlot")
               )
        
      )
    ),
    Grid(
      GridItem(class = "ms-sm12 ms-xl6",
               CompanyCard(
                 Text("Electricity Use (TWh/yr)", variant = "large", style = "text-align: center;"),
                 dataTableOutput("electricity_use_table")
               )
               
      ),
      GridItem(class = "ms-sm12 ms-xl6",
               CompanyCard(
                 Text("Other fuel use (TWh/yr)", variant = "large", style = "text-align: center;")
               )
               
      )
    ),
    Grid(
      GridItem(class = "ms-sm12 ms-xl6",
               CompanyCard(
                 Text("Non-specified energy use (TWh/yr)", variant = "large", style = "text-align: center;")
               )
               
      ),
      GridItem(class = "ms-sm12 ms-xl6",
               CompanyCard(
                 Text("PUE", variant = "large", style = "text-align: center;")
               )
               
      )
    ),
    Grid(
      Stack(style = "text-align: center;", Text("Methodology", variant = "xLarge", style = "color: red;"))
    )
    )
)
#list(unique(data_sheet_company$company_name))

###########################################
######### METHODS PAGE ###################
###########################################

methods_page <- makePage(
  div(
    Grid(
      GridItem(class = "ms-sm12 ms-xl12", style = "text-align: center",
               MainCard(title = "Page Under Construction",
               FontIcon(iconName = "ConstructionCone", style = list(fontSize = 80))
               )
      )
    )
  )
)


##########################################
############## DASHBOARD UI ##############
##########################################

header <- "header"
navigation <- "navigation"
footer <- "footer"

layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}

header <- tagList(
  img(src = "ucsb.png", class = "logo"),
  div(Text(variant = "xLarge", "Energy Use Dashboard"), class = "title"),
  CommandBar(
    items = list(
      CommandBarItem("New", "Add", subitems = list(
        CommandBarItem("Email message", "Mail", key = "emailMessage", href = "mailto:me@example.com"),
        CommandBarItem("Calendar event", "Calendar", key = "calendarEvent")
      )),
      CommandBarItem("Upload sales plan", "Upload"),
      CommandBarItem("Share analysis", "Share"),
      CommandBarItem("Download report", "Download")
    ),
    farItems = list(
      CommandBarItem("Grid view", "Tiles", iconOnly = TRUE),
      CommandBarItem("Info", "Info", iconOnly = TRUE)
    ),
    style = list(width = "100%")))

navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Data Center Energy 101', url = '#!/data-center-energy', key = 'dce', icon = 'D365TalentLearn'),
      list(name = 'Industry Trends', url = '#!/industry-trends', key = 'trends', icon = 'AnalyticsReport'),
      list(name = 'Company Analysis', url = '#!/company-analysis', key = 'analysis', icon = 'AnalyticsView'),
      list(name = 'Methods', url = '#!/methods', key = 'methods', icon = 'WebAppBuilderFragment'),
      list(name = 'ISAL', url = 'http://www.ucsb.edu/', key = 'isal', icon = 'MiniLink')
    ))
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Built with ❤ by Industrial Sustainability Analysis Lab", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to us at info_isal@ucsb.edu"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)


layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}

##########################################
############## SHINY ROUTER ##############
##########################################

router <- make_router(
  route("/", home_page),
  route("data-center-energy", dc_energy_101_page),
  route("industry-trends", industry_trends_page),
  route("company-analysis", company_analysis_page),
  route("methods", methods_page)
  )

# Add shiny.router dependencies manually: they are not picked up because they're added in a non-standard way.
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)

ui <- fluentPage(
  layout(router$ui),
  tags$head(
    tags$link(href = "main.css", rel = "stylesheet", type = "text/css"),
    shiny_router_script_tag
  ))
