
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
    style = 'border-radius: 5px; background-color: white; border-top: 8px solid #137AD1; text-align: center;',
    ...  
  )
}

CompanyCard <- function(..., title = NULL) {
  
  Stack(
    class = "ms-depth-8",
    tokens = list(padding = 20, childrenGap = 5),
    style = 'border-radius: 5px; background-color: white; border-top: 8px solid #137AD1;',
    ...  
  )
}

GraphCard <- function(..., title = NULL) {
  
  Stack(
    class = "ms-depth-8",
    tokens = list(padding = 20, childrenGap = 5),
    style = 'border-radius: 5px; background-color: white; border-top: 8px solid #137AD1; max-height:400px; overflow-y: scroll; position: relative; overflow-y: scroll;',
    ...  
  )
}

################################
##########Grid Layout###########
################################

Grid <- function (...) {
  div(
    class = "ms-Grid", dir = "ltr",
    style = "padding: 0px;",
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
                 style = "border-radius: 5px; background-color: white; border-top: 8px solid #137AD1;",
                 Text("We are a team of University of California, Santa Barbara based researchers aiming to increase transparency and understanding of trends in global data center energy use. This website is a dashboard for modelers, policy-makers, and the general public to gain insight into data currently being reported by many of the world largest technology companies. Our visualization uses aggregated energy data primarily collected from publicly disclosed corporate sustainability reports.", variant = "large"))
        ),
        GridItem(class = "ms-sm12 ms-xl4",
          HighlightsCard(
            FontIcon(iconName = "Calendar", style = list(fontSize = 60)),
            Text(uiOutput("years_reported"), variant = "mega", style = "color: #137AD1;"),
            Text("Years Reporting", variant = "xxLarge")
          )
        ),
        GridItem(class = "ms-sm12 ms-xl4",
          HighlightsCard(
            FontIcon(iconName = "ClipboardList", style = list(fontSize = 60)),
            Text(uiOutput("companies_reporting"), variant = "mega", style = "color: #137AD1;"),
            Text("Companies Reporting", variant = "xxLarge")
          )
        ),
        GridItem(class = "ms-sm12 ms-xl4",
          HighlightsCard(
            FontIcon(iconName = "TableComputed", style = list(fontSize = 60)),
            Text(uiOutput("energy_reported"), variant = "mega", style = "color: #137AD1;"),
            Text("Data Center Electricty Use Reported", variant = "xxLarge")
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
      GridItem(class = "ms-sm12 ms-xl12", 
               CompanyCard(
                 plotlyOutput('transparency_graph')
               )
      )
    ),
    Grid(
      GridItem(class = "ms-sm12 ms-xl12", 
               CompanyCard(
                 Dropdown.shinyInput("input_year", 
                                     options = unique_years,
                                     value = "2021",
                                     placeHolder = "2021",
                                     dropdownWidth = 150),
                 Dropdown.shinyInput("input_reporting_scope", 
                                     options = unique_scope_selection,
                                     value = "Data Centers",
                                     placeHolder = "Data Centers",
                                     dropdownWidth = 150)
               )
      )
    ),
    Grid(
      GridItem(class = "ms-sm12 ms-xl6",
               GraphCard(plotlyOutput('energy_use_aggregated'))),
      GridItem(class = "ms-sm12 ms-xl6",
               GraphCard())
    ),
    div(id = "test-container",
      Grid(
        GridItem(class = "ms-sm12 ms-xl6",
                 GraphCard()),
        GridItem(class = "ms-sm12 ms-xl6",
                 GraphCard())
      ),
      Grid(
        GridItem(class = "ms-sm12 ms-xl6",
                 GraphCard())
      )
    )
  )
)

##############################################
######### COMPANY ANALYSIS PAGE ##############
##############################################

company_analysis_page <- makePage(
  div(
    Grid(style = "display: flex;",
      GridItem(class = "ms-sm12 ms-xl3",
               Stack(
                 class = "ms-depth-8",
                 style = 'border-radius: 5px; background-color: white; border-top: 8px solid #137AD1;',
                 tokens = list(padding = 20, childrenGap = 20),
                 Text("Select company", variant = "xLarge"),
                 Dropdown.shinyInput("selected_company", 
                                     options = unique_companies,
                                     value = "Google",
                                     placeHolder = "Google",
                                     dropdownWidth = 150),
                 downloadButton('download_standards'," Download full profile (.csv)"),
                 dataTableOutput("selected_company_stats")
               )
      ),
      GridItem(class = "ms-sm12 ms-xl5",
               CompanyCard(
                           Text("Company Data Center Overview", variant = "xLarge"),
                           Text(uiOutput("company_data_center_overview"), variant = "mediumPlus"))
              ),
      GridItem(class = "ms-sm12 ms-xl4",
               CompanyCard(
                           Text("Energy Report Assessment", variant = "xLarge"), 
                           Text(uiOutput("energy_reporting_assessment"), variant = "mediumPlus"))
              )
    ),
    Grid(
      Stack(style = "text-align: center; padding: 25px", Text("Current Year Energy Reporting Snapshot", variant = "xxLarge", style = "color: #137AD1;")),
      GridItem(class = "ms-sm12 ms-xl4",                                               
               CompanyCard(
                 Text("Reported energy use levels", variant = "large", style = "text-align: center;"),
                 dataTableOutput("reported_energy_levels"),
                 Stack(horizontal = TRUE, style = "justify-content: end; color: #137AD1;", 
                       TooltipHost(content = "This datatable displays a company's reported energy use levels",
                       ActionButton(iconProps = list("iconName" = "Info"), text = "About This Table", style = "color: #137AD1;")))
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
      Stack(style = "text-align: center; padding: 25px", Text("Historical Energy Use Trend & Data", variant = "xxLarge", style = "color: #137AD1;")),
      GridItem(class = "ms-sm12 ms-xl6",
               CompanyCard(
                 Text("Electricity Use (TWh/yr)", variant = "large", style = "text-align: center;"),
                 div(DT::dataTableOutput("electricity_use_table"), style = "width: 100%") 
               )
               
      ),
      GridItem(class = "ms-sm12 ms-xl6",
               CompanyCard(
                 Text("Other fuel use (TWh/yr)", variant = "large", style = "text-align: center;"),
                 div(dataTableOutput("other_fuel_use_table"), style = "width: 100%")
               )
               
      )
    ),
    Grid(
      GridItem(class = "ms-sm12 ms-xl6",
               CompanyCard(
                 Text("Non-specified energy use (TWh/yr)", variant = "large", style = "text-align: center;"),
                 div(dataTableOutput("ns_energy_use_table"), style = "width: 100%")
               )
               
      ),
      GridItem(class = "ms-sm12 ms-xl6",
               CompanyCard(
                 Text("PUE", variant = "large", style = "text-align: center;"),
                 div(dataTableOutput("pue_table"), style = "width: 100%; overflow-x:auto;")
               )
               
      )
    ),
    Grid(
      Stack(style = "text-align: center; padding: 25px", Text("Methodology", variant = "xxLarge", style = "color: #137AD1;")),
      GridItem(class = "ms-sm12 ms-xl12",
               Stack(class = "ms-depth-8", tokens = list(padding = 20, childrenGap = 10),
                     style = 'border-radius: 5px; background-color: white; border-top: 8px solid #137AD1;',
                     Text("All company data are based on a rigorous review of publicly-available resources.", variant = "large"),
                     PrimaryButton(text = "Learn More", style = "width: 120px; font-style: bold;"),
                     Text("If you spot errors or have more recent data, please let us know!", variant = "large"),
                     PrimaryButton(text = "Report Issue", style = "width: 130px; font-style: bold;"))
      )
    ),
    Grid(
      Stack(style = "text-align: center; padding: 25px", Text("Sources Assessed", variant = "xxLarge", style = "color: #137AD1;")),
      GridItem(class = "ms-sm12 ms-xl12",
               CompanyCard(
                 Text("Energy Data", variant = "large", style = "text-align: center;"),
                 dataTableOutput("sources_table")
               )
      )
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
  img(src = "isalab.svg", class = "logo"),
  div(Text(variant = "xLarge", "Data Center Energy Use Dashboard", style = "color: white;"), class = "title"),
      style = list(width = "100%"))

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
  Text(variant = "medium", "Built by the Industrial Sustainability Analysis Lab", block=TRUE),
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
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
    shiny_router_script_tag
  ))
