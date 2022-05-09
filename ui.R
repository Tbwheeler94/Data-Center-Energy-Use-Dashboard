
################
#####Cards######
################

MainCard <- function(..., title = NULL) {
  Stack(
    class = "ms-depth-8",
    tokens = list(padding = 20, childrenGap = 5),
    style = 'border-radius: 5px; background-color: white; border-top: 8px solid #137AD1; text-align: center;',
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
    tokens = list(padding = 20, childrenGap = 20),
    style = 'border-radius: 5px; background-color: white; border-top: 8px solid #137AD1;',
    ...  
  )
}

GraphCard <- function(..., title = NULL) {
  Stack(
    class = "ms-depth-8",
    tokens = list(padding = 20, childrenGap = 5),
    style = 'border-radius: 5px; background-color: white; border-top: 8px solid #137AD1; max-height:500px; overflow-y: scroll; position: relative;',
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
        MainCard(Text('About the data', variant = "xxLarge", style = "text-align: left;"),
                 Text("We are a team of University of California, Santa Barbara based researchers aiming to increase transparency and understanding of trends in global data center energy use. This website is a dashboard for modelers, policy-makers, and the general public to gain insight into data currently being reported by many of the world largest technology companies. Our visualization uses aggregated energy data primarily collected from publicly disclosed corporate sustainability reports.", variant = "large", style = "text-align: left;"))
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
            Text(uiOutput("energy_reported_text"), variant = "xxLarge")
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
               MainCard(Text('Page Under Construction', variant = "xxLarge"),
                        FontIcon(iconName = "ConstructionCone", style = list(fontSize = 80)
                        )
               )
                        
      )
    )
  )
)

########################################################
######### TRENDS IN ENERGY REPORTING PAGE ##############
########################################################

reporting_trends_page <- makePage(
  
  div(
    Stack(style = "text-align: center; padding: 25px", Text("Trends in Data Center Energy Reporting Transparency", variant = "xxLarge", style = "color: #137AD1;")),
    Grid(
      GridItem(class = "ms-sm12 ms-xl12", 
               CompanyCard(
                 plotlyOutput('transparency_graph')
               )
      )
    )
  )
)

########################################################
############## ENERGY DATA TRENDS PAGE #################
########################################################

energy_data_trends <- makePage(
  
  div(Stack(style = "text-align: center; padding: 25px", Text("Annual Reported Energy Use By Year Across Reporting Companies", variant = "xxLarge", style = "color: #137AD1;")),
    Grid(
      GridItem(class = "ms-xl4"),
      GridItem(class = "ms-sm12 ms-xl4",
               Stack(class = "ms-depth-8",
                     tokens = list(padding = 20, childrenGap = 5),
                     style = 'border-radius: 5px; background-color: white; border-top: 8px solid #137AD1;',
                     Text("Select Year and Energy Reporting Scope", variant = "xLarge", style = "text-align: center;"),
                     br(),
                     Dropdown.shinyInput("input_year", 
                                         options = unique_years,
                                         value = "2020",
                                         placeHolder = "2020",
                                         dropdownWidth = 150,
                                         style = "width: 150px; margin: auto; font-size: 12pt;"),
                     Dropdown.shinyInput("input_reporting_scope", 
                                         options = unique_scope_selection,
                                         value = "Data Centers",
                                         placeHolder = "Data Centers",
                                         dropdownWidth = 150,
                                         style = "width: 150px; margin: auto; font-size: 12pt;")
               )
      )
    ),
    Grid(id = 'data-center-plots',
      GridItem(class = "ms-sm12 ms-xl12",
               GraphCard(Text("Data Center Electricity Use At Scale of KWh to 1s of TWh", variant = "large", style = "text-align: center;"),
                         br(),
                         plotOutput('data_centerplot')))
    ),
    Grid(id = 'company-wide-plot-1',
      GridItem(class = "ms-sm12 ms-xl12",
               GraphCard(Text("Company Wide Energy Use At Scale of KWh to 1s of GWh", variant = "large", style = "text-align: center;"),
                         br(),
                         plotOutput('company_wide_plot_1')))
    ),
    Grid(id = 'company-wide-plot-2',
      GridItem(class = "ms-sm12 ms-xl12",
               GraphCard(Text("Company Wide Energy Use At Scale of 10s of GWh", variant = "large", style = "text-align: center;"),
                         br(),
                         plotOutput('company_wide_plot_2')))
    ),
    Grid(id = 'company-wide-plot-3',
      GridItem(class = "ms-sm12 ms-xl12",
               GraphCard(Text("Company Wide Energy Use At Scale of 100s of GWh", variant = "large", style = "text-align: center;"),
                         br(),
                         plotOutput('company_wide_plot_3')))
    ),
    Grid(id = 'company-wide-plot-4',
      GridItem(class = "ms-sm12 ms-xl12",
               GraphCard(Text("Company Wide Energy Use At Scale of 1s of TWh", variant = "large", style = "text-align: center;"),
                         br(),
                         plotOutput('company_wide_plot_4')))
    ),
    Grid(id = 'company-wide-plot-5',
      GridItem(class = "ms-sm12 ms-xl12",
               GraphCard(Text("Company Wide Energy Use At Scale of 10s of TWh and Greater", variant = "large", style = "text-align: center;"),
                         br(),
                         plotOutput('company_wide_plot_5')))
    )
  )
)

########################################################
############## REPORTING TIMELINE PAGE #################
########################################################

reporting_timeline_page <- makePage(
  div(
    Stack(style = "text-align: center; padding: 25px", Text("Data Center Energy Reporting Transparency Timeline", variant = "xxLarge", style = "color: #137AD1;")),
    Grid(
      GridItem(class = "ms-sm12 ms-xl12", 
               Stack(class = "ms-depth-8 timeline-graph",
                 plotlyOutput('reporting_timeline')
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
      GridItem(class = "ms-sm0 ms-xl3"),
      GridItem(class = "ms-sm12 ms-xl6",
               Stack(
                 class = "ms-depth-8",
                 style = 'border-radius: 5px; background-color: white; border-top: 8px solid #137AD1;',
                 tokens = list(padding = 20, childrenGap = 10),
                 Text("Select A Company To View", variant = "xxLarge", style = "text-align: center;"),
                 Dropdown.shinyInput("selected_company", 
                                     options = unique_companies,
                                     value = "Google",
                                     placeHolder = "Google",
                                     dropdownWidth = 150,
                                     style = "width: 150px; margin: auto; font-size: 12pt;"),
                 downloadButton('download_standards'," Download all reported data (.csv)", style = "text-align: center; font-size: 12pt;"),
                 div(PrimaryButton.shinyInput("show_company_data_center_overview", text = "Read Data Center Overview", style = "margin-right: 10px;"),
                 PrimaryButton.shinyInput("show_company_energy_reporting_assessment_overview", text = "Read Energy Overview"), style = "margin: auto; margin-top: 10px;"),
                 dataTableOutput("selected_company_stats"),
                 div(reactOutput("company_data_center_overview"),
                     reactOutput("company_energy_reporting_assessment_overview"))
               )
      )
    ),
    #Grid(
    #  GridItem(class = "ms-sm12 ms-xl6",
    #           CompanyCard(
    #                       Text("Company Data Center Overview", variant = "xxLarge", style = "text-align: center;"),
    #                       Text(uiOutput("company_data_center_overview"), variant = "large"))
    #          ),
    #  GridItem(class = "ms-sm12 ms-xl6",
    #           CompanyCard(
    #             Text("Energy Report Assessment", variant = "xxLarge", style = "text-align: center;"), 
    #             Text(uiOutput("energy_reporting_assessment"), variant = "large"))
    #  )
    #),
    Grid(
      Stack(style = "text-align: center; padding: 25px", Text(uiOutput("company_profiles_title_1"), variant = "xxLarge", style = "color: #137AD1;")),
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
    Grid(Stack(style = "text-align: center; padding: 25px", Text(uiOutput("company_profiles_title_2"), variant = "xxLarge", style = "color: #137AD1;"))),
    Grid(id = "electricity-use-table",
      GridItem(class = "ms-sm0 ms-xl1"),
      GridItem(class = "ms-sm12 ms-xl10",
               CompanyCard(
                 Text("Electricity Use (TWh/yr)", variant = "large", style = "text-align: center;"),
                 div(dataTableOutput("electricity_use_table"), style = "width: 100%"),
                 Stack(horizontal = TRUE, style = "justify-content: end; color: #137AD1;", 
                       TooltipHost(content = "Values marked with asterisk are inferred, scroll down to Methodology table below to see how the value was calculated",
                       ActionButton(iconProps = list("iconName" = "Info"), text = "About This Table", style = "color: #137AD1;")))
               )
               
      ),
    ),
    Grid(id = "other-fuel-use-table",
      GridItem(class = "ms-sm0 ms-xl1"),
      GridItem(class = "ms-sm12 ms-xl10",
               CompanyCard(
                 Text("Other fuel use (TWh/yr)", variant = "large", style = "text-align: center;"),
                 div(dataTableOutput("other_fuel_use_table"), style = "width: 100%"),
                 Stack(horizontal = TRUE, style = "justify-content: end; color: #137AD1;", 
                       TooltipHost(content = "Values marked with asterisk are inferred, scroll down to Methodology table below to see how the value was calculated",
                       ActionButton(iconProps = list("iconName" = "Info"), text = "About This Table", style = "color: #137AD1;")))
               )
               
      )
    ),
    Grid(id = "ns-energy-use-table",
      GridItem(class = "ms-sm0 ms-xl1"),
      GridItem(class = "ms-sm12 ms-xl10",  id = "ns-energy-use-table",
               CompanyCard(
                 Text("Non-specified energy use (TWh/yr)", variant = "large", style = "text-align: center;"),
                 div(dataTableOutput("ns_energy_use_table"), style = "width: 100%"),
                 Stack(horizontal = TRUE, style = "justify-content: end; color: #137AD1;", 
                       TooltipHost(content = "Values marked with asterisk are inferred, scroll down to Methodology table below to see how the value was calculated",
                       ActionButton(iconProps = list("iconName" = "Info"), text = "About This Table", style = "color: #137AD1;")))
               )
               
      ),
    ),
    Grid(id = "pue-table",
      GridItem(class = "ms-sm0 ms-xl1"),
      GridItem(class = "ms-sm12 ms-xl10",
               CompanyCard(
                 Text("PUE", variant = "large", style = "text-align: center;"),
                 div(dataTableOutput("pue_table"), style = "width: 100%; overflow-x:auto;"),
                 Stack(horizontal = TRUE, style = "justify-content: end; color: #137AD1;", 
                       TooltipHost(content = "Values marked with asterisk are inferred, scroll down to Methodology table below to see how the value was calculated",
                       ActionButton(iconProps = list("iconName" = "Info"), text = "About This Table", style = "color: #137AD1;")))
               )
               
      )
    ),
    Grid(
      GridItem(class = "ms-sm0 ms-xl1"),
      GridItem(class = "ms-sm0 ms-xl10", plotOutput('transparency_over_time_plot'))
    ),
    Grid(
      GridItem(class = "ms-sm0 ms-xl1"),
      GridItem(class = "ms-sm12 ms-xl10",
               Stack(style = "text-align: center; padding: 25px;",Text(uiOutput("company_profiles_title_3"), variant = "xxLarge", style = "color: #137AD1;")),
               CompanyCard(div(dataTableOutput("methodology_table"), style = "width: 100%; overflow-x:auto;"),
                           div(PrimaryButton.shinyInput("learn-more", text = "Learn More", style = "width: 120px; font-style: bold; margin-right: 10px;"),
                               Text("All data are collected from publicly available sources and reviewed prior to being displayed on the website. Click the button to learn more about our methods.", variant = "large")),
                           div(PrimaryButton.shinyInput("report-issue", text = "Contact Us", style = "width: 120px; font-style: bold; margin-right: 10px;"),
                               Text("See an error in the reported data or have access to data you'd like our team to process? Contact us.", variant = "large")))
      ),
    ),
    Grid(
      GridItem(class = "ms-sm0 ms-xl1"),
      GridItem(class = "ms-sm12 ms-xl10",
               Stack(style = "text-align: center; padding: 25px;", Text(uiOutput("company_profiles_title_4"), variant = "xxLarge", style = "color: #137AD1;")),
               CompanyCard(
                 Text(variant = "large", style = "text-align: center;"),
                 div(dataTableOutput("sources_table"), style = "width: 100%;")
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
               MainCard(Text('Section Under Construction', variant = "xxLarge"),
                        FontIcon(iconName = "ConstructionCone", style = list(fontSize = 80))
                        )
      )
    )
  )
)

###########################################
######### CONTACT PAGE ###################
###########################################

contact_page <- makePage(
  div(
    Grid(
      GridItem(class = "ms-sm12 ms-xl12", style = "text-align: center",
               MainCard(Text('Section Under Construction', variant = "xxLarge"),
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
  div(Text(variant = "xxLarge", "Data Center Energy Use Dashboard", style = "color: white;"), class = "title"))

navigation <- Nav(
  className = 'sidenav',
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Data Center Energy 101', url = '#!/data-center-energy', key = 'dce', icon = 'D365TalentLearn'),
      list(name = 'Industry Trends', 
           expandAriaLabel = 'Expand Industry Trends',
           collapseAriaLabel = 'Collapse Industry Trends',
           links = list(
             list(name = 'Trends in Energy Reporting', url = '#!/reporting-trends', key = 'reporting-trends', icon = 'Trending12'),
             list(name = 'Energy Data Trends', url = '#!/energy-data-trends', key = 'data-trends', icon = 'Trending12'),
             list(name = 'Reporting Timeline', url = '#!/reporting-timeline', key = 'reporting-timeline', icon = 'TimelineProgress')), 
           isExpanded = FALSE
           ),
      list(name = 'Company Analysis', url = '#!/company-analysis', key = 'analysis', icon = 'ExploreData'),
      list(name = 'Methods', url = '#!/methods', key = 'methods', icon = 'WebAppBuilderFragment'),
      list(name = 'Contact Us', url = '#!/contact-us', key = 'contact', icon = 'Send'),
      list(name = 'ISA Lab Website', url = 'https://bren.ucsb.edu/people/eric-masanet', key = 'isal', icon = 'MiniLink')
    ))
  ),
  initialSelectedKey = 'home'
)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "mediumPlus", "Built by the Industrial Sustainability Analysis Lab at UCSB", block=TRUE),
  Text(variant = "mediumPlus", nowrap = FALSE, "If you'd like to connect, reach out to us at info@isalab.edu"),
  Text(variant = "mediumPlus", nowrap = FALSE, "All rights reserved.")
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
  route("reporting-trends", reporting_trends_page),
  route("energy-data-trends", energy_data_trends),
  route("reporting-timeline", reporting_timeline_page),
  route("company-analysis", company_analysis_page),
  route("methods", methods_page),
  route("contact-us", contact_page)
  )

# Add shiny.router dependencies manually: they are not picked up because they're added in a non-standard way.
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)

ui <- #secure_app(head_auth = tags$script(inactivity), #authentication
                 fluentPage(
                 tags$title("Data Center Energy Dashboard"),
                 autoWaiter(id = c(#add loading animations to industry trend graphs
                                   "transparency_graph", "data_centerplot", "company_wide_plot_1",
                                   "company_wide_plot_2", "company_wide_plot_3", "company_wide_plot_4",
                                   "company_wide_plot_5", "reporting_timeline",
                                   #add loading animations to company analysis page
                                   "company_profiles_title_1", "company_profiles_title_2", "company_profiles_title_3", "company_profiles_title_4",
                                   "selected_company_stats", "company_data_center_overview", "energy_reporting_assessment",
                                   "reported_energy_levels", "data_standards", "other_metrics",
                                   "electricity_use_table", "other_fuel_use_table",
                                   "ns_energy_use_table", "pue_table", "transparency_over_time_plot", "sources_table"), 
                            html = spin_2(), color = transparent(1), fadeout = TRUE),
                 useShinyjs(),
                 layout(router$ui),
                 tags$head(
                   tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
                   shiny_router_script_tag
                 ))
                 #)
