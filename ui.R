
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
  tagList(contents)
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
                 girafeOutput('transparency_graph'),
                 Stack(horizontal = TRUE, style = "justify-content: end; color: #137AD1;", 
                       TooltipHost(content = "This graph displays the change in the number of companies reporting at different levels of transparency through time. The total height of the stacked bars changes through time because some companies were not founded until after 2007.",
                                   ActionButton(iconProps = list("iconName" = "Info"), text = "About This Table", style = "color: #137AD1;")))
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
      Grid(id = 'data-center-plot-1',
           GridItem(class = "ms-sm12 ms-xl12",
                    GraphCard(Text("Data Center Electricity Use At Scale of KWh to 100s of GWh", variant = "large", style = "text-align: center;"),
                              br(),
                              plotOutput('data_centerplot_1')))
      ),
      Grid(id = 'data-center-plot-2',
           GridItem(class = "ms-sm12 ms-xl12",
                    GraphCard(Text("Data Center Electricity Use At Scale of 1s of TWh", variant = "large", style = "text-align: center;"),
                              br(),
                              plotOutput('data_centerplot_2')))
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
                 br(),
                 plotlyOutput('reporting_timeline', width = "auto")
               )
      )
    )
  )
)

########################################################
############## LEASE/CLOUD NETWORK PAGE ################
########################################################

lease_cloud_network_page <- makePage(
  div(
    Stack(style = "text-align: center; padding: 25px", Text("Network of Data Center Lease/Cloud Providers", variant = "xxLarge", style = "color: #137AD1;")),
    br(),
    Stack(style = "text-align: center;", Text("hover to see relationship | click and drag to interact | scroll to zoom", variant = "medium", style = "color: gray;")),
    visNetworkOutput('lease_cloud_network', height = "71vh")
  )
)

########################################################
############## PUE TRENDS PAGE #########################
########################################################

pue_trends_page <- makePage(
  div(
    Stack(style = "text-align: center; padding: 25px", Text("Industry PUE Trends", variant = "xxLarge", style = "color: #137AD1;")),
    br(),
    Grid(
      GridItem(class = "ms-md12 ms-lg2 ms-xl3",
               HighlightsCard(
                 Text("Select Company", variant = "xLarge", style = "text-align: center;"),
                 NormalPeoplePicker.shinyInput("selected_company_pue",
                                     options = unique_companies_pue),
                 br(),
                 Text("Select PUE Scope", variant = "xLarge", style = "text-align: center;"),
                 Dropdown.shinyInput("selected_scope_pue",
                                     options = unique_scopes_pue,
                                     value = "Fleet Wide",
                                     placeHolder = "Fleet Wide",
                                     style = "width: 150px; margin: auto; font-size: 12pt;")
               )), #add dropdowns inside HighlightsCard when ready
      GridItem(class = "ms-md12 ms-lg10 ms-xl9",
               HighlightsCard(
                 br(),
                 plotlyOutput('pue_trends_plot', width = "auto")
               )) #add graph inside HighlightsCard when ready, specify height in css styling
    )
  )
)

##############################################
######### COMPANY ANALYSIS PAGE ##############
##############################################

company_analysis_page <- makePage(
  div(
    Grid(
      GridItem(class = "ms-md0 ms-lg2 ms-xl3"),
      GridItem(class = "ms-md12 ms-lg8 ms-xl6",
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
                 PrimaryButton.shinyInput("show_company_energy_reporting_assessment_overview", text = "Read Energy Overview"), style = "margin: auto; margin-top: 10px; margin-bottom: -10px"),
                 #dataTableOutput("selected_company_stats"),
                 div(reactOutput("company_data_center_overview"),
                     reactOutput("company_energy_reporting_assessment_overview"), style = "width: 200px;")
               )
      )
    ),
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
    Grid(id = "methodology-table",
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

#create function to set up styling of panes
pane <- function(header, paragraphs) {
  div(
    Sticky(
      div(
        style = "background-color: #137AD1; border-top: 1px solid; border-bottom: 1px solid; padding: 10px;",
        Text(header, variant = "large", style = "color: white;")
      )
    ),
    div(stringi::stri_rand_lipsum(paragraphs), style = "padding: 10px;")
  )
}

methods_page <- makePage(
  div(
    Grid(
      GridItem(class = "ms-sm0 ms-xl2"),
      GridItem(class = "ms-sm12 ms-xl8", style = "text-align: center",
               MainCard(div(Text('Methodology Overview', variant = "xxLarge", style = "color: #137AD1; margin-right: 15px; text-align: center;"),
                            FontIcon(iconName = "Settings", style = list(fontSize = 30, color = "#137AD1"))),
                        br(),
                        Text("This page provides a detailed review of our process for discovering, importing, and visualizing the publicly reported data made accessible through our dashboard.", variant = "large", style = "text-align: left;"),
                        br(),
                        Text("In an effort to align with our goal of increasing transparency and understanding of trends in global data center energy use, we aim to be as transparent and open as possible in describing our methodology and welcome any feedback from our viewers.", variant = "large", style = "text-align: left;")
               )
      )
    ),
    Stack(style = "text-align: center; padding: 25px", Text('General Approach to Methodology', variant = "xxLarge", style = "text-align: center; color: #137AD1;")),
    Grid(
      GridItem(class = "ms-sm0 ms-xl2"),
      GridItem(class = "ms-sm12 ms-xl8",
               includeHTML("www/accordion.html")
      )
    ),
  )
)

# Stack(style = "text-align: center; padding: 25px", Text('Categories of Methodological Notes', variant = "xxLarge", style = "text-align: center; color: #137AD1;")),
# Grid(
#   GridItem(class = "ms-sm0 ms-xl2"),
#   GridItem(class = "ms-sm12 ms-xl8", style = "text-align: center",
#            MainCard(div(Text('Methodological Notes Overview', variant = "xxLarge", style = "color: #137AD1; margin-right: 15px; text-align: center;"),
#                         FontIcon(iconName = "Settings", style = list(fontSize = 30, color = "#137AD1"))),
#                     br(),
#                     Text("In the absence of required standards for publicly reporting energy use data, individual companies report their energy use using a wide range of units and methodologies. To capture as much data as possible for each company, our data processing team sometimes needs to use indirect calculations or estimation to find energy values. When one or more of these methods are used, data values reported on the “Company Analysis” page are accompanied by a methodological note which allows dashboard viewers to trace the listed value back to the original report. Below are our current categories of methods.", variant = "large", style = "text-align: left;")
#            )
#   )
# ),
# Grid(
#   GridItem(class = "ms-sm0 ms-xl2"),
#   GridItem(class = "ms-sm12 ms-xl8",
#            includeHTML("www/accordion2.html")
#   )
# )

###########################################
######### CONTACT PAGE ###################
###########################################

contact_page <- makePage(
  div(
    Grid(
      GridItem(class = "ms-sm12 ms-xl2"),
      GridItem(class = "ms-sm12 ms-xl8", style = "text-align: center",
               MainCard(Text('Contact Us', variant = "xxLarge", style = "text-align: center; color: #137AD1;"),
                        br(),
                        tags$form(id = "submission-form",
                        br(),
                        Stack(
                        div(Text("First Name", variant = "large"),
                        div(TextField.shinyInput("first_name_input", required = TRUE), style = "width: 200px; padding-top: 5px;"), style = "margin-right: 50px; text-align: left;"),
                        div(Text("Last Name", variant = "large"),
                        div(TextField.shinyInput("last_name_input"), style = "width: 200px;padding-top: 5px;"), style = "text-align: left;"), horizontal = TRUE),
                        br(),
                        div(Text("Email", variant = "large", style = "text-align: left;"), style = "text-align: left;"),
                        div(TextField.shinyInput("user_email_input", required = TRUE), style = "width: 300px; padding-top: 5px;"),
                        br(),
                        div(Text("How Did You Hear About Us?", variant = "large", style = "text-align: left;"), style = "text-align: left;"),
                        div(Dropdown.shinyInput("referral_input", options = contact_referral_options), style = "width: 100px; padding-top: 5px;"),
                        br(),
                        div(Text("Message", variant = "large", style = "text-align: left;"), style = "text-align: left;"),
                        div(TextField.shinyInput("user_message_input", required = TRUE, multiline = TRUE, style = "height: 200px;"), style = "width: fill; padding-top: 5px;"),
                        br(),
                        div(PrimaryButton.shinyInput("contact-form-submit", text = "Submit", style = "width: 110px; height: 40px; font-style: bold; margin-right: 10px; float: left;"))),
                        div(id = 'thank-you-for-submission', tags$p("Submission Successful"), style = "text-align: left;"),
                        div(id = 'missing-fields', tags$p("*Please fill out missing field"), style = "text-align: left; color: red;"),
                        div(id = 'invalid-email', tags$p("*Please enter a valid email address (must include @ and .)"), style = "text-align: left; color: red;")
               )
      )
    )
  )
)

###########################################
######### ABOUT ISA LAB ###################
###########################################

about_page <- makePage(
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
  img(src = "isalab-logo.png", class = "logo"),
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
             list(id = 'industry-trends-first-element', name = 'Energy Reporting Trends', url = '#!/reporting-trends', key = 'reporting-trends', icon = 'ReportDocument'),
             list(name = 'Energy Data Trends', url = '#!/energy-data-trends', key = 'data-trends', icon = 'Trending12'),
             list(name = 'Reporting Timeline', url = '#!/reporting-timeline', key = 'reporting-timeline', icon = 'TimelineProgress'),
             list(name = 'Industry Relationships', url = '#!/lease-cloud-network', key = "lease-cloud-network", icon = "SplitObject"),
             list(id = 'industry-trends-last-element', name = 'PUE Trends', url = '#!/pue-trends', key = 'pue-trends', icon = 'BIDashboard')), 
           isExpanded = FALSE
           ),
      list(name = 'Single Company Analysis', url = '#!/company-analysis', key = 'analysis', icon = 'ExploreData'),
      list(name = 'Methods', url = '#!/methods', key = 'methods', icon = 'Settings'),
      list(name = 'Contact Us', url = '#!/contact-us', key = 'contact', icon = 'Send'),
      list(name = 'About This Project', url = '#!/about-us', key = 'about', icon = 'TestBeakerSolid'),
      list(name = 'ISA Lab Website', url = 'https://carlobroderick.wixsite.com/isalab', key = 'isal', icon = 'MiniLink')
    ))
  ),
  initialSelectedKey = 'home'
)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "mediumPlus", "Built by the Industrial Sustainability Analysis Lab at UCSB", block=TRUE),
  Text(variant = "mediumPlus", nowrap = FALSE, "© ISA Lab 2022"),
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
  route("lease-cloud-network", lease_cloud_network_page),
  route("pue-trends", pue_trends_page),
  route("company-analysis", company_analysis_page),
  route("methods", methods_page),
  route("contact-us", contact_page),
  route("about-us", about_page)
  )

# Add shiny.router dependencies manually: they are not picked up because they're added in a non-standard way.
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)

preloader_html <- makePage(div(img(src='https://static.wixstatic.com/media/1ab7d5_947e63da0487445b8ef205972c867761~mv2.png/v1/fill/w_239,h_80,al_c,q_85,usm_0.66_1.00_0.01,enc_auto/industrial%20sustainability%20analysis%20lab_l.png', align = "center"),
                               h3("Welcome to the Data Center Energy Use Dashboard"), 
                               spin_pixel()))

ui <- #secure_app(head_auth = tags$script(inactivity), #authentication
                 fluentPage(
                 tags$title("Data Center Energy Dashboard"),
                 useWaiter(), 
                 waiterPreloader(html = preloader_html, color = "#c6e1f7", fadeout = 50),
                 autoWaiter(id = c(#add loading animations for home page
                                   "years_reported", "companies_reporting", "energy_reported",
                                    #add loading animations to industry trend graphs
                                   "transparency_graph", "data_centerplot_1", "data_centerplot_2", "company_wide_plot_1",
                                   "company_wide_plot_2", "company_wide_plot_3", "company_wide_plot_4",
                                   "company_wide_plot_5", "reporting_timeline",
                                   #add loading animations to company analysis page
                                   "company_profiles_title_1", "company_profiles_title_2", "company_profiles_title_3", 
                                   "company_profiles_title_4", "selected_company_stats", "reported_energy_levels", 
                                   "data_standards", "other_metrics", "electricity_use_table", "other_fuel_use_table",
                                   "ns_energy_use_table", "pue_table", "transparency_over_time_plot", "sources_table"), 
                            html = spin_2(), color = transparent(1), fadeout = TRUE),
                 useShinyjs(),
                 layout(router$ui),
                 tags$head(
                   tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
                   shiny_router_script_tag
                 ))
                 #)
