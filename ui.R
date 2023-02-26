
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

GridItemHome <- function (..., class = "ms-sm12") {
  div(
    class = paste("ms-Grid-col", class),
    style = "padding: 0px;",
    ...
  )
}

GridItemDownload <- function (..., class = "ms-sm12") {
  div(
    class = paste("ms-Grid-col ms-depth-8", class),
    style = "padding: 20px; border-radius: 5px; background-color: white; border-top: 8px solid #137AD1;",
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
      GridItemHome(class = "ms-sm12 ms-xl12", style="margin-top: -15px;", 
        glide(
             height = "250px",
             custom_controls = div(class="glide-controls", glideControls(list(tags$button(class = "ms-Button ms-Button--primary root-14 prev-screen", style="opacity: 0.8; cursor: pointer; height: 250px;")), list(tags$button(class = "ms-Button ms-Button--primary root-14 next-screen", id = "banner_button_new_page", style = "opacity: 0.8; cursor: pointer; height: 250px;"), div(`data-glide-el`="controls",tags$button(class="last-screen", id = "banner_button_last", style = "opacity: 0.8; cursor: pointer; height: 250px;", `data-glide-dir` = "<<", FontIcon(iconName = "ChevronRightSmall", style = list(fontSize = 20))))))),
             next_label = FontIcon(iconName = "ChevronRightSmall", style = list(fontSize = 20)),
             previous_label = FontIcon(iconName = "ChevronLeftSmall", style = list(fontSize = 20)),
             screen(
               div(class="container", 
                   tags$img(src="bg1.svg", class="ms-sm12 ms-xl12 home-images", height="250px"),
                   div(class="centered", style="width: 230px; padding-bottom: 30px;", Text("Welcome to MovingBits", variant = "xxLarge", style = "color: #FFFFFF;")),
                   div(class="centered-bottom-text", style="width: 230px;", Text("How much energy does the internet use? Click below!", variant = "large", style = "color: #FFFFFF;")),
                   div(class="centered-bottom-bubble", Text(FontIcon(iconName = "CircleFill", style = "color: #137AD1; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;")))
               )
             ),
             screen(
               div(class="container", 
                 div(class="centered", style="padding-bottom: 10px;", Text(uiOutput("years_reported"), variant = "mega", style = "color: #FFFFFF;")),
                 div(class="centered-bottom-text", Text("Years Reporting", variant = "xxLarge", style = "color: #FFFFFF")),
                 div(class="centered-bottom-bubble", Text(FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #137AD1; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;"))),
                 tags$img(src="bg2.svg", class="ms-sm12 ms-xl12 home-images", height="250px"),
               )
             ),
             screen(
               div(class="container",
                   tags$img(src="bg3.svg", class="ms-sm12 ms-xl12 home-images", height="250px"),
                   div(class="centered", Text(uiOutput("companies_reporting"), variant = "mega", style = "color: #FFFFFF;")),
                   div(class="centered-bottom-text", Text("Companies Reporting", variant = "xxLarge", style = "color: #FFFFFF")),
                   div(class="centered-bottom-bubble", Text(FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #137AD1; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;")))
               )
             ),
             screen(
               div(class="container",
                   tags$img(src="bg4_1.svg", class="ms-sm12 ms-xl12 home-images", height="250px"),
                   div(class="centered", style="padding-bottom: 70px;", Text(uiOutput("energy_reported"), variant = "mega", style = "color: #FFFFFF;")),
                   div(class="centered-bottom-text", style="width: 250px;", Text(uiOutput("energy_reported_text"), variant = "xLarge", style = "color: #FFFFFF")),
                   div(class="centered-bottom-bubble", Text(FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #FFFFFF; margin: 5px;"), FontIcon(iconName = "CircleFill", style = "color: #137AD1; margin: 5px;")))
               )
             )
        )
      )
    ),
    Grid(
      GridItem(class = "ms-sm12 ms-xl4",
               lp_main_box(image_name = "D365TalentLearn",
                           button_name = 'jump_to_dc_energy_101', title_box = "Data Center Energy 101",
                           description = 'Understand the basics behind data center energy')
      ),
      GridItem(class = "ms-sm12 ms-xl4",
               lp_main_box(image_name = "BIDashboard",
                           button_name = 'jump_to_home_to_industry_trends', title_box = "Industry Trends",
                           description = 'Scrutinize reporting and energy data trends on an industry level')
      ),
      GridItem(class = "ms-sm12 ms-xl4",
               lp_main_box(image_name = "ExploreData",
                           button_name = 'jump_to_company_analysis', title_box = "Company Analysis",
                           description = 'Explore an individual company’s performance')
      )
    ),
    Grid(
      GridItem(class = "ms-sm12 ms-xl4",
               lp_main_box(image_name = "Settings",
                           button_name = 'jump_to_methodology', title_box = "Methodology",
                           description = 'Learn more about our approach')
      ),
      GridItem(class = "ms-sm12 ms-xl4",
               lp_main_box(image_name = "Send",
                           button_name = 'jump_to_contact', title_box = "Contact Us",
                           description = 'Tell us an error in our reported data')
      ),
      GridItem(class = "ms-sm12 ms-xl4",
               lp_main_box(image_name = "TestBeakerSolid",
                           button_name = 'jump_to_about', title_box = "About the Project",
                           description = 'Read about the developers and creators behind this app')
      )
    )
  )
)

###########################################
###### Home to Industry Trends Page #######
###########################################

home_to_industry_trends_page <- makePage(
  div(
    Stack(style = "text-align: left; padding: 25px", Text("Click an industry trend box to access the plot", variant = "xxLarge", style = "color: #137AD1;")),
    Grid(
      GridItem(class = "ms-sm12 ms-xl4",
                      lp_home_to_industry_box(image_name = "home_to_industry_button_reporting_trends", 
                                  button_name = 'jump_to_reporting_trends', title_box = "Energy Reporting Trends",
                                  description = 'Discover how many companies are reporting at certain energy scopes')
      ),
      GridItem(class = "ms-sm12 ms-xl4",
                      lp_home_to_industry_box(image_name = "home_to_industry_button_energy_data_trends", 
                                  button_name = 'jump_to_energy_data_trends', title_box = "Energy Data Trends",
                                  description = 'Compare companies on levels of electricity reported, both on a data center and company wide scope')
      ),
      GridItem(class = "ms-sm12 ms-xl4",
                      lp_home_to_industry_box(image_name = "home_to_industry_button_timeline", 
                                  button_name = 'jump_to_timeline', title_box = "Energy Reporting Timeline",
                                  description = 'Visualize the timeline of data center energy reporting')
      )
    ),
    Grid(
      GridItem(class = "ms-sm12 ms-xl6",
                      lp_home_to_industry_box(image_name = "home_to_industry_button_industry_relationships", 
                                  button_name = 'jump_to_industry_relationships', title_box = "Industry Relationships",
                                  description = 'Analyze the connections between companies based on data center hosting')
      ),
      GridItem(class = "ms-sm12 ms-xl6",
                      lp_home_to_industry_box(image_name = "home_to_industry_button_PUE_trends", 
                                  button_name = 'jump_to_PUE_trends', title_box = "PUE Trends",
                                  description = 'Uncover company PUE data in relation to industry PUE trends')
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
      reactOutput("transparency_graph_explainer"),
      reactOutput("transparency_graph_download"),
      div(style = "display: flex; flex-direction: row; justify-content: flex-end; flex-wrap: wrap; gap: 15px; padding-right: 10px;",
          PrimaryButton.shinyInput("show_transparency_graph_explainer", iconProps = list("iconName" = "Help"), text = "Help"),
          downloadLink("download_transparency_graph", PrimaryButton.shinyInput("fdtri", iconProps = list("iconName" = "Camera"), text = "Save Image (.png)")),
          TooltipHost(content = "Select a download option in the dropdown to the right", downloadLink("download_transparency_data", PrimaryButton.shinyInput("fdtrd", iconProps = list("iconName" = "Download"), text = "Download Data"))),
          Dropdown.shinyInput("transparency_dataset_options",
                              placeholder = ".csv",
                              value = ".csv",
                              options = unique_tag_options)
      ),
      GridItem(class = "ms-sm12 ms-xl12", 
               CompanyCard(
                 # plotOutput('transparency_graph')
                 girafeOutput('transparency_graph', width = "100%")
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
      Grid(id = 'energy-data-trends-plot',
        reactOutput("energy_data_graph_explainer"),
        div(style = "display: flex; flex-direction: row; justify-content: flex-end; flex-wrap: wrap; gap: 15px; padding-right: 10px;",
          PrimaryButton.shinyInput("show_energy_data_graph_explainer", iconProps = list("iconName" = "Help", "color" = "#137AD1"), text = "Help"),
          downloadLink("download_energy_data_trends_graph", PrimaryButton.shinyInput("fdedti", iconProps = list("iconName" = "Camera"), text = "Save Image (.png)")),
          TooltipHost(content = "Select a download option in the dropdown to the right", downloadLink("download_energy_data", PrimaryButton.shinyInput("fdedtd", iconProps = list("iconName" = "Download"), text = "Download Data"))),
          Dropdown.shinyInput("energy_trends_dataset_options",
                              placeholder = "Selected dataset (.csv)",
                              value = "Selected dataset (.csv)",
                              options = unique_download_options)
        ),
        GridItem(class = "ms-sm12  ms-xl3",
                 CompanyCard(
                       Text("Select Year", variant = "xLarge", style = "text-align: center;"),
                       Dropdown.shinyInput("input_year", 
                                           options = unique_years,
                                           value = "2020",
                                           placeHolder = "2020",
                                           dropdownWidth = 150,
                                           style = "width: 150px; margin: auto; font-size: 12pt;"),
                       br(),
                       Text("Select Scope(s)", variant = "xLarge", style = "text-align: center;"),
                       Stack(style = "margin-top: 0px;",
                         Checkbox.shinyInput("reporting_scope_dc", label = "Data Centers", value  = TRUE),
                         Checkbox.shinyInput("reporting_scope_cw", label = "Company Wide", value  = FALSE),
                         tokens = list(padding = 10, childrenGap = 10)
                       ),
                       Text("Select Scale", variant = "xLarge", style = "text-align: center;"),
                       ChoiceGroup.shinyInput("input_scale", 
                                           options = unique_scales,
                                           value = "Up to 1 TWh",
                                           placeHolder = "Up to 1 TWh",
                                           dropdownWidth = 150,
                                           style = "width: 150px; margin: auto; font-size: 12pt;")
                 )
        ),
        GridItem(class = "ms-sm12 ms-xl9",
                 plotOutput('energy_data_trendsplot'))
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
      reactOutput("reporting_timeline_explainer"),
      div(style = "display: flex; flex-direction: row; justify-content: flex-end; flex-wrap: wrap; gap: 15px; padding-right: 10px;",
          PrimaryButton.shinyInput("show_reporting_timeline_explainer", iconProps = list("iconName" = "Help"), text = "Help"),
          downloadLink("download_timeline_graph", PrimaryButton.shinyInput("fdtii", iconProps = list("iconName" = "Camera"), text = "Save Image (.png)")),
          TooltipHost(content = "Select a download option in the dropdown to the right", downloadLink("download_timeline_data", PrimaryButton.shinyInput("fdtid", iconProps = list("iconName" = "Download"), text = "Download Data"))),
          Dropdown.shinyInput("timeline_dataset_options",
                              placeholder = ".csv",
                              value = ".csv",
                              options = unique_tag_options)
      ),
      GridItem(class = "ms-sm12 ms-xl12", 
               Stack(class = "ms-depth-8 timeline-graph",
                 br(),
                 girafeOutput('reporting_timeline', width = "auto")
               )
      )
    )
  )
)

########################################################
############## LEASE/CLOUD NETWORK PAGE ################
########################################################

lease_cloud_network_page <- makePage(
  div(style = "text-align: center;",
      Text("Network of Data Center Lease/Cloud Providers", variant = "xxLarge", style = "color: #137AD1; padding-bottom: 15px;"),
      br(),
      reactOutput("network_graph_explainer"),
      div(style = "display: flex; flex-direction: row; justify-content: flex-end; flex-wrap: wrap; gap: 15px; padding-right: 10px;",
          PrimaryButton.shinyInput("show_network_graph_explainer", iconProps = list("iconName" = "Help"), text = "Help"),
          downloadLink("download_network_graph", PrimaryButton.shinyInput("fdiri", iconProps = list("iconName" = "Camera"), text = "Save Interactive Image (.html)")),
          TooltipHost(content = "Select a download option in the dropdown to the right", downloadLink("download_network_data", PrimaryButton.shinyInput("fdird", iconProps = list("iconName" = "Download"), text = "Download Data"))),
          Dropdown.shinyInput("network_dataset_options",
                              placeholder = ".csv",
                              value = ".csv",
                              options = unique_tag_options)
      ),
      div(style = "text-align: left;", visNetworkOutput('lease_cloud_network', height = "62vh")),
  )
)

# <button id='show_network_graph_explainer' class='shiny-bound-input action-button ms-Button ms-Button--primary root-129' data-is-focusable='true' style='float: right;'>
#   <span class='ms-Button-flexContainer flexContainer-130' data-automationid='splitbuttonprimary'>
#   <i data-icon-name='Info' aria-hidden='true' class='ms-Icon root-32 css-87 ms-Button-icon icon-79' style='font-family: FabricMDL2Icons; color: white;'></i>
#   <span class='ms-Button-textContainer textContainer-131'>
#   <span class='ms-Button-label label-133' style = 'font-weight: normal;'>
#   Help
# </span>
#   </span>
#   </span>
#   </button>

# ActionButton(iconProps = list("iconName" = "Info", "styles" = list(root = list("color" = "white;"))), text = "About This Table",
# style = "color: white; background-color: #137AD1; width: 175px; float: right;"))
########################################################
############## PUE TRENDS PAGE #########################
########################################################

pue_trends_page <- makePage(
  div(
    Stack(style = "text-align: center; padding: 25px", Text("Industry PUE Trends", variant = "xxLarge", style = "color: #137AD1;")),
    Grid(
      reactOutput("pue_graph_explainer"),
      div(style = "display: flex; flex-direction: row; justify-content: flex-end; flex-wrap: wrap; gap: 15px; padding-right: 10px;",
        PrimaryButton.shinyInput("show_pue_graph_explainer", iconProps = list("iconName" = "Help"), text = "Help"),
        downloadLink("download_pue_graph", PrimaryButton.shinyInput("fdpuei", iconProps = list("iconName" = "Camera"), text = "Save Image (.png)")),
        TooltipHost(content = "Select a download option in the dropdown to the right", downloadLink("download_pue_data", PrimaryButton.shinyInput("fdpued", iconProps = list("iconName" = "Download"), text = "Download Data"))),
        Dropdown.shinyInput("pue_dataset_options",
                            placeholder = "Selected dataset (.csv)",
                            value = "Selected dataset (.csv)",
                            options = unique_download_options)
        # TooltipHost(content = "Select a download option in the dropdown to the right",
        #             downloadLink("download_pue_data", tags$button(class = "ms-Button ms-Button--primary root-102", `data-is-focusable` = "true",
        #                                                           tags$span(class = "ms-Button-flexContainer flexContainer-103", `data-automationid` = "splitbuttonprimary",
        #                                                                     icon("download", class = "fa-lg", style = "padding: 5px;"),
        #                                                                     tags$span(class = "ms-Button-textContainer textContainer-104", 
        #                                                                               tags$span(class = "ms-Button-label label-106", "Download Data")))))),
      )
    ),
    Grid(
      GridItem(class = "ms-sm 12 ms-md12 ms-lg4 ms-xl3",
               HighlightsCard(
                 Text("Select Company", variant = "xLarge", style = "text-align: center;"),
                 Dropdown.shinyInput("selected_company_pue",
                                     multiSelect = TRUE,
                                     placeholder = "Google",
                                     value = "Google",
                                     options = unique_companies_pue),
                 br(),
                 Text("Select PUE Scope", variant = "xLarge", style = "text-align: center;"),
                 ChoiceGroup.shinyInput("selected_scope_pue",
                                     options = unique_scopes_pue,
                                     value = "Fleet Wide",
                                     placeHolder = "Fleet Wide",
                                     style = "width: 150px; margin: auto; font-size: 12pt;")
               )
      ),
      GridItem(class = "ms-sm12 ms-md12 ms-lg8 ms-xl9",
               HighlightsCard(
                 br(),
                 girafeOutput('pue_trends_plot', width = "auto")
               )
      ),
    )
  )
)

##############################################
######### COMPANY ANALYSIS PAGE ##############
##############################################

company_analysis_page <- makePage(
  div(
    Grid(
      GridItem(class = "ms-md0 ms-lg0 ms-xl2"),
      GridItem(class = "ms-md12 ms-lg12 ms-xl8",
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
                 # downloadButton('download_standards'," Download all reported data (.csv)", style = "text-align: center; font-size: 12pt;"),
                 div(
                   PrimaryButton.shinyInput("show_company_data_center_overview", text = "Read Data Center Overview", style = "margin-right: 10px;"),
                   PrimaryButton.shinyInput("show_company_energy_reporting_assessment_overview", text = "Read Energy Overview"),
                   reactOutput("company_data_center_overview"),
                   reactOutput("company_energy_reporting_assessment_overview"),
                   style = "margin: auto; margin-top: 10px; margin-bottom: -10px"
                 ),
                 #dataTableOutput("selected_company_stats"),
                 # div(reactOutput("company_data_center_overview"),
                 #     reactOutput("company_energy_reporting_assessment_overview"))
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
                 div(dataTableOutput("sources_table"), style = "width: 100%;"),
                 Stack(horizontal = TRUE, style = "justify-content: end;",
                       PrimaryButton.shinyInput("show_sources_assessed_teaching_bubble", text = "About This Table", style = "margin-right: 10px;")),
                 reactOutput('sources_assessed_teaching_bubble'),
               )
      )
    )
  )
)
#list(unique(data_sheet_company$company_name))

###########################################
######### COMPANY ANALYSIS NEW DESIGN ##############
###########################################

new_company_analysis_page <- makePage(
  div(
    Grid(
      GridItem(class = "ms-md0 ms-lg0 ms-xl2"),
      GridItem(class = "ms-md12 ms-lg12 ms-xl8",
               Stack(
                 class = "ms-depth-8",
                 style = 'border-radius: 5px; background-color: white; border-top: 8px solid #137AD1;',
                 tokens = list(padding = 20, childrenGap = 10),
                 Text("Select A Company To View", variant = "xxLarge", style = "text-align: center;"),
                 Dropdown.shinyInput("selected_company_new", 
                                     options = unique_companies,
                                     value = "Google",
                                     placeHolder = "Google",
                                     dropdownWidth = 150,
                                     style = "width: 150px; margin: auto; font-size: 12pt;"),
                 div(
                   PrimaryButton.shinyInput("show_company_data_center_overview_new", text = "Read Data Center Overview", style = "margin-right: 10px;"),
                   PrimaryButton.shinyInput("show_company_energy_reporting_assessment_overview_new", text = "Read Energy Overview"),
                   reactOutput("company_data_center_overview_new"),
                   reactOutput("company_energy_reporting_assessment_overview_new"),
                   style = "margin: auto; margin-top: 10px; margin-bottom: -10px"
                 ),
                 br(),
                 downloadButton('download_standards'," Download all reported data (.csv)", style = "text-align: center; font-size: 12pt;"),
               )
      )
    ),
    Grid(id = "tab-panel",
         GridItem(class = "ms-sm12 ms-lg12 ms-xl12",
            CompanyCard(
              Pivot(
                PivotItem(headerText = ActionButton.shinyInput("ca_data_tables", text = "Reporting Metrics"),
                  Grid(
                    Stack(style = "text-align: center; padding: 25px", Text(uiOutput("company_profiles_title_1_new"), variant = "xxLarge", style = "color: #137AD1;")),
                    GridItem(class = "ms-sm12 ms-xl4",                                               
                             CompanyCard(
                               Text("Reported energy use levels", variant = "large", style = "text-align: center;"),
                               dataTableOutput("reported_energy_levels_new"),
                               Stack(horizontal = TRUE, style = "justify-content: end; color: #137AD1;", 
                                     TooltipHost(content = "This datatable displays a company's reported energy use levels",
                                                 ActionButton(iconProps = list("iconName" = "Info"), text = "About This Table", style = "color: #137AD1;")))
                             )
                    ),
                    GridItem(class = "ms-sm12 ms-xl4",                                               
                             CompanyCard(
                               Text("Data standards", variant = "large", style = "text-align: center;"),
                               dataTableOutput("data_standards_new")
                             )
                    ),
                    GridItem(class = "ms-sm12 ms-xl4",
                             CompanyCard(
                               Text("Other metrics reported", variant = "large", style = "text-align: center;"),
                               dataTableOutput("other_metrics_new")
                             )
                    )
                  )
                ),
                PivotItem(headerText = ActionButton.shinyInput("ca_data_tables", text = "Energy Data Tables"),
                          Grid(Stack(style = "text-align: center; padding: 25px", Text(uiOutput("company_profiles_title_2_new"), variant = "xxLarge", style = "color: #137AD1;"))),
                          br(),
                          Grid(id = "electricity-use-table-new",
                               GridItem(class = "ms-sm0 ms-xl1"),
                               GridItem(class = "ms-sm12 ms-xl10",
                                  CompanyCard(
                                    Text("Electricity Use (TWh/yr)", variant = "large", style = "text-align: center;"),
                                    div(dataTableOutput("electricity_use_table_new"), style = "width: 100%"),
                                    Stack(horizontal = TRUE, style = "justify-content: end; color: #137AD1;", 
                                          TooltipHost(content = "Values marked with asterisk are inferred, scroll down to Methodology table below to see how the value was calculated",
                                                      ActionButton(iconProps = list("iconName" = "Info"), text = "About This Table", style = "color: #137AD1;")))
                                    
                                  )
                               )
                          ),
                          br(),
                          Grid(id = "other-fuel-use-table-new",
                               GridItem(class = "ms-sm0 ms-xl1"),
                               GridItem(class = "ms-sm12 ms-xl10",
                                  CompanyCard(
                                    Text("Other fuel use (TWh/yr)", variant = "large", style = "text-align: center;"),
                                    div(dataTableOutput("other_fuel_use_table_new"), style = "width: 100%"),
                                    Stack(horizontal = TRUE, style = "justify-content: end; color: #137AD1;", 
                                          TooltipHost(content = "Values marked with asterisk are inferred, scroll down to Methodology table below to see how the value was calculated",
                                                      ActionButton(iconProps = list("iconName" = "Info"), text = "About This Table", style = "color: #137AD1;")))
                                  )
                               )
                          ),
                          br(),
                          Grid(id = "ns-energy-use-table-new",
                               GridItem(class = "ms-sm0 ms-xl1"),
                               GridItem(class = "ms-sm12 ms-xl10",
                                  CompanyCard(
                                    Text("Non-specified energy use (TWh/yr)", variant = "large", style = "text-align: center;"),
                                    div(dataTableOutput("ns_energy_use_table_new"), style = "width: 100%"),
                                    Stack(horizontal = TRUE, style = "justify-content: end; color: #137AD1;", 
                                          TooltipHost(content = "Values marked with asterisk are inferred, scroll down to Methodology table below to see how the value was calculated",
                                                      ActionButton(iconProps = list("iconName" = "Info"), text = "About This Table", style = "color: #137AD1;")))
                                  )
                               )
                          ),
                          br(),
                          Grid(id = "pue-table-new",
                               GridItem(class = "ms-sm0 ms-xl1"),
                               GridItem(class = "ms-sm12 ms-xl10",
                                  CompanyCard(
                                    Text("PUE", variant = "large", style = "text-align: center;"),
                                    div(dataTableOutput("pue_table_new"), style = "width: 100%;"),
                                    Stack(horizontal = TRUE, style = "justify-content: end; color: #137AD1;", 
                                          TooltipHost(content = "Values marked with asterisk are inferred, scroll down to Methodology table below to see how the value was calculated",
                                                      ActionButton(iconProps = list("iconName" = "Info"), text = "About This Table", style = "color: #137AD1;")))
                                  )
                               )
                          )),
                PivotItem(headerText = "Company Reporting Timeline",
                          Grid(
                            GridItem(class = "ms-sm0 ms-xl1"),
                            GridItem(class = "ms-sm0 ms-xl10", plotOutput('transparency_over_time_plot_new'))
                          )),
                PivotItem(headerText = ActionButton.shinyInput("ca_references", text = "References"),
                          Grid(id = "methodology-table-new",
                               GridItem(class = "ms-sm0 ms-xl1"),
                               GridItem(class = "ms-sm12 ms-xl10",
                                        Stack(style = "text-align: center; padding: 25px;",Text(uiOutput("company_profiles_title_3_new"), variant = "xxLarge", style = "color: #137AD1;")),
                                        CompanyCard(div(dataTableOutput("methodology_table_new"), style = "width: 100%; overflow-x:auto;"),
                                                    div(PrimaryButton.shinyInput("learn-more-new", text = "Learn More", style = "width: 120px; font-style: bold; margin-right: 10px;"),
                                                        Text("All data are collected from publicly available sources and reviewed prior to being displayed on the website. Click the button to learn more about our methods.", variant = "large")),
                                                    div(PrimaryButton.shinyInput("report-issue-new", text = "Contact Us", style = "width: 120px; font-style: bold; margin-right: 10px;"),
                                                        Text("See an error in the reported data or have access to data you'd like our team to process? Contact us.", variant = "large")))
                               ),
                          ),
                          Grid(id = "sources-assessed-new",
                               GridItem(class = "ms-sm0 ms-xl1"),
                               GridItem(class = "ms-sm12 ms-xl10",
                                        Stack(style = "text-align: center; padding: 25px;", Text(uiOutput("company_profiles_title_4_new"), variant = "xxLarge", style = "color: #137AD1;")),
                                        CompanyCard(
                                          Text(variant = "large", style = "text-align: center;"),
                                          div(dataTableOutput("sources_table_new"), style = "width: 100%;"),
                                          Stack(horizontal = TRUE, style = "justify-content: end;",
                                                PrimaryButton.shinyInput("show_sources_assessed_teaching_bubble_new", text = "About This Table", style = "margin-right: 10px;")),
                                          reactOutput('sources_assessed_teaching_bubble_new'),
                                        )
                               ))
                  )
              )
            )
          )
    )
  )
)

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
               MainCard(Text('About the data', variant = "xxLarge", style = "text-align: left;"),
                        Text("We are a team of University of California, Santa Barbara based researchers aiming to increase transparency and understanding of trends in global data center energy use. This website is a dashboard for modelers, policy-makers, and the general public to gain insight into data currently being reported by many of the world largest technology companies. Our visualization uses aggregated energy data primarily collected from publicly disclosed corporate sustainability reports.", variant = "large", style = "text-align: left;"))
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
  img(src = "isalab-logo.svg", class = "logo"),
  img(src = "moving-bits-logo.svg", style = "height: 40px; padding-top: 11px; padding-left: 6.5px;")
  #div(Text(variant = "xxLarge", "Data Center Energy Use Dashboard", style = "color: white;"), class = "title")
  )

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
      list(name = 'New Company Analysis', url = '#!/company-analysis-new', key = 'analysis-new', icon = 'ExploreData'),
      list(name = 'Methods', url = '#!/methods', key = 'methods', icon = 'Settings'),
      list(name = 'Contact', url = '#!/contact-us', key = 'contact', icon = 'Send'),
      list(name = 'About', url = '#!/about-us', key = 'about', icon = 'TestBeakerSolid'),
      list(name = 'ISA Lab Website', url = 'https://carlobroderick.wixsite.com/isalab', target="_blank", rel="noopener noreferrer", key = 'isal', icon = 'MiniLink')
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
  route("home-to-industry-trends", home_to_industry_trends_page),
  route("data-center-energy", dc_energy_101_page),
  route("reporting-trends", reporting_trends_page),
  route("energy-data-trends", energy_data_trends),
  route("reporting-timeline", reporting_timeline_page),
  route("lease-cloud-network", lease_cloud_network_page),
  route("pue-trends", pue_trends_page),
  route("company-analysis", company_analysis_page),
  route("company-analysis-new", new_company_analysis_page),
  route("methods", methods_page),
  route("contact-us", contact_page),
  route("about-us", about_page)
  )

# Add shiny.router dependencies manually: they are not picked up because they're added in a non-standard way.
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)

preloader_html <- makePage(div(
                           #p("Welcome to the Data Center Energy Use Dashboard", style = "margin-bottom: 20vh; font-size: 2.5vw; font-weight: bold;"),
                           img(src = "moving-bits-logo-no-line.svg", style = "margin-bottom: 10vh; height: 10vh"),
                           br(),
                           div(style="display: inline-flex; margin-bottom: 10vh",
                                 img(src='energy_icon.svg', style="height: auto; width: 10vw;"),
                                 div(style="vertical-align: center; margin-left: 10px; margin-right: 10px", spin_three_bounce()),
                                 img(src='server.svg', style="height: auto; width: 13vw;")), #server pic was from this link: https://www.freepik.com/free-photos-vectors/server-illustration
                           div(style="display: inline-flex; position: absolute; right:0px; bottom:0px;",
                             h4("Built by the", style= "margin-right: 10px;"),
                             img(src='isalab-logo.svg', style = "width: 179.25px; height: 60px; margin-right: 10px; margin-bottom: 10px;")
                           )))

ui <- #secure_app(head_auth = tags$script(inactivity), #authentication
                 fluentPage(
                 use_cicerone(),
                 tags$title("Data Center Energy Dashboard"),
                 useWaiter(),
                 waiterPreloader(html = preloader_html, color = "#137AD1", fadeout = 50),
                 autoWaiter(id = c(#add loading animations for home page
                                   "years_reported", "companies_reporting", "energy_reported",
                                    #add loading animations to industry trend graphs
                                   "transparency_graph","energy_data_trendsplot", "reporting_timeline", "pue_trends_plot",
                                   #add loading animations to company analysis page
                                   "company_profiles_title_1", "company_profiles_title_2", "company_profiles_title_3", 
                                   "company_profiles_title_4", "selected_company_stats", "reported_energy_levels", 
                                   "data_standards", "other_metrics", "electricity_use_table", "other_fuel_use_table",
                                   "ns_energy_use_table", "pue_table", "transparency_over_time_plot", "sources_table"), 
                            html = spin_2(), color = transparent(1), fadeout = TRUE),
                 useShinyjs(),
                 layout(router$ui),
                 tags$head(
                   tags$style("@import url(https://use.fontawesome.com/releases/v6.2.0/css/all.css);"),
                   tags$script(HTML('
                           Shiny.addCustomMessageHandler("jsCode",
                           function(message) {
                           eval(message.value);
                           });'
                   )),
                   tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
                   shiny_router_script_tag
                 ))
                 #)
