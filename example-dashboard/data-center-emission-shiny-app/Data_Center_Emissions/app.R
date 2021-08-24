#Attaching relevant packages:

library(tidyverse) # Wickham et al. 2019
library(janitor) # Firke 2021
library(here) # Müller 2020
library(lubridate) # Grolemund & Wickham 2011
library(shiny) # Chang et al. 2021
library(shinythemes) # Chang 2021
library(shinydashboard) # Chang & Ribeiro 2018
library(jpeg) # Urbanek 2019
library(scales) # Wickham & Seidel 2020
library(bslib) # Sievert & Cheng 2021
library(sf) # Pebesma 2018
library(tmap) # Tennekes 2018
library(leaflet) # Cheng et al. 2021
library(leaflet.extras) # Karambelkar & Schloerke 2018
library(patchwork) # Pedersen 2020
library(plotly) # Sievert 2020

# COLOR PALETTE: https://colorbrewer2.org/#type=sequential&scheme=Reds&n=6





# Reading in and cleaning the main csv data file:

data_center_tech <- read_csv(here("Data_Center_Emissions",
                                  "data_center_tech.csv")) %>%
  clean_names() %>%
  mutate(reporting_year = as.Date(as.character(reporting_year),
                                  format = "%Y"))





# Reading in data center location csv:

data_center_locations <- read_csv(here("Data_Center_Emissions",
                                       "data_center_locations.csv"))

world_basemap <- read_sf(here("Data_Center_Emissions"),
                         layer = "World_Countries")





# Summarizing the data to derive variable totals per year:

data_sums <- data_center_tech %>%
  group_by(reporting_year) %>%
  summarize(data_energy_sum = sum(total_data_energy_use_mwh),
            company_energy_sum = sum(total_company_energy_use_mwh),
            scope_1_sum = sum(scope_1_emissions),
            scope_2_sum = sum(scope_2_emission),
            scope_3_sum = sum(scope_3_emission))





# Choices for y-axis:

choices_yaxis_t2.1 = c("Total Data Center Energy Use (MWh)" = "total_data_energy_use_mwh",
                       "Total Company Energy Use (MWh)" = "total_company_energy_use_mwh")

choices_yaxis_t2.2 = c("Total Scope 1 Emissions (tCO2eq.)" = "scope_1_emissions",
                       "Total Scope 2 Emissions (tCO2eq.)" = "scope_2_emission",
                       "Total Scope 3 Emissions (tCO2eq.)" = "scope_3_emission")

choices_yaxis_t3 = c("Total Data Center Energy Use (MWh)" = "total_data_energy_use_mwh",
                     "Total Company Energy Use (MWh)" = "total_company_energy_use_mwh",
                     "Scope 1 Emissions (tCO2eq.)" = "scope_1_emissions",
                     "Scope 2 Emissions (tCO2eq.)" = "scope_2_emission",
                     "Scope 3 Emissions (tCO2eq.)" = "scope_3_emission")

choices_yaxis_t4.1 = c("Total Data Center Energy Use (MWh)" = "data_energy_sum",
                       "Total Company Energy Use (MWh)" = "company_energy_sum")

choices_yaxis_t4.2 = c("Total Scope 1 Emissions (tCO2eq.)" = "scope_1_sum",
                       "Total Scope 2 Emissions (tCO2eq.)" = "scope_2_sum",
                       "Total Scope 3 Emissions (tCO2eq.)" = "scope_3_sum")





# Creating the user interface:

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  navbarPage("Global Data Center Energy Usage",





             # TAB 1: About:

             tabPanel("About",

                      mainPanel(
                        tags$div(
                          class = "header",
                          checked = NA,
                          tags$h2("Explore this app to understand the global growth and environmental impact of data centers"),
                          br(
                            style = "line-height: 10%;"),
                          tags$h4("Why data centers?"),
                          tags$p("Data centers can be thought of as the “brains” of the internet. Their role is to house servers which process, store, and communicate the data behind the myriad information services we rely upon every day, whether it be videos, emails, social media, online collaboration, or scientific computing. In 2018, data centers were estimated to have consumed around 205 terawatt-hours (TWh) of electricity - one percent of global electricity use."),
                          tags$p("In recent years, growth in new data centers has shifted primarily toward larger 'hyperscale' facilities. These facilities house thousands of servers in one location, as opposed to individual companies managing their own servers. Despite tremendous global growth and demand for processing power, total energy demand from new data centers is estimated to have increased by six percent between 2010 and 2018. Large U.S. and Chinese technology companies, such as Amazon, Facebook, Alibaba, and Baidu, have been the primary drivers in these new investments."),
                          tags$p("Despite this sector's large demand in global energy, publicly available data is disjointed, difficult to track, or completely unavailable. To achieve the Paris Climate Agreement emission reduction targets, it's critically salient that companies that operate data centers are more transparent in their reporting."),
                          br(),
                          tags$h4("About this application"),
                          tags$p("This application visualizes the current, publicly available data that has been voluntarily reported by the top 20 largest technology firms. This application reveals the unfolding story of data centers, via their growing energy use and carbon emissions. Tracking the progress of these companies' reductions in carbon emission, some have pledged to fully decarbonize their data centers and reduce their energy demand within the next 15 to 20 years."),
                          br(),
                          tags$h4("About the Industrial Sustainability Analysis Lab (ISAL)"),
                          tags$p("The data detailed in this application was collected by the ISAL at the University of California, Santa Barbara. ISAL provides insights in energy demand and scope emissions within the industrial sector.",
                                 tags$a(
                                   href = "https://bren.ucsb.edu/people/eric-masanet",
                                   "Click here"),
                                 'to learn more about ISAL.'))),

                      sidebarPanel(
                        tags$img(
                          src = "datacenter_image.jpg",
                          height = 500,
                          width = 425,
                          style = "display:block;
                                  margin-left: auto;
                                  margin-right: auto;"),
                        fluid = TRUE,
                        style = "background-color:#F7F9FF",
                        tags$p("Source:",
                               tags$a(
                                 href = "https://www.evernex.com/wp-content/uploads/2018/08/postimage-1024x585.jpg",
                                 "Evernex")),
                        tags$a(
                          href = "https://energyinnovation.org/2020/03/17/how-much-energy-do-data-centers-really-use/",
                          "Reference"),
                        'for writing this about page',
                        fluid = TRUE,
                        style = "background-color:#F7F9FF")),
             br(),





             # TAB 2: Company Analysis:

             tabPanel("Company Analysis",
                      sidebarLayout(

                        # TAB 2: widgets:

                        sidebarPanel(
                          selectInput(
                            inputId = "choose_company_t2",
                            label = h3("Choose Company:"),
                            choices = unique(
                              data_center_tech$company_name)),
                          checkboxGroupInput(
                            inputId = "choose_metric_t2.1",
                            label = h3("Choose Energy Metric:"),
                            choices = choices_yaxis_t2.1),
                          style = "background-color: #F7F9FF;
                                  box-shadow: 2px 3px #F7F9FF;",
                          fluid = TRUE,
                          tags$h4("About this plot"),
                          tags$p("This plot details the change in energy use by company (total and/or from only datacenters) between 2010 and 2019."),
                          checkboxGroupInput(
                            inputId = "choose_metric_t2.2",
                            label = h3("Choose Emissions Metric:"),
                            choices = choices_yaxis_t2.2),
                          style = "background-color: #F7F9FF;
                                  box-shadow: 2px 3px #F7F9FF;",
                          fluid = TRUE,
                          tags$h4("About this plot"),
                          tags$p("This plot details the change in a company's scopes one through three carbon emissions between 2010 and 2019.")),

                        # TAB 2: plots:

                        mainPanel(
                          plotOutput(
                            outputId = "companytab_plot1"),
                          plotOutput(
                            outputId = "companytab_plot2")))),
             br(),





             # TAB 3: Comparing the Top 6 U.S. Tech Companies:

             tabPanel("Comparing the Top 6 U.S. Tech Companies",
                      sidebarLayout(

                        # TAB 3: widgets:

                        sidebarPanel(
                          selected = "total_data_energy_use_mwh",
                          style = "background-color: #F7F9FF;
                                  box-shadow: 2px 3px #F7F9FF;",
                          fluid = TRUE,
                          selectInput(
                            inputId = "choose_metric_t3",
                            label = h3("Choose Metric for Comparison:"),
                            choices = choices_yaxis_t3),
                          br(),
                          tags$h4("About this plot"),
                          tags$p("This plot details the change in a company's energy use or emissions relative to the sum of all the companies in aggregate.")),

                        # TAB 3: plots:

                        mainPanel(
                          plotlyOutput(
                            outputId = "comparetab_plot",
                            height = 600,
                            width = 950)))),
             br(),





             # TAB 4: Industry-Wide Analysis:

             tabPanel("Industry-Wide Analysis",
                      sidebarLayout(

                        # TAB 4: widgets:

                        sidebarPanel(
                          checkboxGroupInput(
                            inputId = "choose_metric_t4.1",
                            label = h3("Choose Energy Metric:"),
                            choices = choices_yaxis_t4.1),
                            style = "background-color: #F7F9FF;
                                    box-shadow: 2px 3px #F7F9FF;",
                          fluid = TRUE,
                          tags$h4("About this plot"),
                          tags$p("This plot details the change in industry-wide energy use from the top 20 largest global tech companies (the sum of energy use from all companies) between 2010 and 2019."),
                          checkboxGroupInput(
                            inputId = "choose_metric_t4.2",
                            label = h3("Choose Emissions Metric:"),
                            choices = choices_yaxis_t4.2),
                            style = "background-color: #F7F9FF;
                                    box-shadow: 2px 3px #F7F9FF;",
                          fluid = TRUE,
                          tags$h4("About this plot"),
                          tags$p("This plot details the change in industry-wide carbon emissions from the top 20 largest global tech companies (the sum of carbon emissions from all companies) between 2010 and 2019.")),

                        # TAB 4: plots:

                        mainPanel(
                          plotOutput(
                            outputId = "industrytab_plot1"),
                         #plotOutput(
                           #outputId = "industrytab_plot2")
                        ))),
             br(),





             # TAB 5: Data Center Locations:

             tabPanel("Data Center Locations",

                      # TAB 5: widgets:

                      sidebarPanel(
                        style = "background-color: #F7F9FF;
                                box-shadow: 2px 3px #F7F9FF;",
                        fluid = TRUE,
                        radioButtons(
                          inputId = "pick_company_t5",
                          label = h3("Choose Company:"),
                          choices = unique(
                            data_center_locations$company),
                          selected = 1),
                        selectInput(
                          inputId = "pick_status_t5",
                          label = h3("Choose Data Center Status:"),
                          choices = c("Active",
                                      "Under construction"))),

                      # TAB 5: plots:

                      mainPanel(
                        leafletOutput(
                          outputId = "data_center_locations_plot_t5"))),
             br(),





             # TAB 6: Citations:

             tabPanel("Citations",
                      mainPanel(
                        tags$div(
                          class = "header",
                          checked = NA,
                          tags$h2("Citations"),
                          br(
                            style = "line-height: 10%;")),
                        tags$p("Chang, W. & Ribeiro, B.B. (2018). shinydashboard: Create dashboards with 'shiny' (Version 0.7.1)."),
                        tags$p("Chang, W. (2021). shinythemes: Themes for shiny (Version 1.2.0)."),
                        tags$p("Chang et al. (2021). shiny: Web application framework for R (Version 1.6.0)."),
                        tags$p("Cheng, J., Karambelkar, B., & Xie, Y. (2021). leaflet: Create interactive web maps with the JavaScript 'leaflet' library (Version 2.0.4.1)."),
                        tags$p("Firke, S. (2021). janitor: Simple tools for examining and cleaning dirty data."),
                        tags$p("Grolemund, G. & Wickham, H. (2011). Dates and times made easy with lubridate. Journal of Statistical Software, 40(3), 1-25."),
                        tags$p("Karambelkar, B. & Schloerke, B. (2018). leaflet.extras: Extra functionality for 'leaflet' package."),
                        tags$p("Müller, K. (2020). here: A simpler way to find your files."),
                        tags$p("Pebesma, E. (2018). Simple features for R: Standardized support for spatial vector data. The R Journal, 10(1), 439-446."),
                        tags$p("Pedersen, T.L. (2020). patchwork: The composer of plots."),
                        tags$p("RStudio Team. (2021). RStudio: Integrated development environment for R (Version 4.0.3). RStudio PBC, Boston, MA, USA."),
                        tags$p("Sievert, C. (2020). Interactive web-based data visualization with R, plotly, and shiny. Chapman and Hall/CRC, FL, USA."),
                        tags$p("Sievert, C. & Cheng, J. (2021). bslib: Custom 'bootstrap' 'sass' themes for 'shiny' and 'rmarkdown'."),
                        tags$p("Tennekes, M. (2018). tmap: Thematic maps in R. Journal of Statistical Software, 84(6), 1-39."),
                        tags$p("Urbanek, S. (2019). jpeg: Read and write JPEG images (Version 0.1-8.1)."),
                        tags$p("Wickham et al. (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686."),
                        tags$p("Wickham, H. & Seidel, D. (2020). scales: Scale functions for visualization.")))
             ))





# Creating the server:

server <- function(input,
                   output) {





  # TAB 2: Company Analysis plots:

  energy_emissions_reactive2.1 <- reactive({
    data_center_tech %>%
      filter(company_name == input$choose_company_t2) %>%
      pivot_longer("total_data_energy_use_mwh":"total_company_energy_use_mwh",
                   names_to = "data_type",
                   values_to = "value") %>%
      filter(data_type %in% as.character(input$choose_metric_t2.1))
    })

  energy_emissions_reactive2.2 <- reactive({
    data_center_tech %>%
      filter(company_name == input$choose_company_t2) %>%
      pivot_longer("scope_1_emissions":"scope_3_emission",
                   names_to = "data_type",
                   values_to = "value") %>%
      filter(data_type %in% as.character(input$choose_metric_t2.2))
  })

  output$companytab_plot1 <- renderPlot({
    y_label_t2.1 <- names(choices_yaxis_t2.1)[choices_yaxis_t2.1 == input$choose_metric_t2.1]
    p1 <- ggplot(data = energy_emissions_reactive2.1(),
                 aes_string(x = 'reporting_year',
                            y = 'value',
                            group = 'data_type',
                            color = 'data_type')) +
      geom_line(size = 0.8) +
      geom_point() +
      scale_y_continuous(labels = comma) +
      labs(title = "Energy Use",
           x = "Reporting Year",
           y = "Energy Use (mWh)",
           color = "") +
      theme_bw() +
      theme(plot.title = element_text(face = "bold",
                                      hjust = 0.5,
                                      size = 20),
            axis.title = element_text(hjust = 0.5,
                                      size = 15),
            axis.title.x = element_text(vjust = -0.8),
            axis.title.y = element_text(vjust = 2.5),
            axis.text = element_text(size = 12),
            legend.title = element_text(face = "bold",
                                        hjust = 0.55),
            legend.position = "bottom") +
      scale_color_manual(labels = c("total_data_energy_use_mwh" = "Total Data Center Energy Use (MWh)",
                                    "total_company_energy_use_mwh" = "Total Company Energy Use (MWh)"),
                         values = c("total_data_energy_use_mwh" = "#3182BD",
                                    "total_company_energy_use_mwh" = "#08519C"))

    y_label_t2.2 <- names(choices_yaxis_t2.2)[choices_yaxis_t2.2 == input$choose_metric_t2.2]
    p2 <- ggplot(data = energy_emissions_reactive2.2(),
                 aes_string(x = 'reporting_year',
                            y = 'value',
                            group = 'data_type',
                            color = 'data_type')) +
      geom_line(size = 0.8) +
      geom_point() +
      scale_y_continuous(labels = comma) +
      labs(title = "Carbon Emissions",
           x = "Reporting Year",
           y = "Emissions (tCO2eq.)",
           color = "") +
      theme_bw() +
      theme(plot.title = element_text(face = "bold",
                                      hjust = 0.5,
                                      size = 20),
            axis.title = element_text(hjust = 0.5,
                                      size = 15),
            axis.title.x = element_text(vjust = -0.8),
            axis.title.y = element_text(vjust = 2.5),
            axis.text = element_text(size = 12),
            legend.title = element_text(face = "bold",
                                        hjust = 0.55),
            legend.position = "bottom") +
      scale_color_manual(labels = c("scope_1_emissions" = "Total Scope 1 Emissions (tCO2eq.)",
                                    "scope_2_emission" = "Total Scope 2 Emissions (tCO2eq.)",
                                    "scope_3_emission" = "Total Scope 3 Emissions (tCO2eq.)"),
                         values = c("scope_1_emissions" = "#6BAED6",
                                    "scope_2_emission" = "#3182BD",
                                    "scope_3_emission" = "#08519C"))

    p1 + p2

    })





  # TAB 3: Comparing the Top 6 U.S. Tech Companies plots:

  top_6_us_companies <- data_center_tech %>%
    filter(company_name %in% c("Apple",
                               "Amazon",
                               "Google",
                               "Facebook",
                               "Microsoft",
                               "IBM"))

  output$comparetab_plot <- renderPlotly({
    y_label_t3 <- names(choices_yaxis_t3)[choices_yaxis_t3 == input$choose_metric_t3]
    ggplot(data = top_6_us_companies) +
      geom_area(position = 'fill',
                aes_string(x = "reporting_year",
                           y = input$choose_metric_t3,
                           fill = 'company_name')) +
      scale_fill_brewer(palette = "Blues",
                        direction = -1) +
      labs(title = "Comparison of the Top 6 U.S. Tech Companies",
           x = "Reporting Year",
           y = "Proportion of Energy Use (MWh)/Emissions (tCO2eq.)",
           fill = "Company") +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(face = "bold",
                                      size = 18),
            axis.title = element_text(hjust = 0.5,
                                      size = 13),
            axis.title.x = element_text(vjust = -0.8),
            axis.title.y = element_text(vjust = 2.5),
            axis.text = element_text(size = 12))

    })





  # TAB 4: Industry-Wide Analysis plots:

  industry_change_reactive4.1 <- reactive({
    data_sums %>%
      select(-(scope_1_sum:scope_3_sum)) %>%
      pivot_longer("data_energy_sum":"company_energy_sum",
                   names_to = "data_type",
                   values_to = "value") %>%
      filter(data_type %in% as.character(input$choose_metric_t4.1))
    })

  industry_change_reactive4.2 <- reactive({
    data_sums %>%
      select(-(data_energy_sum:company_energy_sum)) %>%
      pivot_longer("scope_1_sum":"scope_3_sum",
                   names_to = "data_type",
                   values_to = "value") %>%
      filter(data_type %in% as.character(input$choose_metric_t4.2))
    })

  output$industrytab_plot1 <- renderPlot({
    p1 <- ggplot(data = industry_change_reactive4.1(),
                 aes_string(x = "reporting_year",
                            y = 'value',
                            group = 'data_type',
                            fill = 'data_type')) +
      geom_area(size = 1.5) +
      scale_y_continuous(labels = comma) +
      labs(title = "Total Industry Energy Use",
           x = "Reporting Year",
           y = "Total Energy Use (MWh)",
           fill = "") +
      theme_bw() +
      theme(plot.title = element_text(face = "bold",
                                      hjust = 0.5,
                                      size = 20),
            axis.title = element_text(hjust = 0.5,
                                      size = 15),
            axis.title.x = element_text(vjust = -0.8),
            axis.title.y = element_text(vjust = 2.5),
            axis.text = element_text(size = 12),
            legend.title = element_text(face = "bold",
                                        hjust = 0.55),
            legend.position = "bottom") +
      scale_fill_manual(labels = c("data_energy_sum" = "Total Data Center Energy Use (MWh)",
                                   "company_energy_sum" = "Total Company Energy Use (MWh)"),
                        values = c("data_energy_sum" = "#3182BD",
                                   "company_energy_sum" = "#08519C"))

    p2 <- ggplot(data = industry_change_reactive4.2(),
                 aes_string(x = "reporting_year",
                            y = 'value',
                            group = 'data_type',
                            fill = 'data_type')) +
      geom_area(size = 1.5) +
      scale_y_continuous(labels = comma) +
      labs(title = "Total Industry Emissions",
           x = "Reporting Year",
           y = "Total Scope Emissions (tCO2eq.)",
           fill = "") +
      theme_bw() +
      theme(plot.title = element_text(face = "bold",
                                      hjust = 0.5,
                                      size = 20),
            axis.title = element_text(hjust = 0.5,
                                      size = 15),
            axis.title.x = element_text(vjust = -0.8),
            axis.title.y = element_text(vjust = 2.5),
            axis.text = element_text(size = 12),
            legend.title = element_text(face = "bold",
                                        hjust = 0.55),
            legend.position = "bottom") +
      scale_fill_manual(labels = c("scope_1_sum" = "Total Scope 1 Emissions (tCO2eq.)",
                                   "scope_2_sum" = "Total Scope 2 Emissions (tCO2eq.)",
                                   "scope_3_sum" = "Total Scope 3 Emissions (tCO2eq.)"),
                        values = c("scope_1_sum" = "#6BAED6",
                                   "scope_2_sum" = "#3182BD",
                                   "scope_3_sum" = "#08519C"))

    p1 + p2

    })


  #output$industrytab_plot2 <- renderPlot({
  #  ggplot(data = industry_change_reactive4.2(),
  #         aes_string(x = "reporting_year",
  #                    y = 'value',
  #                    group = 'data_type',
  #                    color = 'data_type')) +
  #    geom_line(size = 1.5) +
  #    geom_point() +
  #    scale_y_continuous(labels = comma) +
  #    labs(title = "Comparing Total Industry Scope Emissions",
  #         x = "Reporting Year",
  #         y = "Total Scope Emissions (tCO2eq.)",
  #         color = "Legend") +
  #    theme_bw() +
  #    theme(plot.title = element_text(face = "bold",
  #                                    hjust = 0.5,
  #                                    size = 20),
  #          axis.title = element_text(hjust = 0.5,
  #                                    size = 15),
  #          axis.title.x = element_text(vjust = -0.8),
  #          axis.title.y = element_text(vjust = 2.5),
  #          axis.text = element_text(size = 12),
  #          legend.title = element_text(face = "bold",
  #                                      hjust = 0.55)) +
  #    scale_color_manual(labels = c("scope_1_sum" = "Total Scope 1 Emissions (tCO2eq.)",
  #                                  "scope_2_sum" = "Total Scope 2 Emissions (tCO2eq.)",
  #                                  "scope_3_sum" = "Total Scope 3 Emissions (tCO2eq.)"),
  #                       values = c("scope_1_sum" = "#6BAED6",
  #                                  "scope_2_sum" = "#3182BD",
  #                                  "scope_3_sum" = "#08519C"))
  #  })





  # TAB 5: Data Center Locations:

  data_center_locations_reactive <- reactive({
    data_center_locations %>%
      filter(company %in% input$pick_company_t5) %>%
      filter(status %in% input$pick_status_t5)
    })

  output$data_center_locations_plot_t5 <- renderLeaflet({
    pal <- colorNumeric(
      palette = c('orange',
                  'dark orange',
                  'orange red',
                  'red',
                  'dark red'),
      domain = data_center_locations_reactive()$reported_electricity_use)

    leaflet(data) %>%
      setView(lng = -99,
              lat = 45,
              zoom = 2) %>%
      addTiles() %>%
      addCircles(data = data_center_locations_reactive(),
                 lat = ~ latitude,
                 lng = ~ longitude,
                 weight = 1,
                 radius = ~sqrt(reported_electricity_use)*450,
                 popup = ~as.character(status),
                 label = ~as.character(paste0("Reported Electricity Use: ",
                                              sep = " ",
                                              reported_electricity_use)),
                 color = ~pal(reported_electricity_use),
                 fillOpacity = 0.5)
    })
  }





# Running the application:

shinyApp(ui = ui,
         server = server)
