
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
    style = "padding: 10px; text-align: center",
    ...
  )
}

################################
############## UI ##############
################################
ui <- fluentPage(
  
  MainCard(title = "About the Data",
       Text("We are a team of University of California, Santa Barbara based researchers aiming to increase transparency and understanding of trends in global data center energy use. This website is a dashboard for modelers, policy-makers, and the general public to gain insight into data currently being reported by many of the world largest technology companies. Our visualization uses aggregated energy data primarily collected from publicly disclosed corporate sustainability reports.", variant = "large")),
  
  Grid(
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