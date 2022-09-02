#################################################
#################################################
######### Section 1: Load packages ##############
#################################################
#################################################

# -------------------------------------------------
# Load packages -----------------------------------
# -------------------------------------------------
  
# Base shiny
library(shiny)

# Packages for basic data wrangling
library(here)
library(janitor)
library(tidyverse)
library(lubridate)
library(fastDummies)
library(glue)
library(tidyselect)

# Packages for data importing
library(feather) # Used for importing feather files from directory "source_data/transformed_data/"
#library(xlsx) - OLD, leftover from when data was imported using read.xlsx() in global
#library(readxl) - OLD, leftover from when data was imported using read.xlsx() in global

# Packages for developing highly functional datatables
library(DT) # Wrapper for the datatables javascript library (https://datatables.net/)
library(data.table)

# Packages for interactive plotting and graph styling
library(plotly)
library(visNetwork) # Used to construction the industry trends network graph
library(scales)
library(ggiraph)

# Packages for developing general UI aesthetic and functionality
library(shiny.router) # Enables appearance of multi-page functionality within app
library(shiny.fluent) # Wrapper for Microsoft's Fluent UI (https://developer.microsoft.com/en-us/fluentui#/)
library(shinyglide) # Enables construction of image carousel on the home page

# Packages enabling additional website interactivity
library(tidyselect)
library(shinyjs) # Enables use of javascript functions such as event listeners and toggling css classes
library(shinymanager) # Enables construction of a user authentication page
library(waiter) # Enables implementation of loading animations

# Packages used write contact form submissions to Data-Center-Energy-Dashboard-Contact-Submissions spreadsheet
library(googlesheets4) 
library(googledrive)

# Packages used for performance and scalability testing (only uncomment when needed)
#library(reactlog) # Enables generation of a report detailing steps of code execution
#library(tictoc) # Add tic() and toc() before and after a block of code, respectively, to print execution time
#library(profvis)

# Package use to deploy test version of the site to shinyapps.io. See Production & Testing --> 
# Deploying test version of application in the Developer's Guide for details
#library(rsconnect)

# Basic app setup -------------------------------

# Set cache location (following directions from: https://bit.ly/3Rvgy0U)
#shinyOptions(cache = cachem::cache_disk("./app_cache/cache/"))

########################################################
###### Suppress warnings ###############################
########################################################

#suppress groupby warning
options(dplyr.summarise.inform = FALSE)

###################################################################################################################################
###### Reference function which prepares takes raw dataset and transforms data to be visualization-ready ##########################
###################################################################################################################################

#source(here("R", "transformEnergyDataRaw.R"))

########################################################
###### Authorize user login (authentication) ###########
########################################################

#inactivity <- "function idleTimer() {
#    var t = setTimeout(logout, 120000);
#    window.onmousemove = resetTimer; // catches mouse movements
#    window.onmousedown = resetTimer; // catches mouse movements
#    window.onclick = resetTimer;     // catches mouse clicks
#    window.onscroll = resetTimer;    // catches scrolling
#    window.onkeypress = resetTimer;  //catches keyboard actions
#
#    function logout() {
#    window.close();  //close the window
#    }
#
#    function resetTimer() {
#    clearTimeout(t);
#    t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
#    }
#    }
#    idleTimer();"
#
#
## data.frame with credentials info
#credentials <- data.frame(
#  user = c("ucsb"),
#  password = c("isal"))

###########################################################################
###########################################################################
###### Section 2: Import pre-filtered feather data for use across app #####
###########################################################################
###########################################################################

data_sheet_energy_raw <- read_feather("source_data/transformed_data/data_sheet_energy_raw")
data_sheet_energy_transformed <- read_feather("source_data/transformed_data/data_sheet_energy_transformed")
data_sheet_company_raw <- read_feather("source_data/transformed_data/data_sheet_company_raw")
data_sheet_pue_raw <- read_feather("source_data/transformed_data/data_sheet_pue_raw")
data_of_transparency <- read_feather("source_data/transformed_data/data_of_transparency")
industry_transparency <- read_feather("source_data/transformed_data/industry_transparency")

################################################################################################
################################################################################################
###### Section 3: Create vectors, dataframes, and functions for use across the app #############
################################################################################################
################################################################################################

##################################################
################ Tab 1 - Home ####################
##################################################

# Creating big boxes for main tabs in the landing page (see ui for formatting css)
lp_main_box <-
  function(title_box,
           image_name,
           button_name,
           description) {
    div(
      class = "ms-depth-8 landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      div(description, class = "landing-page-box-description"),
      div(class = "landing-page-icon", FontIcon(
        iconName = image_name, style = list(fontSize = 40)
      )),
      actionButton(button_name, NULL, class = "landing-page-button")
    )
  }

lp_home_to_industry_box <-
  function(title_box,
           image_name,
           button_name,
           description) {
    div(
      class = "ms-depth-8 landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      div(description, class = "landing-page-box-description"),
      div(
        class = "buffer-page-icon",
        style = paste0(
          "background-image: url(",
          image_name,
          ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat;"
        )
      ),
      actionButton(button_name, NULL, class = "landing-page-button")
    )
  }

##################################################
######## Tab 2 - Data Center Energy 101 ##########
##################################################

##################################################
########### Tab 3 - Industry Trends ##############
##################################################

#Generate list of scope options for UI dropdown
unique_scope_selection <- list()
industry_trends_scopes <- c('Data Centers', 'Company Wide')

for (i in 1:length(industry_trends_scopes)) {
  unique_scope_selection[[i]] <-
    list(key = {
      industry_trends_scopes[i]
    },
    text = {
      industry_trends_scopes[i]
    })
}

#Generate list of years for UI dropdown
unique_years <- list()
industry_trends_years <-
  str_subset(sort(
    unique(data_sheet_energy_transformed$data_year),
    decreasing = TRUE
  ), "")

for (i in 1:length(industry_trends_years)) {
  unique_years[[i]] <- list(key = {
    industry_trends_years[i]
  },
  text = {
    industry_trends_years[i]
  })
}

#Generate list of unique companies from PUE sheet
unique_companies_pue <- list()
list_of_pue_companies <-
  str_subset(sort(unique(data_sheet_pue_raw$company), decreasing = FALSE), "")

for (i in 1:length(list_of_pue_companies)) {
  unique_companies_pue[[i]] <- list(key = {
    list_of_pue_companies[i]
  },
  text = {
    list_of_pue_companies[i]
  })
}

#Generate list of unique scopes from PUE sheet
unique_scopes_pue <- list()
list_of_pue_scopes <- c("Fleet Wide", "Individual Locations")

for (i in 1:length(list_of_pue_scopes)) {
  unique_scopes_pue[[i]] <- list(key = {
    list_of_pue_scopes[i]
  },
  text = {
    list_of_pue_scopes[i]
  })
}

#Generate list of unique scales for energy data trends plot
unique_scales <- list()
list_of_scales <-
  c("Up to 500 GWh", "Up to 1 TWh", "Up to 10 TWh", "10+ TWh")

for (i in 1:length(list_of_scales)) {
  unique_scales[[i]] <- list(key = {
    list_of_scales[i]
  },
  text = {
    list_of_scales[i]
  })
}

##################################################
########### Tab 4 - Company Profiles #############
##################################################

no_data <- data.frame(no_data_reported = "No data reported")
`%not_in%` <- purrr::negate(`%in%`)

#generate unique list of companies in alphabetical order and drop blank
unique_companies <- list()
companies <-
  str_subset(sort(unique(
    data_sheet_company_raw %>% pull(company_name)
  )), "")

for (i in 1:length(companies)) {
  unique_companies[[i]] <- list(key = {
    companies[i]
  },
  text = {
    companies[i]
  })
}

#########################################
########### Tab 5 - Methods #############
#########################################

##############################################
########### Tab 7 - Contact Page #############
##############################################

contact_referral_options <- list()
referal_option_list <-
  c("Website", "Newspaper", "Podcast", "Friend", "Other")

for (i in 1:length(referal_option_list)) {
  contact_referral_options[[i]] <-
    list(key = {
      referal_option_list[i]
    },
    text = {
      referal_option_list[i]
    })
}