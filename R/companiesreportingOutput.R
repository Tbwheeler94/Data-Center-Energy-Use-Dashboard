buildcompaniesreportingOutput <- function(by_fuel_type_data) {
  
  length(unique(by_fuel_type_data$company))
  
}

########################################## NOTE TO SELF ########################################
#Scalable verison of number counter builder from blog counter site -- revisit if there is time##
################################################################################################


## Module constants --------------------------------------------------------
#
#F_COUNT_ICON_TEMPLATE <- "www/modules/data_icon/index.html"
#
#
## Module UI ---------------------------------------------------------------
#
#count_icon_ui <- function(id, icon = "icon-wallet", icon_text = "") {
#  ns <- NS(id)
#  
#  htmlTemplate(
#    filename = F_COUNT_ICON_TEMPLATE,
#    icon = icon,
#    count_to = uiOutput(ns("count_to")),
#    icon_text = icon_text
#  )
#}