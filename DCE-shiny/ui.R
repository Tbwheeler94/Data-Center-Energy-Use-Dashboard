#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

htmlTemplate("template.html",
             button = actionButton("action", "Action"),
             slider = sliderInput("x", "Input Value", 1, 100, 50)
)

