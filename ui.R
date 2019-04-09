#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Property price trends in your area"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(textInput(inputId = "postcode", label = "Enter postcode:",
                           value = "AB101AB", width = NULL, placeholder = NULL),
                 actionButton("go", "Go"),
                 h3("About this data"),
                 textOutput("description"),
                 h4("Date issued:"),
                 textOutput("dateIssued"),
                 h4("Date modified:"),
                 textOutput("dateModified"),
                 h4("Next update due:"),
                 textOutput("nextUpdateDue")
       ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
