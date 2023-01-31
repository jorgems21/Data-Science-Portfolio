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
fluidPage(
  navbarPage(title = "Crime in Boston",id= "main",
             tabPanel("General",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("variable", "Districts:", choices = sort(districts), multiple = TRUE)

                        ),
                        mainPanel(DT::dataTableOutput("bus_table"))
                      )),
             navbarMenu("Map",
                        tabPanel("Summary"),
                        "----",
                        "Section header",
                        tabPanel("Table")
             )
  )
)
