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
  navbarPage(title = "Title",
             tabItem(
               tabName = "main1",
               fluidRow(
                 column(5,
                        box(width = 15,
                            title = "Number and Type of Crime by District",
                            color = "green", ribbon = TRUE, title_side = "top right",
                            height = "1000",
                            checkboxGroupInput("checkGroup", 
                                               h3("Checkbox group"),
                                               #choices = offenseCodeGrp,
                                               #selected = 1
                                               ),
                        ),
                 ),
                 # column(7,
                 #        box(width = 20,
                 #            title = "Rate of Incident over District", ribbon = TRUE, title_side = "top right",
                 #            column(width = 12, height = "auto", plotOutput("plot"))
                 #        ),
                 # ),
               ),
             )
  )

    # # Application title
    # titlePanel("Old Faithful Geyser Data"),
    # 
    # # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #         plotOutput("distPlot")
    #     )
    # )
)
