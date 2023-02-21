#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
fluidPage(theme = shinytheme("flatly"),
  navbarPage(title = "Crime in Boston",id= "main",
             tabPanel("General",
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("districts", "Districts:", choices = sort(districts), multiple = TRUE)
                          ,selectizeInput("crimes", "Top 10 Crime Types:", choices = sort(top10crimes$OFFENSE_CODE_GROUP), multiple = TRUE)
                          ,dateRangeInput('dateRange',
                                          label = 'Date range input: yyyy-mm-dd',
                                          start = min(clean_df$OCCURRED_ON_DATE), end = max(clean_df$OCCURRED_ON_DATE)
                          )
                          ,actionButton("run", "Run")

                        ),
                        mainPanel(plotOutput("test_plot"))
                      )),
             navbarMenu("Map",
                        tabPanel("Summary"),
                        "----",
                        "Section header",
                        tabPanel("Table")
             )
  )
)
