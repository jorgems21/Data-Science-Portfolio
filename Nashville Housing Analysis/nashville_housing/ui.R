# Define UI
ui <- fluidPage(
  titlePanel("Nashville Housing Deal Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("sale_price", "Sale Price:", value = 200000, min = 10000, max = 1000000, step = 1000),
      numericInput("total_value", "Total Value:", value = 250000, min = 10000, max = 1000000, step = 1000),
      selectInput("neighborhood", "Neighborhood:", choices = unique(df$Neighborhood)),
      numericInput("year_built", "Year Built:", value = 2000, min = 1900, max = 2023),
      numericInput("bedrooms", "Bedrooms:", value = 3, min = 1, max = 10),
      numericInput("full_bath", "Full Bath:", value = 2, min = 1, max = 5),
      numericInput("half_bath", "Half Bath:", value = 1, min = 0, max = 5),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      textOutput("prediction_result")
    )
  )
)