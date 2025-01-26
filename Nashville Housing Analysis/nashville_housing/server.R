
# Define server
server <- function(input, output) {
  
  model <- randomForest(Sale.Price < Total.Value ~ Neighborhood + Year.Built + Bedrooms + Full.Bath + Half.Bath, 
                        data = df, ntree = 100)
  
  observeEvent(input$predict, {
    new_data <- data.frame(
      Sale.Price = input$sale_price,
      Total.Value = input$total_value,
      Neighborhood = input$neighborhood,
      Year.Built = input$year_built,
      Bedrooms = input$bedrooms,
      Full.Bath = input$full_bath,
      Half.Bath = input$half_bath
    )
    
    pred <- predict(model, new_data, type = "response")
    prediction_text <- ifelse(pred, "This could be a good deal!", "This might not be a good deal.")
    output$prediction_result <- renderText(prediction_text)
  })
}