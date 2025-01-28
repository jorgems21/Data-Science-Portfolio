server <- function(input, output) {
  
  model_info <- train_model(df)
  xgb_model <- model_info$model
  feature_names <- model_info$feature_names
  
  
  predict_deal <- function(new_data) {
    # Convert input to a data frame
    new_data <- as.data.frame(new_data)
    
    # Ensure categorical variables have at least two levels
    if (length(unique(new_data$Property.City)) < 2) {
      new_data$Property.City <- factor(new_data$Property.City, levels = unique(df$Property.City))
    }
    
    new_dummies <- model.matrix(~ . -1, data = new_data)
    colnames(new_dummies) <- make.names(colnames(new_dummies))
    new_dummies <- new_dummies[, feature_names, drop = FALSE]
    dnew <- xgb.DMatrix(data = new_dummies)
    pred <- predict(xgb_model, dnew)
    return(ifelse(pred > 0.5, "This could be a good deal!", "This might not be a good deal."))
  }
  
  observeEvent(input$predict, {
    new_data <- data.frame(
      Property.City = factor(input$property_city, levels = unique(df$Property.City)),
      YearBuilt = input$year_built,
      Bedrooms = input$bedrooms,
      FullBath = input$full_bath,
      HalfBath = input$half_bath
    )
    output$prediction_result <- renderText(predict_deal(new_data))
  })
}