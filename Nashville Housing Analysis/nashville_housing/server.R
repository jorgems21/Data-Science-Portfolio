server <- function(input, output) {
  
  model <- train_model(df)
  
  # Calculate probability
  deal_probability <- reactive({
    predict_deal_probability(
      model = model,
      df = df,
      price = input$sale_price,
      value = input$total_value,
      city = input$property_city,
      year = input$year_built,
      beds = input$bedrooms,
      full_bath = input$full_bath,
      half_bath = input$half_bath
    )
  })
  
  # Calculate potential savings
  savings <- reactive({
    max(input$total_value - input$sale_price, 0)
  })
  
  # Output plots for descriptive analysis
  output$price_dist_plot <- renderPlot({
    plot_data <- df %>%
      group_by(Property.City) %>%
      summarise(Avg_Price = mean(Sale.Price, na.rm = TRUE))
    
    ggplot(plot_data, aes(x = reorder(Property.City, Avg_Price), y = Avg_Price)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Average Sale Price by City", x = "City", y = "Average Price") +
      theme_minimal()
  })
  output$good_deal_rate_plot <- renderPlot({
    plot_data <- df %>%
      group_by(Property.City) %>%
      summarise(Good_Deal_Rate = mean(GoodDeal, na.rm = TRUE))
    
    ggplot(plot_data, aes(x = reorder(Property.City, Good_Deal_Rate), y = Good_Deal_Rate)) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(title = "Good Deal Rate by City", x = "City", y = "Good Deal Rate") +
      theme_minimal()
  })
  output$age_by_price_plot <- renderPlot({
    ggplot(df, aes(x = Property.Age, y = Sale.Price)) +
      geom_point(alpha = 0.5, color = "red") +
      labs(title = "Property Age vs. Sale Price", x = "Property Age (years)", y = "Sale Price") +
      theme_minimal()
  })
  output$age_by_deal_rate_plot <- renderPlot({
    ggplot(df, aes(y = as.factor(Age_built), x = GoodDeal)) +
      geom_boxplot(fill = "purple", alpha = 0.6) +
      labs(title = "Property Age vs. Good Deal Rate", x = "Good Deal", y = "Property Age (Years)") +
      theme_minimal()
  })
  output$summary_table <- renderDT({
    summary_data <- df %>%
      group_by(Property.City) %>%
      summarise(
        Avg_Price = mean(Sale.Price, na.rm = TRUE),
        Avg_Value = mean(Total.Value, na.rm = TRUE),
        Good_Deal_Rate = mean(GoodDeal, na.rm = TRUE)
      )
    datatable(summary_data, options = list(pageLength = 10))
  })
  
  # Output probability
  output$probability <- renderText({
    paste0(round(deal_probability() * 100, 1), "%")
  })
  
  # Output savings
  output$savings <- renderText({
    paste0("$", format(savings(), big.mark = ","))
  })
  # Create gauge plot
  output$probability_gauge <- renderPlot({
    prob <- deal_probability()
    
    # Create data for semicircle
    theta <- seq(-pi/2, pi/2, length.out = 100)
    x <- cos(theta)
    y <- sin(theta)
    df <- data.frame(x = x, y = y)
    
    # Calculate needle position
    needle_theta <- -pi/2 + pi * prob
    needle_x <- cos(needle_theta)
    needle_y <- sin(needle_theta)
    
    ggplot() +
      geom_path(data = df, aes(x, y), size = 2) +
      geom_segment(aes(x = 0, y = 0, xend = needle_x, yend = needle_y),
                   color = "red", linewidth = 2) +
      coord_fixed(xlim = c(-1.1, 1.1), ylim = c(-0.2, 1.1)) +
      theme_void() +
      ggtitle("Deal Probability Gauge")
  })
  
  # Generate insights
  output$insights <- renderText({
    prob <- deal_probability()
    savings_val <- savings()
    
    if (prob > 0.7) {
      paste("High probability of a good deal! The ML model predicts this property has",
            round(prob * 100, 1), "% chance of being a profitable investment with potential savings of $",
            format(savings_val, big.mark = ","), ".")
    } else if (prob > 0.4) {
      paste("Moderate potential. The model suggests a", round(prob * 100, 1),
            "% chance of being a good deal. Consider negotiating the price or conducting more research.")
    } else {
      paste("Low probability of a good deal (", round(prob * 100, 1),
            "%). The ML model suggests looking for other properties or negotiating a lower price.")
    }
  })
  
  # model <- train_model(df)
  # xgb_model <- model_info$model
  # feature_names <- model_info$feature_names
  # Feature importance plot
  output$feature_importance <- renderPlot({
    importance_df <- data.frame(
      Feature = rownames(importance(model)),
      Importance = importance(model)[,1]
    )
    
    ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Features", y = "Importance Score",
           title = "Feature Importance in Deal Prediction")
  })
  
  # predict_deal <- function(new_data) {
  #   # Convert input to a data frame
  #   new_data <- as.data.frame(new_data)
  #   
  #   # Ensure categorical variables have at least two levels
  #   if (length(unique(new_data$Property.City)) < 2) {
  #     new_data$Property.City <- factor(new_data$Property.City, levels = unique(df$Property.City))
  #   }
  #   
  #   new_dummies <- model.matrix(~ . -1, data = new_data)
  #   colnames(new_dummies) <- make.names(colnames(new_dummies))
  #   new_dummies <- new_dummies[, feature_names, drop = FALSE]
  #   dnew <- xgb.DMatrix(data = new_dummies)
  #   pred <- predict(xgb_model, dnew)
  #   return(ifelse(pred > 0.5, "This could be a good deal!", "This might not be a good deal."))
  # }
  
  # observeEvent(input$predict, {
  #   new_data <- data.frame(
  #     Property.City = factor(input$property_city, levels = unique(df$Property.City)),
  #     YearBuilt = input$year_built,
  #     Bedrooms = input$bedrooms,
  #     FullBath = input$full_bath,
  #     HalfBath = input$half_bath
  #   )
  #   output$prediction_result <- renderText(predict_deal(new_data))
  # })
}