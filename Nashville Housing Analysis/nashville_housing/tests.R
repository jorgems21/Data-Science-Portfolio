df
summary(df)



## MODELLING

model_info <- train_model(df)
xgb_model <- model_info$model
feature_names <- model_info$feature_names
new_data <- data.frame(lapply(new_data, function(x) {
  if (is.factor(x) && length(levels(x)) <= 1) as.character(x) else x
}))


str(new_data)
predict_deal <- function(new_data) {
  # Convert input to a data frame
  new_data <- as.data.frame(new_data)
  #new_data <- new_data %>% mutate(across(where(is.character), as.factor))
  
  #new_data$Property.City <- factor(new_data$Property.City, levels = levels(df$Property.City))
  
  # Ensure categorical variables have at least two levels
  #if (length(unique(new_data$Property.City)) < 2) {
  #  new_data$Property.City <- factor(new_data$Property.City, levels = unique(df$Property.City))
  #}
  
  new_dummies <- model.matrix(~ . -1, data = data.frame(new_data))
  colnames(new_dummies) <- make.names(colnames(new_dummies))
  new_dummies <- new_dummies[, feature_names, drop = FALSE]
  dnew <- xgb.DMatrix(data = new_dummies)
  pred <- predict(xgb_model, dnew)
  return(ifelse(pred > 0.5, "This could be a good deal!", "This might not be a good deal."))
}

new_data <- data.frame(
  Sale.Price = 200000,
  Acreage = 0.1,
  Property.City = "Antioch",
  YearBuilt = 2000,
  Bedrooms = 3,
  FullBath = 1,
  HalfBath = 1,
  Exterior.Wall = "BRICK.FRAME",
  Foundation.Type = "CRAWL",
  Land.Use = "SINGLE.FAMILY"
)


predict_deal(new_data)

model <- train_model(df)

new_data <- data.frame(
  Sale.Price = 200000,  # Match training data column names
  Total.Value = 200000,
  Property.City = "Antioch",  # Ensure categorical consistency
  Year.Built = 2000,
  Bedrooms = 3,
  Full.Bath = 1,
  Half.Bath = 1
)

# Get probability prediction
pred_prob <- predict(model, new_data, type = "prob")[, 2]

deal_probability <- predict_deal_probability(
    model = model,
    df = df,
    price = 200000,
    value = 200000,
    city = "Antioch",
    year = 2000,
    beds = 3,
    full_bath = 1,
    half_bath = 1
    )
########### debugging
missing_cols <- setdiff(names(df), names(new_data))
for (col in missing_cols) {
  new_data[[col]] <- NA  # Add missing columns as NA
}
new_data <- new_data %>%
  mutate(across(where(is.numeric), as.integer)) %>%
  mutate(across(where(is.logical), as.character))

is.lo
str(new_data)
str(df)
as.integer(new_data)
# Reorder columns to match model input
#new_data <- new_data[, names(df), drop = FALSE]
# Get probability prediction
pred_prob <- predict(model, new_data, type = "prob")[, 2]
return(pred_prob)

##################

deal_probability
str(df %>% select(Sale.Price,Total.Value, Property.City,
                  Year.Built, Bedrooms, Full.Bath, Half.Bath))
str(new_data)
factor(new_data$Property.City, levels = levels(df$Property.City))
paste0(round(deal_probability() * 100, 1), "%")

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


########### Price distribution by city
# Create dynamic plot
output$price_by_city_plot <- renderPlotly({
  plot_data <- df %>%
    group_by(Property.City) %>%
    summarise(Avg_Price = mean(Sale.Price, na.rm = TRUE))
  
  plot <- ggplot(plot_data, aes(x = reorder(Property.City, Avg_Price), y = Avg_Price)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Average Sale Price by City", x = "City", y = "Average Price") +
    theme_minimal()
  
  plotly::ggplotly(plot)
})

######################################


########### Deal success rate (good deal) by city
output$good_deal_rate_plot <- renderPlotly({
  plot_data <- df %>%
    group_by(Property.City) %>%
    summarise(Good_Deal_Rate = mean(GoodDeal, na.rm = TRUE))
  
  plot <- ggplot(plot_data, aes(x = reorder(Property.City, Good_Deal_Rate), y = Good_Deal_Rate)) +
    geom_col(fill = "darkgreen") +
    coord_flip() +
    labs(title = "Good Deal Rate by City", x = "City", y = "Good Deal Rate") +
    theme_minimal()
  
  plotly::ggplotly(plot)
})

##################################################

###### Key metrics
## average price
## average value
## deal rate
# Create summary table for average price, value, and deal rate by city
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


# Create summary table using gt
# output$summary_table <- render_gt({
#   summary_data <- df %>%
#     group_by(Property.City) %>%
#     summarise(
#       Avg_Price = mean(Sale.Price, na.rm = TRUE),
#       Avg_Value = mean(Total.Value, na.rm = TRUE),
#       Good_Deal_Rate = mean(GoodDeal, na.rm = TRUE)
#     )
#   str(summary_data)
#   
#   gt(summary_data) |> # %>%
#     tab_header(
#       title = "Summary of Prices and Deal Rates by City",
#       subtitle = "Average sale price, property value, and good deal rate"
#     )# %>%
#     #fmt_currency(columns = c(Avg_Price, Avg_Value), currency = "USD") #%>%
#     #fmt_number(columns = Good_Deal_Rate, decimals = 2)
# })

###### Property age vs deal rate
output$age_by_deal_rate_plot <- renderPlotly({
  plot <- ggplot(df, aes(x = as.factor(Property.Age), y = GoodDeal)) +
    geom_boxplot(fill = "purple", alpha = 0.6) +
    labs(title = "Property Age vs. Good Deal Rate", x = "Property Age (Years)", y = "Good Deal Rate") +
    theme_minimal()
  
  ggplotly(plot)
  
})

###### Property age by price

output$age_by_price_plot <- renderPlotly({
  plot <- ggplot(df, aes(x = Property.Age, y = Sale.Price)) +
    geom_point(alpha = 0.5, color = "red") +
    labs(title = "Property Age vs. Sale Price", x = "Property Age (years)", y = "Sale Price") +
    theme_minimal()
  
  plotly::ggplotly(plot)
})


install.packages("xfun", type = "source", dependencies = TRUE)

rmarkdown::render(
  "reports/test.Rmd",
  output_format = "pdf_document"
)

