# Function to load and preprocess data
load_and_preprocess_data <- function(file_path) {
  df <- read.csv(file_path)
  # Remove rows with missing values
  df <- df %>% drop_na()
  df <- df %>% filter(!is.na(Sale.Price), !is.na(Total.Value))
  df$Property.Age <- 2016 - df$Year.Built
  df$Age_built <- cut(df$Property.Age, breaks = seq(0, max(df$Property.Age, na.rm = TRUE), by = 25), include.lowest = TRUE, right = FALSE)
  # Creates indicator for a 'good' deal as the sale price being the 90% of the total value
  df$GoodDeal <- ifelse(df$Sale.Price < df$Total.Value * 0.9, 1, 0)
  df <- df %>% select(GoodDeal, Sale.Price, Total.Value, Property.City, Year.Built,
                      Bedrooms, Full.Bath, Half.Bath,Property.Age, Age_built)
  return(df)
}

# Function to train the model
train_model <- function(data) {
  # prepped_data <- data %>% select(GoodDeal, Sale.Price, Total.Value, Property.City, Year.Built,
  #                              Bedrooms, Full.Bath, Half.Bath)#,
                              #Acreage, Exterior.Wall,Foundation.Type,
                              #Land.Use)
  # dummies <- model.matrix(~ . -1, data = features)
  # colnames(dummies) <- make.names(colnames(dummies))
  # dtrain <- xgb.DMatrix(data = dummies, label = data$GoodDeal)
  # params <- list(objective = "binary:logistic", eval_metric = "logloss")
  randomForest(
    factor(GoodDeal) ~ Sale.Price + Total.Value + Property.City + Year.Built + Bedrooms +
      Full.Bath + Half.Bath,
    data = data,
    ntree = 100
  )
  #return(list(model = model, feature_names = colnames(features)))
  #return(model)
}
# Prediction function using the trained model
predict_deal_probability <- function(model,df, price, value, city, year, beds, full_bath, half_bath) {
  # Create a data frame with the input parameters
  new_data <- data.frame(
    Sale.Price = price,
    Total.Value = value,
    Property.City = city,
    Year.Built = year,
    Bedrooms  = beds,
    Full.Bath = full_bath,
    Half.Bath = half_bath
  )
  # Get probability prediction
  pred_prob <- predict(model, new_data, type = "prob")[, 2]
  return(pred_prob)
}

