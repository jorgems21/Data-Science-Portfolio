# Function to load and preprocess data
load_and_preprocess_data <- function(file_path) {
  df <- read.csv(file_path)
  # Remove rows with missing values
  df <- df %>% drop_na()
  df <- df %>% filter(!is.na(Sale.Price), !is.na(Total.Value))
  # Creates indicator for a 'good' deal as the sale price being the 90% of the total value
  df$GoodDeal <- ifelse(df$Sale.Price < df$Total.Value * 0.9, 1, 0)
  return(df)
}

# Function to train the model
train_model <- function(data) {
  features <- data %>% select(Property.City, Year.Built, Bedrooms, Full.Bath, Half.Bath)
  dummies <- model.matrix(~ . -1, data = features)
  colnames(dummies) <- make.names(colnames(dummies))
  dtrain <- xgb.DMatrix(data = dummies, label = data$GoodDeal)
  params <- list(objective = "binary:logistic", eval_metric = "logloss")
  model <- xgb.train(params = params, data = dtrain, nrounds = 100)
  return(list(model = model, feature_names = colnames(dummies)))
}