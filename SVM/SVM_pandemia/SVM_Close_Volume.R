library(quantmod)
library(caret)
library(TTR)
library(e1071)
library(tidyverse)

base_btc <- c("BTC-USD")
getSymbols(Symbols = base_btc, src = "yahoo", from = "2020-01-01", to = "2022-02-23", auto.assign = TRUE)
BTC <- as.data.frame(`BTC-USD`)

BTC$Target <- ifelse(Delt(BTC$'BTC-USD.Close') > 0, 1, 0)

add_lagged_features <- function(data, column_name, n_days) {
  for (i in 1:n_days) {
    lagged_column_name <- paste0(column_name, '_', i, 'd_back')
    data <- data %>%
      mutate("{lagged_column_name}" := lag(!!sym(column_name), n = i))
  }
  return(data)
}

BTC <- add_lagged_features(BTC, "BTC-USD.Close", 30)
BTC <- add_lagged_features(BTC, "BTC-USD.Volume", 30)

BTC <- BTC[-(1:30),]

set.seed(255707)
m <- floor(0.8 * nrow(BTC))
train_data <- BTC[1:m, ]
test_data <- BTC[(m+1):nrow(BTC), ]

x_train <- train_data %>% select(-Target)
y_train <- train_data$Target
x_test <- test_data %>% select(-Target)
y_test <- test_data$Target

train_accuracy <- numeric(30)
test_accuracy <- numeric(30)
for (i in 1:30) {
  close_vars <- paste(paste0("`BTC-USD.Close_", 1:i, "d_back`"), collapse = " + ")
  volume_vars <- paste(paste0("`BTC-USD.Volume_", 1:i, "d_back`"), collapse = " + ")
  
  formula <- as.formula(paste("Target ~", close_vars, "+", volume_vars))
  
  model <- svm(formula, data = train_data, kernel = "radial", type = "C-classification")
  
  # Predykcje dla zbioru treningowego
  predict_train <- predict(model, newdata = train_data)
  confusion_train <- confusionMatrix(as.factor(predict_train), as.factor(train_data$Target))
  train_accuracy[i] <- confusion_train$overall['Accuracy']
  
  # Predykcje dla zbioru testowego
  predict_test <- predict(model, newdata = test_data)
  confusion_test <- confusionMatrix(as.factor(predict_test), as.factor(test_data$Target))
  test_accuracy[i] <- confusion_test$overall['Accuracy']
  
  cat("Model z opóźnieniem", i, "dni:\n")
  cat("Accuracy - Train:", confusion_train$overall['Accuracy'], "\n")
  cat("Accuracy - Test:", confusion_test$overall['Accuracy'], "\n\n")
}

# Wybór najlepszego modelu na podstawie najwyższej dokładności
best_model_index <- which.max(train_accuracy)
cat("Najlepszy model ma opóźnienie", best_model_index, "dni z dokładnością", train_accuracy[best_model_index], "na zbiorze treningowym.\n")
# Najlepszy model ma opóźnienie 30 dni z dokładnością 0.6107143 na zbiorze treningowym.