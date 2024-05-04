library(quantmod)
library(caret)
library(TTR)
library(glmnet)
library(tidyverse)

base_btc <- c("BTC-USD")
getSymbols(Symbols = base_btc, src = "yahoo", from = "2022-02-24", to = "2024-01-01", auto.assign = TRUE)
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

# view(BTC)

BTC <- BTC[-(1:30),]

# view(BTC)

set.seed(255707) 
m <- floor(0.8 * nrow(BTC))
train_data <- BTC[1:m, ]
test_data <- BTC[(m+1):nrow(BTC), ]

x_train <- model.matrix(~ . -1 - Target, data = train_data)
y_train <- train_data$Target
x_test <- model.matrix(~ . -1 - Target, data = test_data)
y_test <- test_data$Target

train_accuracy <- numeric(30)
test_accuracy <- numeric(30)
for (i in 1:30) {
  close_vars <- paste(paste0("`BTC-USD.Close_", 1:i, "d_back`"), collapse = " + ")
  
  formula <- as.formula(paste("Target ~", close_vars))
  
  model <- glm(formula, data = train_data, family = binomial)
  
  # Predykcje dla zbioru treningowego
  predict_train <- predict(model, newdata = train_data, type = "response")
  class_train <- ifelse(predict_train > 0.5, 1, 0)
  confusion_train <- confusionMatrix(as.factor(class_train), as.factor(train_data$Target))
  train_accuracy[i] <- confusion_train$overall['Accuracy']
  
  # Preykcje dla zbioru testowego
  predict_test <- predict(model, newdata = test_data, type = "response")
  class_test <- ifelse(predict_test > 0.5, 1, 0)
  confusion_test <- confusionMatrix(as.factor(class_test), as.factor(test_data$Target))
  test_accuracy[i] <- confusion_test$overall['Accuracy']

  cat("Model z opóźnieniem", i, "dni:\n")
  cat("Accuracy - Train:", confusion_train$overall['Accuracy'], "\n")
  cat("Accuracy - Test:", confusion_test$overall['Accuracy'], "\n\n")
}

confusion_train

# Wybór najlepszego modelu na podstawie najwyższej dokładności ZBIORU TRENINGOWEGO
best_model_index <- which.max(train_accuracy)
cat("Najlepszy model ma opóźnienie", best_model_index, "dni z dokładnością", train_accuracy[best_model_index], "na zbiorze treningowym.\n")
# Najlepszy model ma opóźnienie 30 dni z dokładnością 0.6131528 na zbiorze treningowym.

# Wybór najlepszego modelu na podstawie najwyższej dokładności ZBIORU TESTOWEGO
# best_model_index_test <- which.max(test_accuracy)
# cat("Najlepszy model na zbiorze testowym ma opóźnienie", best_model_index_test, "dni z dokładnością", test_accuracy[best_model_index_test], ".\n")
# Najlepszy model na zbiorze testowym ma opóźnienie 27 dni z dokładnością 0.5384615