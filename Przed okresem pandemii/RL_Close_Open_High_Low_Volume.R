library(quantmod)
library(caret)
library(TTR)
library(glmnet)
library(tidyverse)

base_btc <- c("BTC-USD")
getSymbols(Symbols = base_btc, src = "yahoo", from = "2018-01-01", to = "2019-12-31", auto.assign = TRUE)
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
BTC <- add_lagged_features(BTC, "BTC-USD.Open", 30)
BTC <- add_lagged_features(BTC, "BTC-USD.High", 30)
BTC <- add_lagged_features(BTC, "BTC-USD.Low", 30)
BTC <- add_lagged_features(BTC, "BTC-USD.Volume", 30)

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
models <- list()

for (i in 1:30) {
  close_vars <- paste(paste0("`BTC-USD.Close_", 1:i, "d_back`"), collapse = " + ")
  open_vars <- paste(paste0("`BTC-USD.Open_", 1:i, "d_back`"), collapse = " + ")
  high_vars <- paste(paste0("`BTC-USD.High_", 1:i, "d_back`"), collapse = " + ")
  low_vars <- paste(paste0("`BTC-USD.Low_", 1:i, "d_back`"), collapse = " + ")
  volume_vars <- paste(paste0("`BTC-USD.Volume_", 1:i, "d_back`"), collapse = " + ")
  
  formula <- as.formula(paste("Target ~", close_vars, "+", open_vars, "+", high_vars, "+", low_vars, "+", volume_vars))
  
  model <- glm(formula, data = train_data, family = binomial)
  models[[i]] <- model
  
  # Predykcje dla zbioru treningowego
  predict_train <- predict(model, newdata = train_data, type = "response")
  class_train <- ifelse(predict_train > 0.5, 1, 0)
  confusion_train <- confusionMatrix(as.factor(class_train), as.factor(train_data$Target))
  train_accuracy[i] <- confusion_train$overall['Accuracy']
  
  # Predykcje dla zbioru testowego
  predict_test <- predict(model, newdata = test_data, type = "response")
  class_test <- ifelse(predict_test > 0.5, 1, 0)
  confusion_test <- confusionMatrix(as.factor(class_test), as.factor(test_data$Target))
  test_accuracy[i] <- confusion_test$overall['Accuracy']
}

# Wybór najlepszego modelu na podstawie najwyższej dokładności ZBIORU TRENINGOWEGO
best_model_index <- which.max(train_accuracy)
best_model_train <- models[[best_model_index]]

# Wybór najlepszego modelu na podstawie najwyższej dokładności ZBIORU TESTOWEGO
best_model_index_test <- which.max(test_accuracy)
best_model_test <- models[[best_model_index_test]]

# Przewidywanie i klasyfikacja na zbiorze treningowym
predict_train_best <- predict(best_model_train, newdata = train_data, type = "response")
class_train_best <- ifelse(predict_train_best > 0.5, 1, 0)

# Przewidywanie i klasyfikacja na zbiorze testowym
predict_test_best <- predict(best_model_test, newdata = test_data, type = "response")
class_test_best <- ifelse(predict_test_best > 0.5, 1, 0)

class_train_best_factor <- factor(class_train_best, levels = c(0, 1))
class_test_best_factor <- factor(class_test_best, levels = c(0, 1))
target_train_factor <- factor(train_data$Target, levels = c(0, 1))
target_test_factor <- factor(test_data$Target, levels = c(0, 1))

# Obliczenie macierzy pomyłek dla najlepszego modelu treningowego i testowego
confusion_train_best <- confusionMatrix(class_train_best_factor, target_train_factor)
confusion_test_best <- confusionMatrix(class_test_best_factor, target_test_factor)

# Metryki na zbiorze treningowym
# print(confusion_train_best)
TPR_train <- confusion_train_best$byClass['Sensitivity']
FPR_train <- 1 - confusion_train_best$byClass['Specificity']
TNR_train <- confusion_train_best$byClass['Specificity']
PPV_train <- confusion_train_best$byClass['Pos Pred Value']
NPV_train <- confusion_train_best$byClass['Neg Pred Value']
cat("ACC (Train):", confusion_train_best$overall['Accuracy'], "\n")
cat("TPR (Train):", TPR_train, "\n")
cat("FPR (Train):", FPR_train, "\n")
cat("TNR (Train):", TNR_train, "\n")
cat("PPV (Train):", PPV_train, "\n")
cat("NPV (Train):", NPV_train, "\n")

# Metryki na zbiorze testowym
# print(confusion_test_best)
TPR_test <- confusion_test_best$byClass['Sensitivity']
FPR_test <- 1 - confusion_test_best$byClass['Specificity']
TNR_test <- confusion_test_best$byClass['Specificity']
PPV_test <- confusion_test_best$byClass['Pos Pred Value']
NPV_test <- confusion_test_best$byClass['Neg Pred Value']
cat("ACC (Test)):", confusion_test_best$overall['Accuracy'], "\n")
cat("TPR (Test):", TPR_test, "\n")
cat("FPR (Test):", FPR_test, "\n")
cat("TNR (Test):", TNR_test, "\n")
cat("PPV (Test):", PPV_test, "\n")
cat("NPV (Test):", NPV_test, "\n")