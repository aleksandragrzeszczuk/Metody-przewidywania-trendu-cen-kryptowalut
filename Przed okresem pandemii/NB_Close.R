library(quantmod)
library(caret)
library(TTR)
library(glmnet)
library(tidyverse)
library(e1071)

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
models <- vector("list", 30) 

for (i in 1:30) {
  close_vars <- paste(paste0("`BTC-USD.Close_", 1:i, "d_back`"), collapse = " + ")
  
  formula <- as.formula(paste("Target ~", close_vars))
  
  model_nb <- naiveBayes(formula, data = train_data, na.action = na.pass)
  models[[i]] <- model_nb  
  
  # Predykcje na zbiorze treningowym
  predict_train <- predict(model_nb, newdata = train_data, type = "class")
  confusion_train <- confusionMatrix(as.factor(predict_train), as.factor(y_train))
  train_accuracy[i] <- confusion_train$overall['Accuracy']
  
  # Predykcje na zbiorze testowym
  predict_test <- predict(model_nb, newdata = test_data, type = "class")
  confusion_test <- confusionMatrix(as.factor(predict_test), as.factor(y_test))
  test_accuracy[i] <- confusion_test$overall['Accuracy']
}

# Wybór najlepszego modelu
best_model_index_train <- which.max(train_accuracy)
best_model_train <- models[[best_model_index_train]]
predict_train_best <- predict(best_model_train, newdata = train_data, type = "class")
confusion_train_best <- confusionMatrix(as.factor(predict_train_best), as.factor(train_data$Target))

best_model_index_test <- which.max(test_accuracy)
best_model_test <- models[[best_model_index_test]]
predict_test_best <- predict(best_model_test, newdata = test_data, type = "class")
confusion_test_best <- confusionMatrix(as.factor(predict_test_best), as.factor(test_data$Target))

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
cat("ACC (Test):", confusion_test_best$overall['Accuracy'], "\n")
cat("TPR (Test):", TPR_test, "\n")
cat("FPR (Test):", FPR_test, "\n")
cat("TNR (Test):", TNR_test, "\n")
cat("PPV (Test):", PPV_test, "\n")
cat("NPV (Test):", NPV_test, "\n")
