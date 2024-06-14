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

BTC <- BTC[-(1:30),]

set.seed(255707)
m <- floor(0.8 * nrow(BTC))
train_data <- BTC[1:m, ]
test_data <- BTC[(m+1):nrow(BTC), ]

x_train <- model.matrix(~ . -1 - Target, data = train_data)
y_train <- train_data$Target
x_test <- model.matrix(~ . -1 - Target, data = test_data)
y_test <- test_data$Target

# Ridge Regression
set.seed(255707)
cv.ridge <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 0, nfolds = 10)
best_lambda <- cv.ridge$lambda.min
ridge_model <- glmnet(x_train, y_train, family = "binomial", alpha = 0, lambda = best_lambda)

# Predict on training set
predict_train_ridge <- predict(ridge_model, s = best_lambda, newx = x_train, type = "response")
class_train_ridge <- ifelse(predict_train_ridge > 0.5, 1, 0)
confusion_train_ridge <- confusionMatrix(as.factor(class_train_ridge), as.factor(y_train))

# Predict on test set
predict_test_ridge <- predict(ridge_model, s = best_lambda, newx = x_test, type = "response")
class_test_ridge <- ifelse(predict_test_ridge > 0.5, 1, 0)
confusion_test_ridge <- confusionMatrix(as.factor(class_test_ridge), as.factor(y_test))

# Metrics on training set
TPR_train_ridge <- confusion_train_ridge$byClass['Sensitivity']
FPR_train_ridge <- 1 - confusion_train_ridge$byClass['Specificity']
TNR_train_ridge <- confusion_train_ridge$byClass['Specificity']
PPV_train_ridge <- confusion_train_ridge$byClass['Pos Pred Value']
NPV_train_ridge <- confusion_train_ridge$byClass['Neg Pred Value']
cat("ACC (Train):", confusion_train_ridge$overall['Accuracy'], "\n")
cat("TPR (Train):", TPR_train_ridge, "\n")
cat("FPR (Train):", FPR_train_ridge, "\n")
cat("TNR (Train):", TNR_train_ridge, "\n")
cat("PPV (Train):", PPV_train_ridge, "\n")
cat("NPV (Train):", NPV_train_ridge, "\n")

# Metrics on test set
TPR_test_ridge <- confusion_test_ridge$byClass['Sensitivity']
FPR_test_ridge <- 1 - confusion_test_ridge$byClass['Specificity']
TNR_test_ridge <- confusion_test_ridge$byClass['Specificity']
PPV_test_ridge <- confusion_test_ridge$byClass['Pos Pred Value']
NPV_test_ridge <- confusion_test_ridge$byClass['Neg Pred Value']
cat("ACC (Test):", confusion_test_ridge$overall['Accuracy'], "\n")
cat("TPR (Test):", TPR_test_ridge, "\n")
cat("FPR (Test):", FPR_test_ridge, "\n")
cat("TNR (Test):", TNR_test_ridge, "\n")
cat("PPV (Test):", PPV_test_ridge, "\n")
cat("NPV (Test):", NPV_test_ridge, "\n")
