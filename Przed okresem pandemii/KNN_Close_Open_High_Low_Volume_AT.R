library(quantmod)
library(caret)
library(TTR)
library(glmnet)
library(tidyverse)

base_btc <- c("BTC-USD")
getSymbols(Symbols = base_btc, src = "yahoo", from = "2018-01-01", to = "2019-12-31", auto.assign = TRUE)
BTC <- as.data.frame(`BTC-USD`)

BTC$Target <- ifelse(Delt(BTC$'BTC-USD.Close') > 0, 1, 0)
BTC$Target <- factor(BTC$Target, levels = c(0, 1)) 

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

# SMA
BTC$SMA <- SMA(BTC$'BTC-USD.Close_1d_back', n = 30)
# MFI
BTC$MFI <- MFI(HLC = BTC[,c("BTC-USD.High_1d_back", "BTC-USD.Low_1d_back", 'BTC-USD.Close_1d_back')], volume = BTC$'BTC-USD.Volume_1d_back', n = 29)
# RSI
BTC$RSI <- RSI(BTC$'BTC-USD.Close_1d_back', n = 29)
# WSTĘGA BOLLINGERA
BB <- BBands(BTC$'BTC-USD.Close_1d_back', n = 30, sd = 2)
BTC$BB_Upper <- as.data.frame(BB)[, "up"]
BTC$BB_Middle <- as.data.frame(BB)[, "mavg"]
BTC$BB_Lower <- as.data.frame(BB)[, "dn"]
# CCI
HLC <- as.matrix(BTC[,c("BTC-USD.High_1d_back", "BTC-USD.Low_1d_back", 'BTC-USD.Close_1d_back')])
BTC$CCI <- CCI(HLC = HLC, n = 30)
# KANAŁ DONCHIANA
highs <- BTC$`BTC-USD.High_1d_back`
lows <- BTC$`BTC-USD.Low_1d_back`
closes <- BTC$`BTC-USD.Close_1d_back`
donchian <- DonchianChannel(H = highs, n = 30)
BTC$Donchian_High <- as.data.frame(donchian)[, "high"]
BTC$Donchian_Low <- as.data.frame(donchian)[, "low"]

BTC <- BTC[-(1:30),]

# view(BTC)

set.seed(255707)
m <- floor(0.8 * nrow(BTC))
train_data <- BTC[1:m, ]
test_data <- BTC[(m+1):nrow(BTC), ]

k <- 3  
model_knn <- train(Target ~ ., data = train_data, method = "knn", preProcess = "scale", tuneGrid = data.frame(k = k), trControl = trainControl(method = "none"))

# Predykcje dla zbioru treningowego i testowego
predictions_train <- predict(model_knn, newdata = train_data)
predictions_test <- predict(model_knn, newdata = test_data)

# Obliczanie macierzy pomyłek
confusion_train <- confusionMatrix(predictions_train, train_data$Target)
confusion_test <- confusionMatrix(predictions_test, test_data$Target)

# Metryki na zbiorze treningowym
# print(confusion_train)
TPR_train <- confusion_train$byClass['Sensitivity']
FPR_train <- 1 - confusion_train$byClass['Specificity']
TNR_train <- confusion_train$byClass['Specificity']
PPV_train <- confusion_train$byClass['Pos Pred Value']
NPV_train <- confusion_train$byClass['Neg Pred Value']
cat("ACC (Train):", confusion_train$overall['Accuracy'], "\n")
cat("TPR (Train):", TPR_train, "\n")
cat("FPR (Train):", FPR_train, "\n")
cat("TNR (Train):", TNR_train, "\n")
cat("PPV (Train):", PPV_train, "\n")
cat("NPV (Train):", NPV_train, "\n")

# Metryki na zbiorze testowym
# print(confusion_test)
TPR_test <- confusion_test$byClass['Sensitivity']
FPR_test <- 1 - confusion_test$byClass['Specificity']
TNR_test <- confusion_test$byClass['Specificity']
PPV_test <- confusion_test$byClass['Pos Pred Value']
NPV_test <- confusion_test$byClass['Neg Pred Value']
cat("ACC (Test):", confusion_test$overall['Accuracy'], "\n")
cat("TPR (Test):", TPR_test, "\n")
cat("FPR (Test):", FPR_test, "\n")
cat("TNR (Test):", TNR_test, "\n")
cat("PPV (Test):", PPV_test, "\n")
cat("NPV (Test):", NPV_test, "\n")
