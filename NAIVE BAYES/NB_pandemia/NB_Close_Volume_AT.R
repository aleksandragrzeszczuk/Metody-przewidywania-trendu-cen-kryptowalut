library(quantmod)
library(caret)
library(TTR)
library(glmnet)
library(tidyverse)
library(e1071)

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
BTC <- add_lagged_features(BTC, "BTC-USD.High", 1)
BTC <- add_lagged_features(BTC, "BTC-USD.Low", 1)

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
  tech_vars <- c(paste0("`BTC-USD.Close_", 1:i, "d_back`"), paste0("`BTC-USD.Volume_", 1:i, "d_back`"), "SMA", "MFI", "RSI", "BB_Upper", "BB_Middle", "BB_Lower", "CCI", "Donchian_High", "Donchian_Low")
  formula_vars <- paste(c(tech_vars), collapse = " + ")
  formula <- as.formula(paste("Target ~", formula_vars))
  
  model_nb <- naiveBayes(formula, data = train_data, na.action = na.pass)
  
  # Predykcje na zbiorze treningowym
  predict_train <- predict(model_nb, newdata = train_data, type = "class")
  confusion_train <- confusionMatrix(as.factor(predict_train), as.factor(y_train))
  train_accuracy[i] <- confusion_train$overall['Accuracy']
  
  # Predykcje na zbiorze testowym
  predict_test <- predict(model_nb, newdata = test_data, type = "class")
  confusion_test <- confusionMatrix(as.factor(predict_test), as.factor(y_test))
  test_accuracy[i] <- confusion_test$overall['Accuracy']
  
  cat("Model z opóźnieniem", i, "dni:\n")
  cat("Accuracy - Train:", confusion_train$overall['Accuracy'], "\n")
  cat("Accuracy - Test:", confusion_test$overall['Accuracy'], "\n\n")
}

# Wybór najlepszego modelu na podstawie najwyższej dokładności
best_model_index <- which.max(train_accuracy)
cat("Najlepszy model ma opóźnienie", best_model_index, "dni z dokładnością", train_accuracy[best_model_index], "na zbiorze treningowym.\n")

# Wybór najlepszego modelu na podstawie najwyższej dokładności ZBIORU TESTOWEGO
best_model_index_test <- which.max(test_accuracy)
cat("Najlepszy model na zbiorze testowym ma opóźnienie", best_model_index_test, "dni z dokładnością", test_accuracy[best_model_index_test], ".\n")

# Metryki na zbiorze treningowym
TPR_train <- confusion_train$byClass['Sensitivity']
FPR_train <- 1 - confusion_train$byClass['Specificity']
TNR_train <- confusion_train$byClass['Specificity']
PPV_train <- confusion_train$byClass['Pos Pred Value']
NPV_train <- confusion_train$byClass['Neg Pred Value']

cat("TPR (Train):", TPR_train, "\n")
cat("FPR (Train):", FPR_train, "\n")
cat("TNR (Train):", TNR_train, "\n")
cat("PPV (Train):", PPV_train, "\n")
cat("NPV (Train):", NPV_train, "\n")