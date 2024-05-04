library(quantmod)
library(caret)
library(TTR)
library(e1071)
library(tidyverse)

base_btc <- c("BTC-USD")
getSymbols(Symbols = base_btc, src = "yahoo", from = "2022-02-24", to = "2024-01-01", auto.assign = TRUE)BTC <- as.data.frame(`BTC-USD`)
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

BTC$SMA <- SMA(BTC$'BTC-USD.Close_1d_back', n = 30)
BTC$MFI <- MFI(HLC = BTC[,c("BTC-USD.High_1d_back", "BTC-USD.Low_1d_back", 'BTC-USD.Close_1d_back')], volume = BTC$'BTC-USD.Volume_1d_back', n = 29)
BTC$RSI <- RSI(BTC$'BTC-USD.Close_1d_back', n = 29)
BB <- BBands(BTC$'BTC-USD.Close_1d_back', n = 30, sd = 2)
BTC$BB_Upper <- as.data.frame(BB)[, "up"]
BTC$BB_Middle <- as.data.frame(BB)[, "mavg"]
BTC$BB_Lower <- as.data.frame(BB)[, "dn"]
HLC <- as.matrix(BTC[,c("BTC-USD.High_1d_back", "BTC-USD.Low_1d_back", 'BTC-USD.Close_1d_back')])
BTC$CCI <- CCI(HLC = HLC, n = 30)
highs <- BTC$`BTC-USD.High_1d_back`
lows <- BTC$`BTC-USD.Low_1d_back`
closes <- BTC$`BTC-USD.Close_1d_back`
donchian <- DonchianChannel(H = highs, n = 30)
BTC$Donchian_High <- as.data.frame(donchian)[, "high"]
BTC$Donchian_Low <- as.data.frame(donchian)[, "low"]

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
for (i in 1:30) {
  tech_vars <- c(paste0("`BTC-USD.Close_", 1:i, "d_back`"), paste0("`BTC-USD.Volume_", 1:i, "d_back`"), paste0("`BTC-USD.High_", 1:i, "d_back`"), paste0("`BTC-USD.Low_", 1:i, "d_back`"), paste0("`BTC-USD.Open_", 1:i, "d_back`"), "SMA", "MFI", "RSI", "BB_Upper", "BB_Middle", "BB_Lower", "CCI", "Donchian_High", "Donchian_Low")
  formula_vars <- paste(c(tech_vars), collapse = " + ")
  formula <- as.formula(paste("Target ~", formula_vars))
  
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
# Najlepszy model ma opóźnienie 1 dni z dokładnością 0.6142857 na zbiorze treningowym.