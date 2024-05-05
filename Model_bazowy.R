library(quantmod)
library(caret)
library(TTR)
library(glmnet)
library(tidyverse)

base_btc <- c("BTC-USD")
getSymbols(Symbols = base_btc, src = "yahoo", from = "2012-01-01", to = "2017-12-31", auto.assign = TRUE)
BTC <- as.data.frame(`BTC-USD`)

BTC$Target_ <- ifelse(Delt(BTC$'BTC-USD.Close') > 0, 1, 0)
sum(BTC$Target == 1, na.rm = TRUE)

# 647/1202 = 0.5382696 -> Czyli prawdopodobieństwo wzrostu ceny wynosi 54%

y_pred_prob <- numeric(1000) 
for (k in 1:1000){
  y_pred_sample <- rbinom(n = 1202, size = 1, prob = 0.54)
  y_pred_prob[k] <- sum(y_pred_sample) / 1202
}
mean(y_pred_prob) # 0.5402438

y_pred_prob <- numeric(1000) 
for (k in 1:1000){
  y_pred_sample <- rbinom(n = 1202, size = 1, prob = 0.5)
  y_pred_prob[k] <- sum(y_pred_sample) / 1202
}
mean(y_pred_prob)
