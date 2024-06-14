library(quantmod)
library(caret)
library(TTR)
library(glmnet)
library(tidyverse)

base_btc <- c("BTC-USD")
getSymbols(Symbols = base_btc, src = "yahoo", from = "2018-01-01", to = "2019-12-31", auto.assign = TRUE)
BTC <- as.data.frame(`BTC-USD`)

BTC$Target_ <- ifelse(Delt(BTC$'BTC-USD.Close') > 0, 1, 0)
sum(BTC$Target == 1, na.rm = TRUE)

#############################################
# OKRES PRZED PANDEMIĄ 01.01.2018 - 31.12.2019
#############################################

# 377/730 = 0.5164
y_pred_prob <- numeric(10000) 
for (k in 1:10000){
  y_pred_sample <- rbinom(n = 730, size = 1, prob = 0.5164384)
  y_pred_prob[k] <- sum(y_pred_sample) / 730
}
mean(y_pred_prob) # 0.5163

y_true <- BTC$Target
y_pred <- rbinom(n = length(y_true), size = 1, prob = 0.5164384)
conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(y_true))
# print(conf_matrix)

ACC <- conf_matrix$overall['Accuracy']
TPR <- conf_matrix$byClass['Sensitivity']
FPR <- 1 - conf_matrix$byClass['Specificity']
TNR <- conf_matrix$byClass['Specificity']
PPV <- conf_matrix$byClass['Pos Pred Value']
NPV <- conf_matrix$byClass['Neg Pred Value']

cat("ACC:", ACC, "\n") # 0.5240
cat("TPR:", TPR, "\n") # 0.5341
cat("FPR:", FPR, "\n") # 0.4854
cat("TNR:", TNR, "\n") # 0.5146
cat("PPV:", PPV, "\n") # 0.5067
cat("NPV:", NPV, "\n") # 0.5419

#############################################
# OKRES PANDEMI 01.01.2020 - 23.02.2022
#############################################

# 418/785 = 0.5325
y_pred_prob <- numeric(10000) 
for (k in 1:10000){
  y_pred_sample <- rbinom(n = 785, size = 1, prob = 0.5324841)
  y_pred_prob[k] <- sum(y_pred_sample) / 785
}
mean(y_pred_prob) # 0.5321

y_true <- BTC$Target
y_pred <- rbinom(n = length(y_true), size = 1, prob = 0.5324841)
conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(y_true))
# print(conf_matrix)

ACC <- conf_matrix$byClass['Accuracy']
TPR <- conf_matrix$byClass['Sensitivity']
FPR <- 1 - conf_matrix$byClass['Specificity']
TNR <- conf_matrix$byClass['Specificity']
PPV <- conf_matrix$byClass['Pos Pred Value']
NPV <- conf_matrix$byClass['Neg Pred Value']

cat("ACC:", ACC, "\n") # 0.5077  
cat("TPR:", TPR, "\n") # 0.4645
cat("FPR:", FPR, "\n") # 0.4545
cat("TNR:", TNR, "\n") # 0.5455
cat("PPV:", PPV, "\n") # 0.4722
cat("NPV:", NPV, "\n") # 0.5377

#############################################
# OKRES WOJNY 24.02.2022 - 01.01.2024
#############################################

# 326/677 = 0.4815
y_pred_prob <- numeric(10000) 
for (k in 1:10000){
  y_pred_sample <- rbinom(n = 677, size = 1, prob = 0.4815362)
  y_pred_prob[k] <- sum(y_pred_sample) / 677
}
mean(y_pred_prob) # 0.4819

y_true <- BTC$Target
y_pred <- rbinom(n = length(y_true), size = 1, prob = 0.4815362)
conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(y_true))
# print(conf_matrix)

ACC <- conf_matrix$byClass['Accuracy']
TPR <- conf_matrix$byClass['Sensitivity']
FPR <- 1 - conf_matrix$byClass['Specificity']
TNR <- conf_matrix$byClass['Specificity']
PPV <- conf_matrix$byClass['Pos Pred Value']
NPV <- conf_matrix$byClass['Neg Pred Value']

cat("ACC:", ACC, "\n") # 0.5207  
cat("TPR:", TPR, "\n") # 0.5286
cat("FPR:", FPR, "\n") # 0.4877
cat("TNR:", TNR, "\n") # 0.5123
cat("PPV:", PPV, "\n") # 0.5378
cat("NPV:", NPV, "\n") # 0.5030
