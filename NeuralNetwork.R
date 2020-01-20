install.packages("neuralnet")
install.packages("boot")
install.packages("plyr")

library(boot)
library(plyr)
library(neuralnet)
library(gmodels)

rm(list = ls())

# 1. Load data
data <- read.csv("QCM/QCM12.csv", sep = ";")
names(data) <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "T1", "T2", "T3", "T4", "T5")

# 2. Data exploration
str(data)
head(data)
summary(data)

# 3. Transformation and clean up
normalize <- function(x) {
  return ((x-min(x)) / (max(x)-min(x)))
}

data_norm <- as.data.frame(lapply(data, normalize))

# all data is normalised from 0 to 1
head(data_norm)

n <- 100
outs <- NULL

pbar <- create_progress_bar('text')
pbar$init(n)

for(i in 1:n){
  # 4. Data splitting
  set.seed(i)
  data_rows <- nrow(data_norm)
  data_sample <- sample(data_rows, data_rows * 0.6)
  data_train <- data_norm[data_sample,]
  data_test <- data_norm[-data_sample,]

  # 5. Model creation (training data)
  data_model <- neuralnet(T1+T2+T3+T4+T5~P1+P2+P3+P4+P5+P6+P7+P8+P9+P10, data = data_train, hidden = c(30, 10))

  # 6. Prediction (test data)
  model_results <- compute(data_model, data_test[1:10])

  # 7. Model Evaluation
  predicted_alcohol <- model_results$net.result

  original_values <- max.col(data_test[, 11:15])
  predicted_values <- max.col(predicted_alcohol)

  outs[i] <- mean(predicted_values == original_values)

  pbar$step()

  # CrossTable(original_values, predicted_values, prop.chisq = FALSE, prop.r = FALSE, prop.c=FALSE, dnn = c("actual","predicted"))
}

# average accuracy
mean(outs)
