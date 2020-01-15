install.packages("neuralnet")
install.packages("boot")
install.packages("plyr")


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

# 4. Data splitting
data_rows <- nrow(data_norm)
data_sample <- sample(data_rows, data_rows * 0.8)
data_train <- data_norm[data_sample,]
data_test <- data_norm[-data_sample,]

# 5. Model creation (training data)
library(neuralnet)

# train the model
data_model <- neuralnet(T1+T2+T3+T4+T5~P1+P2+P3+P4+P5+P6+P7+P8+P9+P10, data = data_train, hidden = c(30, 10))

# plot(data_model)

# 6. Prediction (test data)
model_results <- compute(data_model, data_test[1:10])

# 7. Model Evaluation
predicted_alcohol <- model_results$net.result

original_values <- max.col(data_test[, 11:15])
predicted_values <- max.col(predicted_alcohol)
mean(predicted_values == original_values)

library(gmodels)
CrossTable(original_values, predicted_values, prop.chisq = FALSE,
           prop.r = FALSE, prop.c=FALSE, dnn = c("actual","predicted"))

# K fold cross validation

library(boot)
library(plyr)

k <- 5
outs <- NULL

pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  sample <- sample(1:nrow(data_norm),round(0.8*nrow(data_norm)))
  train <- data_norm[sample,]
  test <- data_norm[-sample,]

  data_model <- neuralnet(T1+T2+T3+T4+T5~P1+P2+P3+P4+P5+P6+P7+P8+P9+P10, data = train, hidden = c(30, 10))

  predict <- compute(data_model, test[,1:10])

  predict_results <- predict$net.result
  original_values <- max.col(test[, 11:15])
  predict_results <- max.col(predict_results)
  outs[i] <- mean(predict_results == original_values)

  pbar$step()
}

mean(outs)
