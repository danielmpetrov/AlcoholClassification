install.packages("C50")
install.packages("plyr")

library(C50)
library(plyr)
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
  if (x[11] == 1) {
    "P1"
  } else if (x[12] == 1) {
    "P2"
  } else if (x[13] == 1) {
    "P3"
  } else if (x[14] == 1) {
    "P4"
  } else {
    "P5"
  }
}

# convert dummies to categories
Cat = apply(data, 1, normalize)
data_norm <- data.frame(data[1:10], Cat)

# dummies are converted to factors
head(data_norm)

n <- 1000
outs <- NULL

pbar <- create_progress_bar('text')
pbar$init(n)

for(i in 1:n){
  # 4. Data splitting
  rows <- nrow(data_norm)
  sample <- sample(rows, rows * 0.5)
  train <- data_norm[sample,]
  test <- data_norm[-sample,]

  # check that ratio of categorisation from one data set
  # to another is valid and all categories are represented
  # prop.table(table(train$Cat))
  # prop.table(table(test$Cat))

  # 5. Model creation (training data)
  model <- C5.0(train[-11], train$Cat)
  # model
  # summary(model)

  # 6. Prediction (test data)
  predict <- predict(model, test)

  # 7. Model Evaluation
  # CrossTable(test$Cat, predict, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual', 'predicted'))
  outs <- mean(predict == test$Cat)

  pbar$step()
}

# average accuracy
mean(outs)

n <- 1000
outs <- NULL

pbar <- create_progress_bar('text')
pbar$init(n)

for(i in 1:n){
  # 4. Data splitting
  rows <- nrow(data_norm)
  sample <- sample(rows, rows * 0.5)
  train <- data_norm[sample,]
  test <- data_norm[-sample,]

  # 5. Model creation (training data)
  # boost for extra performance
  boost <- C5.0(train[-11], train$Cat, trials = 10)
  # boost
  # summary(boost)

  # 6. Prediction (test data)
  boost.predict <- predict(boost, test)

  # 7. Model Evaluation
  # CrossTable(test$Cat, boost.predict, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual', 'predicted'))

  outs <- mean(predict == test$Cat)

  pbar$step()
}

mean(outs)
