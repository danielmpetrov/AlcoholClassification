install.packages("C50")
install.packages("gmodels")

install.packages("devtools")
library(devtools)
devtools::install_github('m-clark/confusionMatrix')
library(confusionMatrix)

library(C50)
library(gmodels)

rm(list = ls())

# 1. Load data
qcm3 <- read.csv("QCM/QCM3.csv", sep = ";")
qcm6 <- read.csv("QCM/QCM6.csv", sep = ";")
qcm7 <- read.csv("QCM/QCM7.csv", sep = ";")
qcm10 <- read.csv("QCM/QCM10.csv", sep = ";")
qcm12 <- read.csv("QCM/QCM12.csv", sep = ";")
data <- rbind(qcm3, qcm6, qcm7, qcm10, qcm12)
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

# Configuration
training_prop <- 0.6

n <- 0
while(n < 5){
  # 4. Data splitting
  rows <- nrow(data_norm)
  sample <- sample(rows, rows * training_prop)
  train <- data_norm[sample,]
  test <- data_norm[-sample,]

  # consider only splits that have all 5 factors in the testing and training data
  if(length(unique(test$Cat)) < 5){
    next
  }

  if(length(unique(train$Cat)) < 5){
    next
  }

  # 5. Model creation (training data)
  model <- C5.0(train[-11], train$Cat)
  # model
  # summary(model)

  # 6. Prediction (test data)
  predict <- predict(model, test)

  # 7. Model Evaluation
  CrossTable(test$Cat, predict, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual', 'predicted'))
  t <- confusion_matrix(predict, test$Cat, return_table = TRUE)
  f <- data.frame(
    t$Other$Class,
    t$Other$`Balanced Accuracy`,
    t$Other$`PPV/Precision`,
    t$Other$`Sensitivity/Recall/TPR`,
    t$Other$`FPR/Fallout`,
    t$Other$FNR)
  names(f) <- c('Class', 'Accuracy', 'Precision', 'Recall', 'FAR', 'FRR')
  print(f)

  n = n + 1
}
