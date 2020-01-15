install.packages("C50")


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

head(data_norm)

# 3.5 Train Model (without splitting)
# achieves 100% accuracy
library(C50)
set.seed(123)
model <- C5.0(data_norm[-11], data_norm$Cat)
model
summary(model)
plot(model) # error???

# 4. Data splitting

rows <- nrow(data_norm)
sample <- sample(rows, rows * 0.6)
train <- data_norm[sample,]
test <- data_norm[-sample,]

# check that ratio of categorisation from one data set
# to another is valid and all categories are represented
prop.table(table(train$Cat))
prop.table(table(test$Cat))

library(C50)
model2 <- C5.0(train[-11], train$Cat)
model2
summary(model2)

predict <- predict(model2, test)

library(gmodels)
CrossTable(test$Cat, predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
