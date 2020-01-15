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

