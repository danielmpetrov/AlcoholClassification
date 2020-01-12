rm(list = ls())

# 1. Load data
data <- read.csv("QCM/QCM12.csv", sep = ";")

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
install.packages("neuralnet")
library(neuralnet)

# train the model
data_model <- neuralnet(
  X1.Octanol+X1.Propanol+X2.Butanol+X2.propanol+X1.isobutanol~X0.799_0.201+X0.799_0.201.1+X0.700_0.300+X0.700_0.300.1+X0.600_0.400+X0.600_0.400.1+X0.501_0.499+X0.501_0.499.1+X0.400_0.600+X0.400_0.600.1,
  data = data_train,
  hidden = c(30, 10))

plot(data_model) # yikes

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
