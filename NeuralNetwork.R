install.packages("neuralnet")
install.packages("boot")
install.packages("gmodels")

install.packages("devtools")
library(devtools)
devtools::install_github('m-clark/confusionMatrix')
library(confusionMatrix)

library(boot)
library(neuralnet)
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
  return ((x-min(x)) / (max(x)-min(x)))
}

data_norm <- as.data.frame(lapply(data, normalize))

# all data is normalised from 0 to 1
head(data_norm)

# Configuration
number_of_runs <- 5
training_prop <- 0.7
hidden_layer_structure <- c(100, 50, 25)

ptm <- proc.time()

for(i in 1:number_of_runs){
  # 4. Data splitting
  data_rows <- nrow(data_norm)
  data_sample <- sample(data_rows, data_rows * training_prop)
  data_train <- data_norm[data_sample,]
  data_test <- data_norm[-data_sample,]

  # 5. Model creation (training data)
  data_model <- neuralnet(T1+T2+T3+T4+T5~P1+P2+P3+P4+P5+P6+P7+P8+P9+P10,
                          data = data_train,
                          hidden = hidden_layer_structure,
                          stepmax = 1000000)

  # 6. Prediction (test data)
  model_results <- compute(data_model, data_test[1:10])

  # 7. Model Evaluation
  predicted_alcohol <- model_results$net.result

  original_values <- max.col(data_test[, 11:15])
  predicted_values <- max.col(predicted_alcohol)

  CrossTable(original_values, predicted_values, prop.chisq = FALSE, prop.r = FALSE, prop.c=FALSE, dnn = c("actual","predicted"))
  t <- confusion_matrix(predicted_values, original_values, return_table = TRUE)
  f <- data.frame(
    t$Other$Class,
    t$Other$`Balanced Accuracy`,
    t$Other$`PPV/Precision`,
    t$Other$`Sensitivity/Recall/TPR`,
    t$Other$`FPR/Fallout`,
    t$Other$FNR)
  names(f) <- c('Class', 'Accuracy', 'Precision', 'Recall', 'FAR', 'FRR')
  print(f)
}

time <- proc.time() - ptm
print(time)

plot(data_model,
     radius = 0.1,
     show.weights = FALSE,
     intercept = FALSE,
     information = TRUE,
     arrow.length = 0.1,
     col.hidden = "red")
