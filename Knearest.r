library(caTools)
library(class)
library(caret)
set.seed(42)

data <- read.csv("Prepped_diabetes_data_named.data")

data$diabetes <- as.factor(data$diabetes)
# Check the structure of your data
str(data)

# Assuming "diabetes" is the response variable
subs.train <- sample(1:nrow(data), 0.8 * nrow(data))
data.train <- data[subs.train, ]
data.test <- data[-subs.train, ]

data.train_scaled = scale(data.train[-9])
test_scaled_scaled = scale(data.test[-9])

pred <- knn(
    train <- data.train_scaled,
    test <- test_scaled_scaled,
    cl <- data.train$diabetes,
    k <- 15,
    prob <- FALSE
)

#attributes(.Last.value)


# Hilfsfunktion
model_evaluation <- function(predictions, test_data) {
  
  #predictions.class <- ifelse(predictions > 0.5, 1, 0)
  
  # Konfussionsmatrix
  conf_matrix <- table(predictions, test_data$diabetes, dnn = list("Predicted" = "Predicted", "Actual" = "Actual"))
  print(conf_matrix)
  
  # Berechnung der Gütemaße aus der Confusion Matrix
  TP <- conf_matrix[2, 2]  # True Positives
  TN <- conf_matrix[1, 1]  # True Negatives
  FP <- conf_matrix[2, 1]  # False Positives
  FN <- conf_matrix[1, 2]  # False Negatives
  
  # Fehler
  err <- (FP + FN)/ (FP + FN + TN + TP)
  print(paste("Relativer Klassifikationsfehler: ",err * 100, "%"))
  # Accuracy
  acc <- (TP + TN) / (FP + FN + TN + TP)
  print(paste("Accuracy: ",acc * 100, "%"))
  # Precision
  prec <- TP / (TP + FP)
  print(paste("Precision: ",prec * 100, "%"))
  # Recall
  rec <- TP / (TP + FN)
  print(paste("Recall: ",rec * 100, "%"))
  # F1-Score berechnen
  f1_score <- 2 * ((prec * rec) / (prec + rec))
  print(paste("F1-Score: ",f1_score * 100, "%"))
}

#confusionMatrix(data = pred, reference = data.test$diabetes, positive = "1")

summary(pred)
model_evaluation(pred, data.test)