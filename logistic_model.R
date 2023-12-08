install.packages("caret")  # Falls 'caret' noch nicht installiert ist
library(caret)

# Daten einlesen
diabetes_prediction_dataset <- read.csv("Prepped_diabetes_data_named.data")

head(diabetes_prediction_dataset)

# Stratifizierter Split der Daten, 80% der Daten für das Training, 20% für Test
train_indices <- createDataPartition(diabetes_prediction_dataset$diabetes, p = 0.8, list = FALSE, times = 1)
diabetes_prediction_dataset.train <- diabetes_prediction_dataset[train_indices,]
diabetes_prediction_dataset.test <- diabetes_prediction_dataset[-train_indices,]

# Hilfsfunktion
model_evaluation <- function(model, test_data) {
  predictions <- predict.glm(model, newdata = test_data, type = "response")
  predictions.class <- ifelse(predictions > 0.5, 1, 0)
  
  # Konfussionsmatrix
  conf_matrix <- table(predictions.class, test_data$diabetes, dnn = list("Predicted" = "Predicted", "Actual" = "Actual"))
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

# Trainieren eines Modells mit angepasstem Optimierungsalgorithmus
model.full <- glm(diabetes ~ gender + age + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level, 
             data = diabetes_prediction_dataset.train, family = binomial(link = "logit"))

# Evaluation des vollen Modell
summary(model.full)
model_evaluation(model.full, diabetes_prediction_dataset.test)

# Rückwertselimination des Model
model.back <- step(model.full, direction = "backward", trace = 0)

# Evaluation des selektierten Modell
summary(model.back)
model_evaluation(model.back, diabetes_prediction_dataset.test)

# Vorwärtselimination
model.zero <- glm(diabetes ~ 1, 
                  data = diabetes_prediction_dataset.train, family = binomial(link = "logit"))
model.forward <- step(model.zero, direction = "forward", scope = ~ gender + age + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level, trace = 0)

# Evaluation des selektierten Modell
summary(model.forward)
model_evaluation(model.forward, diabetes_prediction_dataset.test)

# Manuelles Modelbuilding
model.one <- glm(diabetes ~ age + bmi + HbA1c_level + blood_glucose_level, 
                 data = diabetes_prediction_dataset.train, family = binomial(link = "logit"))
model.two <- glm(diabetes ~ HbA1c_level + blood_glucose_level, 
                 data = diabetes_prediction_dataset.train, family = binomial(link = "logit"))

# Evaluation Model Manuel
summary(model.one)
model_evaluation(model.one, diabetes_prediction_dataset.test)
summary(model.two)
model_evaluation(model.two, diabetes_prediction_dataset.test)


saveRDS(model, file = "logreg_model.rda")
