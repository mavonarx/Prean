install.packages("caret")
library(caret)


diabetes_prediction_dataset <- read.csv("Plots/diabetes_prediction_dataset.csv", header=TRUE, sep=";")

# One Hot Encode Gender Column
one_hot_encoded_data <- dummyVars(" ~ gender", data = diabetes_prediction_dataset)
one_hot_encoded_data <- data.frame(predict(one_hot_encoded_data, newdata = diabetes_prediction_dataset))

# Append One Hot Encoding to existing Dataset
diabetes_prediction_dataset$encodedFemale <- one_hot_encoded_data$genderFemale
diabetes_prediction_dataset$encodedMale <- one_hot_encoded_data$genderMale
diabetes_prediction_dataset$encodedOther <- one_hot_encoded_data$genderOther

# One Hot Encode Gender Smoking_History
one_hot_encoded_data <- dummyVars(" ~ smoking_history", data = diabetes_prediction_dataset)
one_hot_encoded_data <- data.frame(predict(one_hot_encoded_data, newdata = diabetes_prediction_dataset))

# Append One Hot Encoding to existing Dataset
diabetes_prediction_dataset$encoded_smoking_current <- one_hot_encoded_data$smoking_historycurrent
diabetes_prediction_dataset$encoded_smoking_ever <- one_hot_encoded_data$smoking_historyever
diabetes_prediction_dataset$encoded_smoking_former <- one_hot_encoded_data$smoking_historyformer
diabetes_prediction_dataset$encoded_smoking_never <- one_hot_encoded_data$smoking_historynever
diabetes_prediction_dataset$encoded_smoking_noInfo <- one_hot_encoded_data$smoking_historyNo.Info
diabetes_prediction_dataset$encoded_smoking_notCurrent <- one_hot_encoded_data$smoking_historynot.current

# Calculate Correlation between Variable and Diabetes
correlation <- c(
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$encodedFemale),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$encodedMale),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$encodedOther),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$encoded_smoking_current),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$encoded_smoking_ever),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$encoded_smoking_former),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$encoded_smoking_never),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$encoded_smoking_noInfo),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$encoded_smoking_notCurrent),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$age),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$hypertension),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$heart_disease),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$bmi),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$HbA1c_level),
cor(diabetes_prediction_dataset$diabetes, diabetes_prediction_dataset$blood_glucose_level))

x <- c("Female", "Male", "Other","Smoking_current","Smoking_ever","Smoking_former","Smoking_never","Smoking_noInfo","Smoking_notCurrent", "Age", "Hypertension", "Heart_disease", "BMI", "HbA1c", "blood_glucose")

# Set Margin
par(mar = c(5, 10, 4, 2))

# 1. Plot: Barplot for Correlation between Variables 1-8 and Diabetes
barplot(height = correlation[1:8], names.arg = x[1:8], las = 2, main= "Correlation with Diabetes", xlim = c(-0.5, 0.5), horiz = TRUE)

# Set Margin
par(mar = c(5, 10, 4, 2))

# 1. Plot: Barplot for Correlation between Variables 9-15 and Diabetes
barplot(height = correlation[9:length(correlation)], names.arg = x[9:length(correlation)], las = 2, main= "Correlation with Diabetes", xlim = c(-0.5, 0.5), horiz = TRUE)

