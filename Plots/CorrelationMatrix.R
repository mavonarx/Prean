library(corrplot)
# Read the data
diabetes_prediction_dataset <- read.csv("Plots/diabetes_prediction_dataset.csv", header=TRUE, sep=";")

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Subset data for individuals with diabetes
data_with_diabetes <- diabetes_prediction_dataset[diabetes_prediction_dataset$diabetes == 1, ]

# Compute correlation matrix for individuals with diabetes
correlation_matrix_with_diabetes <- cor(data_with_diabetes[c("age", "bmi", "HbA1c_level", "blood_glucose_level")])

# Plot correlation matrix for individuals with diabetes as a heatmap
corrplot(correlation_matrix_with_diabetes, method="color", addCoef.col="black", tl.col="black", tl.srt=45)
title("Heatmap - Diabetes Group")

# Subset data for individuals without diabetes
data_without_diabetes <- diabetes_prediction_dataset[diabetes_prediction_dataset$diabetes == 0, ]

# Compute correlation matrix for individuals without diabetes
correlation_matrix_without_diabetes <- cor(data_without_diabetes[c("age", "bmi", "HbA1c_level", "blood_glucose_level")])

# Plot correlation matrix for individuals without diabetes as a heatmap
corrplot(correlation_matrix_without_diabetes, method="color", addCoef.col="black", tl.col="black", tl.srt=45)
title("Heatmap - Non Diabetes Group")

# Reset the layout to the default (1x1) after creating the plots
par(mfrow=c(1, 1))