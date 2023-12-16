# Read the data
diabetes_prediction_dataset <- read.csv("diabetes_prediction_dataset.csv", header=TRUE, sep=";")

unique_smoking_history <- unique(diabetes_prediction_dataset$smoking_history)
unique_diabetes <- unique(diabetes_prediction_dataset$diabetes)

print(unique_smoking_history)
print(unique_diabetes)


mosaicplot(table(diabetes_prediction_dataset$smoking_history, diabetes_prediction_dataset$diabetes),
           main="Mosaic Plot of Diabetes vs Smoking History", col=c("lightgreen", "skyblue"))
