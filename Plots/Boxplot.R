# Read the data
diabetes_prediction_dataset <- read.csv("diabetes_prediction_dataset.csv", header=TRUE, sep=";")

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 1: Box plot for age grouped by diabetes status
boxplot(age ~ diabetes, data=diabetes_prediction_dataset, main="Age by Diabetes Status", xlab="Diabetes", ylab="Age", col=c("skyblue", "lightgreen"))

# Plot 2: Box plot for blood glucose level grouped by diabetes status
boxplot(blood_glucose_level ~ diabetes, data=diabetes_prediction_dataset, main="Blood Glucose Level by Diabetes Status", xlab="Diabetes", ylab="Blood Glucose Level", col=c("skyblue", "lightgreen"))

# Reset the layout to the default (1x1) after creating the plots
par(mfrow=c(1, 1))
