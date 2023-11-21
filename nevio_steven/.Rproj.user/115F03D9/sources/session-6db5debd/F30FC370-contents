# Read the data
diabetes_prediction_dataset <- read.csv("diabetes_prediction_dataset.csv", header=TRUE, sep=";")

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 1: Histogram for age distribution
hist(diabetes_prediction_dataset$age, main="Age Distribution", xlab="Age", ylab="Count", col="lightgrey")

# Plot 2: Histogram for BMI distribution
hist(diabetes_prediction_dataset$bmi, main="BMI Distribution", xlab="BMI", ylab="Count", col="pink")

# Reset the layout to the default (1x1) after creating the plots
par(mfrow=c(1, 1))
