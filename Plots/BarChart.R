# Read the data
diabetes_prediction_dataset <- read.csv("Plots/diabetes_prediction_dataset.csv", header=TRUE, sep=";")

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 1: Bar chart for gender distribution
barplot(table(diabetes_prediction_dataset$gender), main="Gender Distribution", xlab="Gender", ylab="Count", col="skyblue")

# Plot 2: Bar chart for diabetes status
barplot(table(diabetes_prediction_dataset$diabetes), main="Diabetes Status", xlab="Diabetes", ylab="Count", col="lightgreen")

# Reset the layout to the default (1x1) after creating the plots
par(mfrow=c(1, 1))
