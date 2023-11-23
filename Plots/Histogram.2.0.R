# Read the data
diabetes_prediction_dataset <- read.csv("diabetes_prediction_dataset.csv", header=TRUE, sep=";")

head(diabetes_prediction_dataset)

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 1: Histogram for age distribution
hist(diabetes_prediction_dataset$age, main="Age Distribution", xlab="Age", ylab="Count", col="lightgrey")

# Plot 2: Histogram for BMI distribution
hist(diabetes_prediction_dataset$bmi, main="BMI Distribution", xlab="BMI", ylab="Count", col="pink")

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 3: Barplot for Gender distribution
barplot(table(diabetes_prediction_dataset$gender), main="Gender Distribution", xlab = "Categories", ylab = "Frequency", col="lightgrey")

# Plot 4: Histogram for Hypertension distribution
barplot(table(diabetes_prediction_dataset$hypertension), main="Hypertension Distribution", xlab = "Categories", ylab = "Frequency", col="pink")

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 5: Barplot for Heart Disease distribution
barplot(table(diabetes_prediction_dataset$heart_disease), main="Heart Disease Distribution", xlab = "Categories", ylab = "Frequency", col="lightgrey")

# Plot 6: Barplot for Smoking History distribution
barplot(table(diabetes_prediction_dataset$smoking_history), main="Smoking History Distribution", xlab = "Categories", ylab = "Frequency", col="pink")

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 7: Histogram for HbA1c distribution
hist(diabetes_prediction_dataset$HbA1c_level, main="HbA1c Distribution", xlab="HbA1c Distribution", ylab="Count", col="pink")

# Plot 8: Barplot for Diabetes distribution
barplot(table(diabetes_prediction_dataset$diabetes), main="Diabetes Status", xlab="Diabetes", ylab="Count", col="lightgrey")

# Reset the layout to the default (1x1) after creating the plots
par(mfrow=c(1, 1))

