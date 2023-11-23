diabetes_prediction_dataset <- read.csv("Plots/diabetes_prediction_dataset.csv", header=TRUE, sep=";")

# Create a contingency table for gender
table_data <- table(diabetes_prediction_dataset$gender, diabetes_prediction_dataset$diabetes)

print(table_data)

# Set up a 1x3 grid layout for two plots side by side
par(mfrow=c(1, 3))

# Function to create a barplot with numbers on top of columns
add_numbers_on_top <- function(data, col, title, pos) {
  bp <- barplot(data, col = col, main = title, xlab = "Diabetes", ylab = "Count")
  text(bp, data, labels = data, pos = pos, cex = 1, col = "black")
}

# Plot 1: Barplot Female
add_numbers_on_top(table_data[1,], c("lightblue", "lightcoral"), "Female Diabetes Counts", 1)

# Plot 2: Barplot Male
add_numbers_on_top(table_data[2,], c("lightblue", "lightcoral"), "Male Diabetes Counts", 1)

# Plot 3: Barplot Others
add_numbers_on_top(table_data[3,], c("lightblue", "lightcoral"), "Others Diabetes Counts", 1)


# Create a contingency table for hypertension
table_data <- table(diabetes_prediction_dataset$hypertension, diabetes_prediction_dataset$diabetes)

print(table_data)


# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 4: Barplot Hypertension 0
add_numbers_on_top(table_data[1,], c("lightblue", "lightcoral"), "Hypertension equals 0", 1)

# Plot 5: Barplot Hypertension 1
add_numbers_on_top(table_data[2,], c("lightblue", "lightcoral"), "Hypertension equals 1", 1)



# Create a contingency table for heart disease
table_data <- table(diabetes_prediction_dataset$heart_disease, diabetes_prediction_dataset$diabetes)

print(table_data)

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 6: Barplot Heart disease 0
add_numbers_on_top(table_data[1,], c("lightblue", "lightcoral"), "Heart disease equals 0", 1)

# Plot 7: Barplot Heart disease 1
add_numbers_on_top(table_data[2,], c("lightblue", "lightcoral"), "Heart disease equals 1", 1)



# Create a contingency table for smoking history
table_data <- table(diabetes_prediction_dataset$smoking_history, diabetes_prediction_dataset$diabetes)

print(table_data)

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 8: Barplot smoking current
add_numbers_on_top(table_data[1,], c("lightblue", "lightcoral"), "Smoking: Current", 1)

# Plot 9: Barplot smoking ever
add_numbers_on_top(table_data[2,], c("lightblue", "lightcoral"), "Smoking: ever", 1)

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 10: Barplot smoking former
add_numbers_on_top(table_data[3,], c("lightblue", "lightcoral"), "Smoking: Former", 1)

# Plot 11: Barplot smoking never
add_numbers_on_top(table_data[4,], c("lightblue", "lightcoral"), "Smoking: never", 1)

# Set up a 1x2 grid layout for two plots side by side
par(mfrow=c(1, 2))

# Plot 12: Barplot smoking former
add_numbers_on_top(table_data[5,], c("lightblue", "lightcoral"), "Smoking: No Info", 1)

# Plot 13: Barplot smoking never
add_numbers_on_top(table_data[6,], c("lightblue", "lightcoral"), "Smoking: not current", 1)

