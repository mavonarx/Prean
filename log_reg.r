library(caret)

# Assuming your file path is correct
data <- read.csv("Prepped_diabetes_data_named.data")

# Check the structure of your data
str(data)

# Assuming "diabetes" is the response variable
subs.train <- sample(1:nrow(data), 0.8 * nrow(data))
data.train <- data[subs.train, ]
data.test <- data[-subs.train, ]

# Specify the formula correctly
model <- glm(diabetes ~ ., family = binomial(link = 'logit'), data = data.train)

# Make predictions on the test set
data.test$model_pred <- predict(model, newdata = data.test, type = 'response')

# Threshold the predictions (assuming you want binary predictions)
data.test$model_pred_binary <- ifelse(data.test$model_pred > 0.5, 1, 0)

# Print the confusion matrix
confusion_matrix <- table(data.test$diabetes, data.test$model_pred_binary)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))

# Print the summary of the logistic regression model
summary(model)
