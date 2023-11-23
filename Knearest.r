library(caTools)
library(class)
library(caret)
set.seed(42)
data <- read.csv("SemesterArbeit/Prepped_diabetes_data_named.data")

head(data)

subs.train <- sample(1:100000, 80000)
data.train <- data[subs.train, ]
data.test <- data[-subs.train, ]

train_scaled <- scale(data.train[-9])
test_scaled <- scale(data.test[-9])

pred <- knn(
    train <- data.train,
    test <- data.test,
    cl <- data.train$diabetes,
    k <- 3,
    prob <- FALSE
)

target <- data.test$diabetes


summary(pred)
