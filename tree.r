library(rpart)
library(rpart.plot)

df <- read.csv("diabetes_prediction_dataset.csv")
head(df)
set.seed(123)
subs.train<- sample(1:nrow(df) ,0.8 * nrow(df))
df.train <- df[subs.train,]
df.test<- df[-subs.train,]
t0 <- rpart(formula = diabetes ~ gender + age +
hypertension + heart_disease + smoking_history +
bmi + HbA1c_level + blood_glucose_level, data = df.train, 
control = rpart.control(
    cp = 0, 
    minsplit = 400, 
    minbucket = 100, 
    maxdepth =  5
    ))
rpart.plot(t0)
printcp(t0)
CP <- t0$cptable[, "CP"] 
CP <- rev(CP) 
cp <- sqrt(CP[-1] * CP[-length(CP)]) 
cp <- c(cp, Inf) 