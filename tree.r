library(rpart)
library(rpart.plot)
library(caret)

# read csv with data
df <- read.csv("Prepped_diabetes_data_named.data")
# diabetes should be treated as a factor
df$diabetes <- as.factor(df$diabetes)
head(df)

# set seed for predictability
set.seed(123)
# select random samples
subs.train<- sample(1:nrow(df) ,0.8 * nrow(df)) 

# select training data
df.train <- df[subs.train,]
# select test data
df.test<- df[-subs.train,]

#initial tree without pruning limited by minsplit minbucket and maxdepth
t0 <- rpart(formula = diabetes ~ gender + age +
  hypertension + heart_disease + smoking_history +
  bmi + HbA1c_level + blood_glucose_level, data = df.train, 
  control = rpart.control(
    cp = 0,
    minsplit = 300, 
    minbucket = 100, 
    maxdepth = 8,
    
))
#plot initial tree
rpart.plot(t0)


#cp table
printcp(t0)

# get cp_i values into an array 
CP <- t0$cptable[, "CP"] 
CP <- rev(CP) 
cp <- sqrt(CP[-1] * CP[-length(CP)]) 
cp <- c(cp, Inf) 

plotcp(t0)
# cp 0.0059 chosen (because of 1-STD-dev rule) which is cp[2]

# pruning with the cp value
t <- prune(tree = t0, cp = cp[2] )
rpart.plot(t)

# prediction
p <- predict(t, newdata = df.test, type = "class")

# confusion matrix
pure_table <- table(p,df.test$diabetes)
conf_mat <- confusionMatrix(data = p, reference = df.test$diabetes, positive = "1")
conf_mat
# percentage values of confusion matrix
prop.table(pure_table)


# error rate (= 1- accuracy)
sum(p != df.test$diabetes) / nrow(df.test)

sensitivity <- conf_matrix$byClass["Sensitivity"]
sensitivity

specificity <- conf_matrix$byClass["Specificity"]
specificity

precision <- conf_matrix$byClass["Pos Pred Value"]
precision

f1_score <- conf_matrix$byClass["F1"]
f1_score


saveRDS(t0, file = "initial_tree.rda")
saveRDS(t, file = "pruned_tree.rda")
