library(shiny)
library(rpart)
library(rpart.plot)
library(caret)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tree"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "gender",
                  label = "gender:",
                  min = 1,
                  max = 3,
                  value = 3
      ),
      sliderInput(inputId = "age",
                  label = "age:",
                  min = 0,
                  max = 100,
                  value = 50
      ), 
      sliderInput(inputId = "hypertension",
                  label = "hypertension:",
                  min = 0,
                  max = 1,
                  value = 1
      ),
      sliderInput(inputId = "heart_disease",
                  label = "heart_disease:",
                  min = 0,
                  max = 1,
                  value = 1
      ),
      sliderInput(inputId = "smoking_history",
                  label = "smoking_history:",
                  min = 0, 
                  max = 4,
                  value = 2,
      ),
      sliderInput(inputId = "bmi",
                  label = "bmi:",
                  min = 5, 
                  max = 100,
                  value = 24,
      ),
      sliderInput(inputId = "HbA1c_level",
                  label = "HbA1c_level:",
                  min = 2, 
                  max = 10,
                  value = 5,
      ),
      sliderInput(inputId = "blood_glucose_level",
                  label = "blood_glucose_level:",
                  min = 50, 
                  max = 300,
                  value = 130,
      ),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      textOutput(outputId = "logreg_diabetes_prediction"),
      textOutput(outputId = "prune_tree_diabetes_prediction"),
      textOutput(outputId = "KNN_diabetes_prediction")
      
    )
  )
)

logreg = readRDS("logreg_model.rda")
tree = readRDS("pruned_tree.rda")



server <- function(input, output) {
  
  Data_Frame <- reactive({
    # Debugging: print the inputs
    data.frame(
      gender = input$gender,
      age = input$age,
      hypertension = input$hypertension,
      heart_disease = input$heart_disease,  
      smoking_history = input$smoking_history,
      bmi = input$bmi,
      HbA1c_level = input$HbA1c_level,
      blood_glucose_level = input$blood_glucose_level
    )
  })
  #predictions <- predict.glm(logreg, newdata = Data_Frame, type = "response")
  
  output$logreg_diabetes_prediction <- renderText({
    df <- Data_Frame()
    prediction <- predict.glm(logreg, newdata = df, type = "response")
    if (prediction > 0.5) {
      return(paste("Logistic regression Predicts:  Diabetes with:", round(prediction*100, 3), "%"))
    } else {
      return(paste("Logistic regression Predicts:  No Diabetes with:", 100-round(prediction*100, 3), "%"))
    }
  })
  
  output$prune_tree_diabetes_prediction <- renderText({
    df <- Data_Frame()
    prediction = predict(tree, newdata = df)
    if (prediction[2] > 0.5) {
      return(paste("Pruned Tree Predicts:  Diabetes with:", round(prediction[2]*100, 3), "%"))
    } else {
      return(paste("Pruned Tree Predicts:  No Diabetes with:", 100-round(prediction[2]*100, 3), "%"))
    }
  })
  
  output$KNN_diabetes_prediction <- renderText({
    df <- Data_Frame()
    set.seed(42)
    
    data <- read.csv("Prepped_diabetes_data_named.data")
    data$diabetes <- as.factor(data$diabetes)
    
    # Splitting the data
    subs.train <- sample(1:nrow(data), 0.8 * nrow(data))
    data.train <- data[subs.train, ]
    data.test <- data[-subs.train, ]
    
    # Scaling
    scale_func <- preProcess(data.train[-9], method = c("center", "scale"))
    data.train_scaled <- predict(scale_func, data.train[-9])
    df_scaled <- predict(scale_func, df[-9])
    
    # KNN prediction
    pred <- knn(
      train = data.train_scaled,
      test = df_scaled,
      cl = data.train$diabetes,
      k = 15
    )
    print(pred)
    if(pred != 0){ 
      return("KNN Predicts:  Diabetes")
    } else {
      return("KNN Predicts:  No Diabetes")
    }
  })
  
  
}

shinyApp(ui = ui, server = server)
