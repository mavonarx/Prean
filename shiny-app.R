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
      textOutput(outputId = "diabetes_prediction"),
      tableOutput(outputId = "cptable"),
      plotOutput(outputId = "cp_plot"),
      plotOutput(outputId = "tree_pruned"),
    )
  )
)

logreg = readRDS("logreg_model.rda")
tree = readRDS("initial_tree.rda")
pruned_tree = readRDS("pruned_tree.rda")



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
  
  output$diabetes_prediction <- renderText({
    df <- Data_Frame()
    prediction <- predict.glm(logreg, newdata = df, type = "response")
    if (prediction > 0.5) {
      return("Logistic regression Predicts:  Diabetes")
    } else {
      return("Logistic regression Predicts:  No diabetes")
    }
  })
  output$diabetes_prediction <- renderText({
    df <- Data_Frame()
    pred = predict(tree, df = df.test, type = "class")
  })
  
}

shinyApp(ui = ui, server = server)
