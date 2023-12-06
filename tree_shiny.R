library(shiny)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tree"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "minsplit",
                  label = "Minsplit:",
                  min = 100,
                  max = 500,
                  value = 300
                  ),
      sliderInput(inputId = "minbucket",
                  label = "Minbucket:",
                  min = 50,
                  max = 300,
                  value = 100
                  ), 
      sliderInput(inputId = "maxdepth",
                  label = "MaxDepth:",
                  min = 5,
                  max = 15,
                  value = 8
                  ),
      sliderInput(inputId = "cp",
                  label = "Prune with CP_index:",
                  min = 0, 
                  max = 4,
                  value = 2,
                  ),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "tree"),
      tableOutput(outputId = "cptable"),
      plotOutput(outputId = "cp_plot"),
      plotOutput(outputId = "tree_pruned"),
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  library(rpart)
  library(rpart.plot)
  library(caret)
  
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
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$tree <- renderPlot({
    t0 <- rpart(formula = diabetes ~ gender + age +
                  hypertension + heart_disease + smoking_history +
                  bmi + HbA1c_level + blood_glucose_level, data = df.train, 
                control = rpart.control(
                  cp = 0,
                  minsplit = input$minsplit, 
                  minbucket = input$minbucket, 
                  maxdepth = input$maxdepth,
    ))
    rpart.plot(t0, main = "initial Decision Tree (not pruned)")
  })
  
  output$cptable <- renderTable({
    t0 <- rpart(
      formula = diabetes ~ gender + age +
        hypertension + heart_disease + smoking_history +
        bmi + HbA1c_level + blood_glucose_level, 
      data = df.train, 
      control = rpart.control(
        cp = 0,
        minsplit = input$minsplit, 
        minbucket = input$minbucket, 
        maxdepth = input$maxdepth
      )
    )
    as.data.frame(printcp(t0))
  })
  
  output$cp_plot <- renderPlot({
    t0 <- rpart(formula = diabetes ~ gender + age +
                  hypertension + heart_disease + smoking_history +
                  bmi + HbA1c_level + blood_glucose_level, data = df.train, 
                control = rpart.control(
                  cp = 0,
                  minsplit = input$minsplit, 
                  minbucket = input$minbucket, 
                  maxdepth = input$maxdepth,
                ))
    plotcp(t0, main = "Cost Complexity Plot")
  })
  
  output$tree_pruned <- renderPlot({
    t0 <- rpart(formula = diabetes ~ gender + age +
                  hypertension + heart_disease + smoking_history +
                  bmi + HbA1c_level + blood_glucose_level, data = df.train, 
                control = rpart.control(
                  cp = 0,
                  minsplit = input$minsplit, 
                  minbucket = input$minbucket, 
                  maxdepth = input$maxdepth,
                ))
    CP <- t0$cptable[, "CP"] 
    CP <- rev(CP) 
    cp <- sqrt(CP[-1] * CP[-length(CP)]) 
    cp <- c(cp, Inf)
    t <- prune(tree = t0, cp = cp[input$cp] )
    rpart.plot(t,  main = "Pruned Decision Tree")
  })
 
  
}

shinyApp(ui = ui, server = server)

