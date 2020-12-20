# Import libraries
library(shiny)
library(corrplot)
library(ggplot2)
library(smotefamily)
library(caret)
library(randomForest)
library(ROCR)

# Read in the RF model
model <- readRDS("model.rds")


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Loan Default prediction'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    numericInput("age", 
                 label = "Age of Customer", 
                 value = 20),
    numericInput("ed", 
                 label = "Education Category", 
                 value = 1),
    numericInput("employ", 
                 label = "Employment status", 
                 value = 1),
    numericInput("address", 
                 label = "Geographic area", 
                 value = 1),
    numericInput("income", 
                 label = "Gross Income", 
                 value = 15),
    numericInput("debtinc", 
                 label = "Individual's debt", 
                 value = 5),
    numericInput("creddebt", 
                 label = "Debt-to-Credit Ratio", 
                 value = 0.3),
    numericInput("othdebt", 
                 label = "Any other debts", 
                 value = 1.2),
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("age",
               "ed",
               "employ",
               "address",
               "income",
               "debtinc", 
               "creddebt", 
               "othdebt"),
      Value = as.numeric(c(input$age,
                           input$ed,
                           input$employ,
                           input$address,
                           input$income,
                           input$debtinc,
                           input$creddebt,
                           input$othdebt))
    )
    
    default <- "default"
    df <- rbind(df, default)
    input <- t(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Probability=predict(model,test,type="response")
    Prediction = ifelse(Probability > 0.4795718, "Default","No Default")
    Output <- data.frame(Prediction,Probability)
      print(Output)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)

