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
