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

