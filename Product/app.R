library(shiny)
library(dplyr)
library(tm)

load("data.RData")
source(file = "Predictor.R", local = TRUE)

shinyApp(
  
  # Define UI for application that draws a histogram
  ui = fluidPage(
     
     # Application title
     titlePanel("Next Word Prediction"),
     
     # Text with no limit can be inputed her
  
     sidebarLayout(
        sidebarPanel(
          textAreaInput("TextInput", "Input text here", height = 200), width = 8
        ),
        
        # show the 5 most powerful predictions each time
        mainPanel(
           DT::dataTableOutput("PredictNextWords")
        )
     )
  ),
  server = function(input, output) {
       output$PredictNextWords <- DT::renderDataTable({
         
       words <- data.frame(NextWords = Predictor(input$TextInput))
       
       words
     }, options = list(dom = 't'), escape = FALSE,selection = list(target='cell',mode='single'), rownames= FALSE)
       
  }
)



