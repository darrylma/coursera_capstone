#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)

source("./predict.R")

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  prediction <- reactive({
    data <- predict_word(input$inputText)
    if(is.null(data)){
      validate(
        need(!is.null(data), "Please provide more text for the prediction model")
      )
      return(NULL)
    }
    return(data)
  })
    
  output$predictions <- renderTable({
    req(nrow(prediction()) > 0)
    data <- subset(prediction(), select=-c(freq,perc))
    if(nrow(prediction()) > 5){
      data[1:5,]
    }
    else{
      data
    }
  })
  
  output$wordCloud <- renderPlot({
    req(nrow(prediction()) > 0)
    wordcloud(words = prediction()$predicted, freq = prediction()$freq, min.freq = 1,
              max.words=50, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
})
