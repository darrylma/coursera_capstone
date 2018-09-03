#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("
    .shiny-output-error-validation {
    color: red;
    }
    "))
  ),
    
  # Application title
  titlePanel("Predicting the Next Word"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Instuctions: Please enter text in the input box below. The most likely next words will then be diplayed in the table on the right"),
      textInput("inputText", "Enter a word or a sentence",value = "")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      helpText("The table below shows which words (i.e. word1, word2, word3) were used to determine the next 'predicted' words and provides the likelihood of each next word"),
      tableOutput("predictions"),
      helpText("The size of each word in the word cloud below indicates its likelihood to be the next work"),
      plotOutput("wordCloud")
    )
  )
))
