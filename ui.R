#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)

ngram <- load("C:\\Users\\David\\Documents\\GitHub\\CapstonFinalReport\\ngram.RData")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Find the possible next word in the sentance"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput(inputId="textsample",label="Enter in the text and will try and find next word",value = "Enter something")
    ),
    
    mainPanel(
      h3(textOutput("textsample")),
      helpText('     '),
      textOutput("answer")
    )
  )
))