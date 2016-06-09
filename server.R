suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo)))

source("./Cleaner.R")
final4Data <- readRDS(file="./fourgrams.RData")
final3Data <- readRDS(file="./threegrams.RData")
final2Data <- readRDS(file="./twograms.RData")

shinyServer(function(input, output) {
  
  wordPrediction <- reactive({
    text <- input$text
    textInput <- cleanInput(text)
    wordCount <- length(textInput)
    wordPrediction <- nextWordPrediction(wordCount,textInput)})
  
  output$predictedWord <- renderPrint(wordPrediction())
  output$enteredWords <- renderText({ input$text }, quoted = FALSE)
})