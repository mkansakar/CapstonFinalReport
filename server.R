#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  next.word <- function(text) {
    words_list <- strsplit(text, split = " ")
    words <- words_list[[1]]
    nWords <- length(words)
    fourgram <- paste(words[nWords - 2], words[nWords - 1], words[nWords], collapse = " ")
    trigram <- paste(words[nWords - 1], words[nWords], collapse = " ")
    bigram <- words[nWords]
    
    if (sum(dt.four$look == fourgram)>0) {
      result <- as.character(dt.four[dt.four$look == fourgram, c("output")])
    }
    else {
      if (sum(dt.tri$look == trigram)>0) {
        result <- as.character(dt.tri[dt.tri$look == trigram, c("output")])
      }
      else {
        if (sum(dt.bi$look == bigram)>0) {
          result <- as.character(dt.bi[dt.bi$look == bigram, c("output")])
        }
        else {
          index <- sample(1:100, 1)
          result <- dt.one[index]
        }
      }
    }
    result
  }

  textdata <- reactive({input$textsample})

  nextword <- next.word("textdata")

  output$answer <- nextword
  output$textsample <- textdata

})
