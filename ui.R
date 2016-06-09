suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo)))

shinyUI(navbarPage("Coursera Data Science Capstone Final Report", 
                   theme = shinytheme("flatly"),
                   tabPanel("Next Word Prediction",
                            fluidRow(
                              column(3),
                              column(6,
                                     tags$div(textInput("text", 
                                                        label = h3("Enter your text here in english:"),
                                                        value = ),
                                              br(),
                                              tags$hr(),
                                              h4("The predicted next word is:"),
                                              tags$span(style="color:purple",
                                                        tags$strong(tags$h3(textOutput("predictedWord")))),
                                              br(),
                                              tags$hr(),
                                              h4("You entered:"),
                                              tags$em(tags$h4(textOutput("enteredWords"))),
                                              align="center")
                              )))))
