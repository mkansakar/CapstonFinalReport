suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo)))

fourgrams <- readRDS(file="./fourgrams.RData")
threegrams <- readRDS(file="./threegrams.RData")
twograms <- readRDS(file="./twograms.RData")


dataCleaner<-function(text){
  
  cleanText <- tolower(text)
  cleanText <- removePunctuation(cleanText)
  cleanText <- removeNumbers(cleanText)
  cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
  cleanText <- stripWhitespace(cleanText)
  
  return(cleanText)
}

cleanInput <- function(text){
  
  textInput <- dataCleaner(text)
  textInput <- txt.to.words.ext(textInput, 
                                language="English.all", 
                                preserve.case = TRUE)
  
  return(textInput)
}


nextWordPrediction <- function(wordCount,textInput){
  
  if (wordCount>=3) {
    textInput <- textInput[(wordCount-2):wordCount] 
    
  }
  
  else if(wordCount==2) {
    textInput <- c(NA,textInput)   
  }
  
  else {
    textInput <- c(NA,NA,textInput)
  }
  
  
  ### 1 ###
  wordPrediction <- as.character(fourgrams[fourgrams$unigram==textInput[1] & 
                                              fourgrams$bigram==textInput[2] & 
                                              fourgrams$trigram==textInput[3],][1,]$quadgram)
  
  if(is.na(wordPrediction)) {
    wordPrediction1 <- as.character(threegrams[threegrams$unigram==textInput[2] & 
                                                 threegrams$bigram==textInput[3],][1,]$trigram)
    
    if(is.na(wordPrediction)) {
      wordPrediction <- as.character(twograms[twograms$unigram==textInput[3],][1,]$bigram)
    }
  }
  
  
  print(wordPrediction)
  
}