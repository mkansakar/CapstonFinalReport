library(NLP) 
library(openNLP) 
library(tm)
library(ngram)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringi)
library(scales)
library(wordcloud)
library(BiocInstaller)
library(bnlearn)
library(RColorBrewer) 
library(reshape2)
library(intergraph)
library(GGally)
library(network)
library(sna)
library(qdap)
library(rJava)
library(RWeka)

setwd("D:\\downloads\\Class\\Capstone\\files\\final\\en_US")

#loading all of the data from the txt files
blogs <- readLines("en_US.blogs.txt")
news <- readLines("en_US.news.txt")
twitter <- readLines("en_US.twitter.txt")

# load the profanity list file
profanity <- readLines("D:\\downloads\\Class\\Capstone\\files\\profanity.txt")

set.seed(123)
blogs <- blogs[rbinom(length(blogs)*.1, length(blogs), .5)]
write.csv(blogs, file = "D:\\downloads\\Class\\Capstone\\files\\blog.sample.csv", 
          row.names = FALSE, col.names = FALSE)

set.seed(123)
news <- news[rbinom(length(news)*.1, length(news), .5)]
write.csv(news, file = "D:\\downloads\\Class\\Capstone\\files\\news.sample.csv", 
          row.names = FALSE, col.names = FALSE)

set.seed(123)
twitter <- twitter[rbinom(length(twitter)*.1, length(twitter), .5)]
write.csv(twitter, file = "D:\\downloads\\Class\\Capstone\\files\\twitter.sample.csv", 
          row.names = FALSE, col.names = FALSE)

# put all of the samples into one Corpus document
corpusExampleDoc <- Corpus(DirSource("D:\\downloads\\Class\\Capstone\\files\\"), readerControl = list(language="en_US"))

rm(blogs, news, twitter)

tokenizator <- function (x){
  library(tm)
  corpus <- Corpus(VectorSource(x)) # make a corpus object
  corpus <- tm_map(corpus, tolower) # make everything lowercase
  corpus <- tm_map(corpus, removeWords,stopwords("english")) # remove stop words
  corpus <- tm_map(corpus, removePunctuation) # remove punctuation
  corpus <- tm_map(corpus, removeNumbers) # remove numbers
  corpus <- tm_map(corpus, stripWhitespace) # get rid of extra spaces
  corpus <- tm_map(corpus, removeWords, profanity) # get rid of profanity
  corpus <- tm_map(corpus, removeWords, "/|@|\\|") # remove special charaters
}

stemming <- function(x){
  corpus <- tm_map(x, stemDocument, language = "english") # create stem document
  corpus <- tm_map(corpus, PlainTextDocument) # complete the words in plain text
}

tokenizator(corpusExampleDoc)
stemming(corpusExampleDoc)

tdm <- TermDocumentMatrix(corpusExampleDoc)
#dtm <- DocumentTermMatrix(corpusExampleDoc)
gc()

# Fourgram
FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
myFourTdm <- TermDocumentMatrix(corpusExampleDoc, control = list(tokenize = FourgramTokenizer))
myFourTdm <- removeSparseTerms(myFourTdm, 0.66)
gc()
# Trigrams 
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
myTriTdm <- TermDocumentMatrix(corpusExampleDoc, control = list(tokenize = TrigramTokenizer))
myTriTdm <- removeSparseTerms(myTriTdm, 0.65)
gc()
# Bigrams
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
myBiTdm <- TermDocumentMatrix(corpusExampleDoc, control = list(tokenize = BigramTokenizer))
myBiTdm <- removeSparseTerms(myBiTdm, 0.65)
gc()
#onegram
myonetdm <- TermDocumentMatrix(corpusExampleDoc, control = list(minWordLength = 2))
gc()

# First step is to convert the four TDM objects to dataframes
dt.four.raw <- as.data.frame(inspect(myFourTdm))
dt.tri.raw <- as.data.frame(inspect(myTriTdm))
dt.bi.raw <- as.data.frame(inspect(myBiTdm))
dt.one.raw <- as.data.frame(inspect(myonetdm))

# The next one is the function to prepare the dataframes for fast looking # # of terms.
prepare.data <- function(dt, number) {
  look.base <- rep(NA, nrow(dt))
  outputa <- rep(NA, nrow(dt))
  suma <- rep(NA, nrow(dt))
  for (i in 1:nrow(dt)) {
    a <- strsplit(row.names(dt[i,]), split = " ")
    a <- a[[1]]
    base <- a[1]
    if (number > 2) {
      base <- paste(base, a[2], sep = " ", collapse = "")
    }
    if (number > 3) {
      base <- paste(base, a[3], sep = " ", collapse = "")
    }
    look.base[i] <- base
    outputa[i] <- a[number] 
    suma[i] <- dt[i,1] + dt[i,2] + dt[i,3]
  }
  ou.raw <- data.frame(
    look = look.base ,
    output = outputa ,
    sum = suma)
  ou <- aggregate(ou.raw["sum"], by = ou.raw["look"], FUN=max)
  a <- rep(NA, nrow(ou) )
  for (i in 1:nrow(ou) ) {
    a[i] <- as.character(ou.raw[(ou.raw$look == ou[i,1] & ou.raw$sum == ou[i,2]), 2])
  }
  ou$output <- a
  ou
}

dt.four <- prepare.data(dt.four.raw, 4)
dt.tri <- prepare.data(dt.tri.raw, 3)
dt.bi <- prepare.data(dt.bi.raw, 2)
dt.one <- prepare.data(dt.one.raw, 1)

save(dt.one, dt.bi, dt.tri, dt.four, file = "C:\\Users\\David\\Documents\\GitHub\\CapstonFinalReport\\ngram.RData")


next.word <- function(text) {
  #First split the sentence into words
  words_list <- strsplit(text, split = " ")
  # Select the last three words to make prediction
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
 
#next.word("enter something")




