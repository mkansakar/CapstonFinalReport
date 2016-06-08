Predict Next Word A Capston Final Project:
========================================================
author: David Smerchek  
date: 06_07_2016
autosize: true
### Last Project for the John Hopkins University Data Science Specialization Capstone and in cooperation with SwiftKey. 

Predictive Application
========================================================
## Objective

### The goal of this exercise is to create a product to highlight the prediction algorithm that you have built and to provide an interface that can be accessed by others by creating 

1. A Shiny app that takes as input a phrase (multiple words) in a text box input and outputs a prediction of the next word.
2. This slide deck consisting of no more than 5 slides created with R Studio Presenter, pitching your algorithm and app

Processes
=======================================================
### Initialy read in the three files provided by swiftkey
### Next took a sample of each file 
### Created a Corpus Document from the three files, to convert all to lower case, remove stop words, profanity, numbers, puntuation,and special characters. Then creates a stem document to process into a Term Document Matrix(tdm)
### Create the 4,3,2,1 NGrams from the tdm
### Create a ngram datafram of all of the NGrams into a save file to be able to process faster in the application
### Created function to find the next word from the entered phrase.

Usage Of The Application
=======================================================


Additional Information
=======================================================
- The next word prediction app is hosted on shinyapps.io: 
- GitHub repo: 
- This pitch deck:
