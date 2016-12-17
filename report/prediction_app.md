NLP Word Prediction Application
========================================================
author: Brian Gulbis
date: December 17, 2016
autosize: true
font-family: 'Arial'
transition: fade

About the Application
========================================================

* This application takes a phrase and predicts the next word
* Users may choose from two different data sources for the predictions
    - [HC Corpora](http://www.corpora.heliohost.org/aboutcorpus.html) data set
    - [Microsoft Web Language Model](https://www.microsoft.com/cognitive-services/en-us/web-language-model-api)
* Ultimately, this could be used as the foundation for a text prediction keyboard

Prediction Algorithm
========================================================

* Take the data set and create 1-, 2-, and 3-gram tokens
    - Profane words are removed using Google's [list of bad words](https://gist.github.com/jamiew/1112488)
    - Numbers and punctuation are removed
    - Stop words are included
* Calculate Good-Turing discount for each order token 
* Calculate the probability for a word using Katz Backoff

Data Sources
========================================================

There are two data sources which can be used:

1. HC Corpora data set
    * Obtained from scraping websites with a web crawler
    * Divided into 3 categories by type of website: blogs, news, and Twitter
1. Microsoft Web Language Model
    * Utilizes a large corpus obtained from websites
    * Contains 1- to 5-gram tokens

How to Use the Application
========================================================
incremental: true

1. Go to the [Prediction Application](https://bgulbis.shinyapps.io/Prediction_App/) on Shinyapps.io
1. Enter a phrase or sentence in the text box
1. Choose your data source for prediction
1. Choose whether to include common stop words in the results
    * Does not apply when using the Microsoft Web Language Model
1. Select the number of predictions to return
1. Click the Predict button and get your predictions!
