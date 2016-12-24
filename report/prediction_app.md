NLP Word Prediction Application
========================================================
author: Brian Gulbis
date: December 23, 2016
autosize: true
font-family: 'Arial'
transition: fade
css: custom.css

About the Application
========================================================
type: exclaim

* This application takes a phrase and predicts the next word
* Users may choose from two different data sources for the predictions
    - [HC Corpora](http://www.corpora.heliohost.org/aboutcorpus.html) data set
    - [Microsoft Web Language Model](https://www.microsoft.com/cognitive-services/en-us/web-language-model-api)
* Ultimately, this could be used as the foundation for a text prediction keyboard

Prediction Algorithm for HC Corpora
========================================================
type: exclaim

* Create 1-, 2-, and 3-gram tokens from the blogs, news, and Twitter data sets
    - Profane words are removed using Google's [list of bad words](https://gist.github.com/jamiew/1112488)
    - Numbers and punctuation are removed
    - Stop words are included and may optionally be removed from the prediction results
* Calculate Good-Turing discount for each order token to allow for unseen words
* Calculate the probability for a word using Katz back-off method

About the Data Sources
========================================================
type: exclaim

There are two data sources which can be used:

1. [HC Corpora](http://www.corpora.heliohost.org/aboutcorpus.html) data set
    * Obtained from scraping websites with a web crawler
    * Divided into 3 categories by type of website: blogs, news, and Twitter
    * Accuracy on a test set is around 12%
1. [Microsoft Web Language Model](https://www.microsoft.com/cognitive-services/en-us/web-language-model-api)
    * Utilizes a large corpus obtained from websites
    * Contains 1- to 5-gram tokens

How to Use the Application
========================================================
type: exclaim

1. Go to the [Prediction Application](https://bgulbis.shinyapps.io/Prediction_App/) on Shinyapps.io
1. Enter a phrase or sentence in the text box
1. Choose your data source for prediction
1. Choose whether to include common stop words in the results
    * Not available when using the Microsoft Web Language Model
1. Select the number of predictions to return
1. Click the Predict button and get your predictions!
