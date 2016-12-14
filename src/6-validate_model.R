# validation

library(tidyverse)
library(stringr)
library(quanteda)

drop_last_word <- function(x) {
    # extract last word from sentence
    sent <- x %>%
        map(~ str_c(.x[1:length(.x) - 1], collapse = " ")) 
}

get_last_word <- function(x) {
    last <- x %>%
        map(~ .x[length(.x)]) 
        # map(str_c, collapse = " ") 

    # tibble(sentence = sent, word = last)
}

valid_blogs <- read_rds("data/tidy/valid_blogs.Rds") %>%
    tokenize("sentence", simplify = TRUE, verbose = TRUE) %>%
    tokenize(removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, 
             removeTwitter = TRUE, removeURL = TRUE)

valid_news <- read_rds("data/tidy/valid_news.Rds")
valid_tweets <- read_rds("data/tidy/valid_tweets.Rds")

blogs_sent <- drop_last_word(valid_blogs)
blogs_last <- get_last_word(valid_blogs)

