# make intial prediction model

library(tidyverse)
library(stringr)
library(tidytext)
library(caret)

make_ngram <- function(df) {
    df %>%
        unnest_tokens(word, value, token = "ngrams", n = 3, collapse = FALSE) %>%
        separate(word, c("first", "second", "third"), sep = " ") %>%
        anti_join(profanity, by = c("first" = "value")) %>%
        anti_join(profanity, by = c("second" = "value")) %>%
        anti_join(profanity, by = c("third" = "value")) %>%
        filter(!str_detect(word, "[[:digit:]]"))
}



profanity <- read_lines("data/external/profanity.txt") %>%
    as_tibble()

words_3gram <- list.files("data/tidy", "sample", full.names = TRUE) %>%
    map_df(read_rds) %>%
    unnest_tokens(word, value, token = "ngrams", n = n, collapse = FALSE) %>%
    separate(word, c("first", "second", "third"), sep = " ") %>%
    anti_join(profanity, by = c("first" = "value")) %>%
    anti_join(profanity, by = c("second" = "value")) %>%
    anti_join(profanity, by = c("third" = "value")) %>%
    filter(x, !str_detect(word, "[[:digit:]]"))

words_part <- createDataPartition(words_3gram$third, p = 0.8, list = FALSE)
train_words <- words_3gram[words_part, ]
test

library(tidytext)
library(stringr)

make_tokens <- function(df) {
    df %>%
        unnest_tokens(word, value) %>%
        anti_join(profanity, by = c("word" = "value")) %>%
        filter(!str_detect(word, "[[:digit:]]"))
}

filter_instances <- function(df, perc, val) {
    df %>%
        filter(run_total <= perc * val)
}

make_ngram <- function(df, n) {
    sep_cols <- c("first", "second")
    if (n == 3) sep_cols <- c(sep_cols, "third")
    
    x <- df %>%
        unnest_tokens(word, value, token = "ngrams", n = n, collapse = FALSE) %>%
        separate(word, sep_cols, sep = " ", remove = FALSE) %>%
        anti_join(profanity, by = c("first" = "value")) %>%
        anti_join(profanity, by = c("second" = "value")) 
    
    if (n == 3) x <- anti_join(x, profanity, by = c("third" = "value"))
    
    filter(x, !str_detect(word, "[[:digit:]]"))
}

sep2 <- c("first", "second")
sep3 <- c(sep2, "third")

blogs_2gram <- blogs_sample %>%
    make_ngram(2) %>%
    count(word, first, second, sort = TRUE)

blogs_3gram <- blogs_sample %>%
    make_ngram(3) %>%
    count(word, first, second, third, sort = TRUE) 

news_2gram <- news_sample %>%
    make_ngram(2) %>%
    count(word, first, second, sort = TRUE)

news_3gram <- news_sample %>%
    make_ngram(3) %>%
    count(word, first, second, third, sort = TRUE)

tweets_2gram <- tweets_sample %>%
    make_ngram(2) %>%
    count(word, first, second, sort = TRUE) 

tweets_3gram <- tweets_sample %>%
    make_ngram(3) %>%
    count(word, first, second, third, sort = TRUE)

dirr::save_rds("data/tidy", "sample|[2-3]gram")
