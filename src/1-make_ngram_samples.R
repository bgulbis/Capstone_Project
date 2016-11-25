# Initial prediction model

library(tidyverse)
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

profanity <- read_lines("../data/external/profanity.txt") %>%
    as_tibble()

blogs <- read_lines("../data/raw/en_US.blogs.txt.gz") %>%
    as_tibble() %>%
    mutate(num_char = nchar(value))

news <- read_lines("../data/raw/en_US.news.txt.gz") %>%
    as_tibble() %>%
    mutate(num_char = nchar(value))

tweets <- read_lines("../data/raw/en_US.twitter.txt.gz") %>%
    as_tibble() %>%
    mutate(num_char = nchar(value))

frac <- 0.2

set.seed(77123)
blogs_sample <- blogs %>%
    select(value) %>%
    sample_frac(frac) 

news_sample <- blogs %>%
    select(value) %>%
    sample_frac(frac) 

tweets_sample <- blogs %>%
    select(value) %>%
    sample_frac(frac) 

blogs_2gram <- blogs_sample %>%
    make_ngram(2) %>%
    count(word, sort = TRUE)

blogs_3gram <- blogs_sample %>%
    make_ngram(3) %>%
    count(word, sort = TRUE)

news_2gram <- news_sample %>%
    make_ngram(2) %>%
    count(word, sort = TRUE)

news_3gram <- news_sample %>%
    make_ngram(3) %>%
    count(word, sort = TRUE)

tweets_2gram <- tweets_sample %>%
    make_ngram(2) %>%
    count(word, sort = TRUE)

tweets_3gram <- tweets_sample %>%
    make_ngram(3) %>%
    count(word, sort = TRUE)

