# Initial prediction model

library(tidyverse)
library(quanteda)

library(stringr)
library(tm)
library(tidytext)
library(caret)

profanity <- read_lines("data/external/profanity.txt") 

make_ngram <- function(df) {
    df %>%
        dmap(iconv, from = "latin1", to = "ASCII", sub = "") %>%
        unnest_tokens(word, value, token = "ngrams", n = 3, collapse = FALSE) %>%
        separate(word, c("first", "second", "third"), sep = " ", remove = FALSE) %>%
        anti_join(profanity, by = c("first" = "value")) %>%
        anti_join(profanity, by = c("second" = "value")) %>%
        anti_join(profanity, by = c("third" = "value")) %>%
        filter(!str_detect(word, "[[:digit:]]"))
}

frac <- 0.01

set.seed(77123)
blogs <- read_lines("data/raw/en_US.blogs.txt.gz") %>%
    as_tibble() %>%
    sample_frac(frac) 

blogs_corpus <- Corpus(VectorSource(blogs$value)) %>%
    tm_map(content_transformer(function(y) iconv(y, from = "latin1", to = "ASCII", sub = " "))) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, profanity) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace) %>%
    tidy() %>%
    unnest_tokens(phrase, text, token = "ngrams", n = 3, collapse = FALSE) %>%
    separate(phrase, c("first", "second", "third"), sep = " ")
    
blogs_tidy <- tidy(blogs_corpus)

    as_tibble() %>%
    sample_frac(frac) %>%
    make_ngram()

set.seed(77123)
news <- read_lines("data/raw/en_US.news.txt.gz") %>%
    as_tibble() %>%
    sample_frac(frac) %>%
    make_ngram()

set.seed(77123)
tweets <- read_lines("data/raw/en_US.twitter.txt.gz") %>%
    as_tibble() %>%
    sample_frac(frac) %>%
    make_ngram()

words <- bind_rows(blogs, news) %>%
    bind_rows(tweets)

pval = 0.7

set.seed(77123)
words_part <- createDataPartition(blogs_corpus$third, p = pval, list = FALSE)
words_temp <- blogs_corpus[words_part, ]
words_test <- blogs_corpus[-words_part, ]

set.seed(77123)
words_part2 <- createDataPartition(words_temp$third, p = pval, list = FALSE)
words_train <- words_temp[words_part2, ]
words_valid <- words_temp[-words_part2, ]

write_rds(words_train, "data/tidy/words_train.Rds")
write_rds(words_valid, "data/tidy/words_valid.Rds")
write_rds(words_test, "data/tidy/words_test.Rds")
