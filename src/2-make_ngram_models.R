# make n-gram models

library(tidyverse)
library(stringr)
library(quanteda)
library(text2vec)
library(doParallel)
registerDoParallel()

profanity <- read_lines("data/external/profanity.txt") 

my_tokenizer <- function(x) {
    quanteda::tokenize(
        x,
        removeNumbers = TRUE, 
        removePunct = TRUE, 
        removeSymbols = TRUE, 
        removeTwitter = TRUE, 
        removeURL = TRUE
    )
}

make_dtm_count <- function(x, min_ngram = 1L, max_ngram = 3L, term_min = 3L) {
    it <- itoken(x, toLower, my_tokenizer)
    
    vocab <- create_vocabulary(
        it, 
        ngram = c(min_ngram, max_ngram), 
        stopwords = profanity
        # stopwords = c(stopwords("english"), profanity)
    ) %>%
        prune_vocabulary(term_count_min = term_min)
    
    vect <- vocab_vectorizer(vocab)

    dtm <- create_dtm(it, vect)
}

make_token_files <- function(x, y, uni_min = 5L, bi_min = 3L, tri_min = 2L) {
    sentences <- read_rds(x) %>%
        tokenize("sentence", simplify = TRUE, verbose = TRUE) 
    
    make_dtm_count(sentences, 1L, 1L, uni_min) %>%
        colSums() %>%
        write_rds(paste0("data/tidy/tokens_", y, "1.Rds"), compress = "gz")
    
    make_dtm_count(sentences, 2L, 2L, bi_min) %>%
        colSums() %>%
        write_rds(paste0("data/tidy/tokens_", y, "2.Rds"), compress = "gz")

    make_dtm_count(sentences, 3L, 3L, tri_min) %>%
        colSums() %>%
        write_rds(paste0("data/tidy/tokens_", y, "3.Rds"), compress = "gz")
}

make_token_files("data/tidy/train_blogs.Rds", "blogs")
make_token_files("data/tidy/train_news.Rds", "news")
make_token_files("data/tidy/train_tweets.Rds", "tweets")
