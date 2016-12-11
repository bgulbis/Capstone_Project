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

make_dtm_count <- function(x, min_ngram = 1L, max_ngram = 3L) {
    it <- itoken(x, toLower, my_tokenizer)
    
    vocab <- create_vocabulary(
        it, 
        ngram = c(min_ngram, max_ngram), 
        stopwords = profanity
        # stopwords = c(stopwords("english"), profanity)
    ) 
    
    vect <- vocab_vectorizer(vocab)
    # vect <- hash_vectorizer(vocab)
    
    dtm <- create_dtm(it, vect)
    
    # weight <- TfIdf$new()
    # dtm2 <- weight$fit_transform(dtm)
    
    # count_dtm <- ceiling(colSums(dtm2)) %>% 
    count_dtm <- colSums(dtm) 
        as_tibble() %>%
        rownames_to_column("ngram") %>%
        mutate(n = str_count(ngram, "_") + 1)
}

size = 50000

set.seed(77123)
tokens_blogs <- read_rds("data/tidy/train_blogs.Rds") %>%
    sample(size) %>%
    tokenize("sentence", simplify = TRUE, verbose = TRUE) %>%
    make_dtm_count()

set.seed(77123)
tokens_news <- read_rds("data/tidy/train_news.Rds") %>%
    sample(size) %>%
    tokenize("sentence", simplify = TRUE, verbose = TRUE) %>%
    make_dtm_count()

set.seed(77123)
tokens_tweets <- read_rds("data/tidy/train_tweets.Rds") %>%
    sample(size) %>%
    tokenize("sentence", simplify = TRUE, verbose = TRUE) %>%
    make_dtm_count()

write_rds(tokens_blogs, "data/final/tokens_blogs.Rds")
write_rds(tokens_news, "data/final/tokens_news.Rds")
write_rds(tokens_tweets, "data/final/tokens_tweets.Rds")
