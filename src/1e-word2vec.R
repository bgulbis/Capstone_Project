
library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(wordVectors)

blogs <- read_rds("data/tidy/train_blogs.Rds") %>%
    corpus()

# set.seed(77123)
# blogs2 <- read_rds("data/tidy/train_blogs.Rds") %>%
#     as_tibble() %>%
#     sample_frac(0.1) %>%
#     unnest_tokens(phrase, value, "sentences", strip_punctuation = TRUE)
# 
# write_lines(blogs2, "data/tidy/test_word2vec.txt")

news <- read_rds("data/tidy/train_news.Rds") %>%
    corpus() 

tweets <- read_rds("data/tidy/train_tweets.Rds") %>%
    corpus() 

profanity <- read_lines("data/external/profanity.txt") 

size = 10000
set.seed(77123)
words <- c(blogs, news, tweets) %>%
    sample(size = size) %>%
    tokenize("sentence", simplify = TRUE) %>%
    tokenize(removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, removeTwitter = TRUE, removeURL = TRUE) %>%
    toLower() %>%
    removeFeatures(profanity) %>%
    map(str_c, collapse = " ") %>%
    unlist()

write_lines(words, "data/tidy/test_word2vec.txt")

rm(blogs, news, tweets)

mod <- train_word2vec("data/tidy/test_word2vec.txt", "data/tidy/vectors.bin", threads = 6, cbow = 1, force = TRUE)
mod <- read.vectors("data/tidy/vectors.bin")
test <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of" %>%
    # word(-10, -1) %>%
    tokenize("word", removePunct = TRUE, simplify = TRUE) %>%
    toLower()

nearest_to(mod, mod[[test]])

# x <- word2phrase("data/tidy/test_word2vec.txt", "data/tidy/word2phrase.txt")

library(text2vec) 
it_train <- itoken(words, tokenizer = word_tokenizer)
vocab <- create_vocabulary(it_train) %>%
    prune_vocabulary(term_count_min = 5L)
vectorizer <- vocab_vectorizer(vocab, skip_grams_window = 5L)
dtm_train <- create_dtm(it_train, vectorizer)
tcm_train <- create_tcm(it_train, vectorizer)

glv <- GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
glv$fit(tcm_train, n_iter = 10)

vect <- glv$get_word_vectors()

library(glmnet)
data("movie_review")
