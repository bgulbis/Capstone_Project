
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

size = 25000
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

# rm(blogs, news, tweets)

mod <- train_word2vec("data/tidy/test_word2vec.txt", "data/tidy/vectors.bin", threads = 3, force = TRUE)
mod <- read.vectors("data/tidy/vectors.bin")
test <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of" %>%
    # word(-10, -1) %>%
    tokenize("word", removePunct = TRUE, simplify = TRUE) %>%
    toLower()

nearest_to(mod, mod[[c("a", "case", "of")]])
nearest_to(mod, mod[[test]])
nearest_to(mod, mod[["wine"]])

word2phrase("data/tidy/test_word2vec.txt", "data/tidy/word2phrase.txt", force = TRUE)
word2phrase("data/tidy/word2phrase.txt", "data/tidy/word2phrase2.txt", force = TRUE)
mod2 <- train_word2vec("data/tidy/word2phrase2.txt", "data/tidy/phrases.bin", threads = 3, force = TRUE)

nearest_to(mod2, mod2[["beer"]])

nearest_to(mod,mod[["guy"]] - mod[["man"]] + mod[["woman"]])

library(text2vec) 

set.seed(77123)
blogs2<- read_rds("data/tidy/train_blogs.Rds") %>%
    corpus() %>%
    sample(size = 1000)

    # as_tibble() %>%
    # sample_frac(0.01)

corp <- blogs2 %>%
    tokenize("sentence", simplify = TRUE) %>%
    tokenize(
        removeNumbers = TRUE, 
        removePunct = TRUE, 
        removeSymbols = TRUE, 
        removeTwitter = TRUE, 
        removeURL = TRUE
    ) %>%
    toLower() %>%
    removeFeatures(profanity) 

# extract last word from sentence
sent <- corp %>%
    map(~ str_c(.x[1:length(.x) - 1], collapse = " ")) %>%
    unlist()

last <- corp %>%
    map(~ .x[length(.x)]) %>%
    # map(str_c, collapse = " ") %>%
    unlist()

blogs_tbl <- tibble(sentence = sent, word = last) 


# size = 1000
# words2 <- c(blogs, news, tweets) %>%
#     sample(size = size) %>%
#     tokenize("sentence", simplify = TRUE)

it_train <- itoken(corp)

my_tokenizer <- function(x) {
    quanteda::tokenize(x,
             removeNumbers = TRUE, 
             removePunct = TRUE, 
             removeSymbols = TRUE, 
             removeTwitter = TRUE, 
             removeURL = TRUE
    )
}

it <- itoken(words, toLower, my_tokenizer)

vocab1 <- create_vocabulary(it, stopwords = c(stopwords("english"), profanity)) 

vocab2 <- create_vocabulary(it, ngram = c(2L, 2L), stopwords = c(stopwords("english"), profanity)) 
    # prune_vocabulary(term_count_min = 5L)

vocab12 <- create_vocabulary(it, ngram = c(1L, 3L), stopwords = c(stopwords("english"), profanity)) 

vocab3 <- create_vocabulary(it, ngram = c(3L, 3L), stopwords = c(stopwords("english"), profanity)) 

vocab4 <- create_vocabulary(it, ngram = c(4L, 4L), stopwords = c(stopwords("english"), profanity)) 

vect1 <- vocab_vectorizer(vocab1)
vect2 <- vocab_vectorizer(vocab2)
vect12 <- vocab_vectorizer(vocab12)
vect3 <- vocab_vectorizer(vocab3)

dtm1 <- create_dtm(it, vect1)
dtm2 <- create_dtm(it, vect2)
dtm12 <- create_dtm(it, vect12)
dtm3 <- create_dtm(it_train, vect3)

tfidf_mod <- TfIdf$new()

tfidf_mod$fit(dtm1)
tfidf1 <- tfidf_mod$transform(dtm1)

tfidf_mod$fit(dtm2)
tfidf2 <- tfidf_mod$transform(dtm2)

tfidf_mod$fit(dtm3)
tfidf3 <- tfidf_mod$transform(dtm3)

cs1 <- colSums(tfidf1) %>% 
    as_tibble() %>%
    rownames_to_column()

cs2 <- colSums(tfidf2) %>% 
    as_tibble() %>%
    rownames_to_column()

cs3 <- colSums(tfidf3) %>% 
    as_tibble() %>%
    rownames_to_column()

cs1a <- colSums(dtm1) %>% 
    as_tibble() %>%
    rownames_to_column()

cs2a <- colSums(dtm2) %>% 
    as_tibble() %>%
    rownames_to_column()

cs12a <- colSums(dtm12) %>% 
    as_tibble() %>%
    rownames_to_column()

cs3a <- colSums(dtm3) %>% 
    as_tibble() %>%
    rownames_to_column()



cs2 <- colSums(dtm_train)

write_rds(vocab, "data/tidy/t2v_vocab.Rds")
write_rds(dtm_train, "data/tidy/t2v_dtm.Rds")

rm(dtm_train, tfidf_dtm)
# GloVe

vocab2 <- create_vocabulary(it_train) %>%
    prune_vocabulary(term_count_min = 5L)
vect2 <- vocab_vectorizer(vocab2, skip_grams_window = 5L)
tcm_train <- create_tcm(it_train, vect2)

glv <- GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
glv$fit(tcm_train, n_iter = 10)

word_vects <- glv$get_word_vectors()

# "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
test <- word_vects["beer", , drop = FALSE] 
cos_sim = sim2(x = word_vects, y = test, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)
# binary_dtm <- transform_binary(dtm_train)
