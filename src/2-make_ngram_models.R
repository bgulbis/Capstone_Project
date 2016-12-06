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

calc_discount <- function(r, m, n) {
    if_else(r > 0 & r <= 5, ((r + 1) / r) * (n / m), 1) 
}

calc_prob_remain <- function(disc, mle) {
    1 - sum(disc * mle)
}

size = 1000

blogs <- read_rds("data/tidy/train_blogs.Rds") %>%
    sample(size) %>%
    tokenize("sentence", simplify = TRUE, verbose = TRUE)

news <- read_rds("data/tidy/train_news.Rds") %>%
    sample(size) %>%
    tokenize("sentence", simplify = TRUE, verbose = TRUE)

# tweets <- read_rds("data/tidy/train_tweets.Rds") %>%
#     sample(size) %>%
#     tokenize("sentence", simplify = TRUE, verbose = TRUE)

# corp <- c(blogs, news, tweets)
corp <- c(blogs, news)

# rm(blogs, news, tweets)

it <- itoken(corp, toLower, my_tokenizer)

vocab <- create_vocabulary(
    it, 
    ngram = c(1L, 3L), 
    stopwords = profanity
    # stopwords = c(stopwords("english"), profanity)
) 

prune <- prune_vocabulary(vocab, doc_proportion_min = 0.001, doc_proportion_max = 0.975)

vect <- vocab_vectorizer(prune)

dtm <- create_dtm(it, vect)

weight <- TfIdf$new()
dtm2 <- weight$fit_transform(dtm)

identical(dtm, dtm2)

count_dtm <- colSums(dtm) %>% 
    as_tibble() %>%
    rownames_to_column("ngram") %>%
    mutate(n = str_count(ngram, "_") + 1) 

count_dtm2 <- colMeans(dtm2) %>% 
    as_tibble() %>%
    rownames_to_column("ngram") %>%
    mutate(n = str_count(ngram, "_") + 1) 


words_1gram <- count_dtm %>%
    filter(n == 1) %>%
    mutate(mle1 = value / sum(value)) %>%
    select(word1 = ngram, count1 = value, mle1)

words_2gram <- count_dtm %>%
    filter(n == 2) %>%
    separate(ngram, c("word1", "word2"), sep = "_") %>%
    left_join(words_1gram, by = "word1") %>%
    mutate(mle2 = value / count1) %>%
    select(word1, word2, count2 = value, mle2)

words_3gram <- count_dtm %>%
    filter(n == 3) %>%
    separate(ngram, c("word1", "word2", "word3"), sep = "_") %>%
    left_join(words_2gram, by = c("word1", "word2")) %>%
    mutate(mle3 = value / count2) %>%
    select(word1, word2, word3, count3 = value, mle3)

# Good-Turing discount ---------------------------------
gt_1gram <- table(words_1gram$count1) %>%
    as_tibble() %>%
    rename(uni = n) 

gt_2gram <- table(words_2gram$count2) %>%
    as_tibble() %>%
    rename(bi = n) 

gt_3gram <- table(words_3gram$count3) %>%
    as_tibble() %>%
    rename(tri = n) 

gt_freq <- full_join(gt_1gram, gt_2gram, by = "Var1") %>%
    full_join(gt_3gram, by = "Var1") %>%
    rename(count = Var1) %>%
    dmap_at("count", as.integer) %>%
    arrange(count) %>%
    mutate(uni_next = if_else(lead(count) == count + 1, lead(uni), 0L),
           bi_next = if_else(lead(count) == count + 1, lead(bi), 0L),
           tri_next = if_else(lead(count) == count + 1, lead(tri), 0L)) %>%
    dmap(~ coalesce(.x, 0L)) 

pred_1gram_gt <- words_1gram %>%
    left_join(gt_freq, by = c("count1" = "count")) %>%
    mutate(discount = calc_discount(count1, uni, uni_next)) %>%
    group_by(word1) %>%
    mutate(remain = calc_prob_remain(discount, mle1))

pred_2gram_gt <- words_2gram %>%
    left_join(gt_freq, by = c("count2" = "count")) %>%
    mutate(discount = calc_discount(count2, bi, bi_next)) %>%
    group_by(word1) %>%
    mutate(remain = calc_prob_remain(discount, mle2))

pred_3gram_gt <- words_3gram %>%
    left_join(gt_freq, by = c("count3" = "count")) %>%
    mutate(discount = calc_discount(count3, tri, tri_next)) %>%
    group_by(word1, word2) %>%
    mutate(remain = calc_prob_remain(discount, mle3))

write_rds(pred_1gram_gt, "data/final/pred_1gram_gt.Rds")
write_rds(pred_2gram_gt, "data/final/pred_2gram_gt.Rds")
write_rds(pred_3gram_gt, "data/final/pred_3gram_gt.Rds")
