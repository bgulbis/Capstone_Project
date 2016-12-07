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

make_dtm_count <- function(x) {
    it <- itoken(x, toLower, my_tokenizer)
    
    vocab <- create_vocabulary(
        it, 
        ngram = c(1L, 3L), 
        stopwords = profanity
        # stopwords = c(stopwords("english"), profanity)
    ) 
    
    vect <- vocab_vectorizer(vocab)
    # vect <- hash_vectorizer(vocab)
    
    dtm <- create_dtm(it, vect)
    
    weight <- TfIdf$new()
    dtm2 <- weight$fit_transform(dtm)
    
    count_dtm <- ceiling(colSums(dtm2)) %>% 
        as_tibble() %>%
        rownames_to_column("ngram") %>%
        mutate(n = str_count(ngram, "_") + 1) 
}

calc_discount <- function(r, m, n) {
    if_else(r > 0 & r <= 5, ((r + 1) / r) * (n / m), 1) 
}

calc_prob_remain <- function(disc, mle) {
    1 - sum(disc * mle)
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


calc_mle <- function(x, y = NULL, ng = 1L) {
    sep_cols <- map_chr(1:ng, ~paste0("word", .x))
    join_by <- map_chr(1:(ng - 1), ~paste0("word", .x))

    if (ng == 1L) {
        m <- list(~value / sum(value))
    } else if (ng == 2L) {
        m <- list(~value / count1)
    } else if (ng == 3L) {
        m <- list(~value / count2)
    }
    
    x <- filter_(x, .dots = list(~n == ng)) %>%
        separate_("ngram", sep_cols, sep = "_")
    
    if (!is.null(y)) {
        x <- left_join(x, y, by = join_by)
    }
    
    x %>%
        mutate_(.dots = set_names(m, "mle")) %>%
        select_(.dots = c(as.list(sep_cols), list(~value, ~mle))) %>%
        rename_(.dots = set_names(list(~value, ~mle), list(paste0("count", ng), paste0("mle", ng)))) 
}

words_1gram <- calc_mle(tokens_blogs)
words_2gram <- calc_mle(tokens_blogs, words_1gram, 2L)
words_3gram <- calc_mle(tokens_blogs, words_2gram, 3L)


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
