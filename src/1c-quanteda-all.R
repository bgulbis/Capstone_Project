
library(tidyverse)
library(quanteda)

# helper functions -------------------------------------

make_ngram <- function(x, n = 1L) {
    col_name <- "phrase"

    if (n == 1L) {
        col_name <- "word1"
        new_col <- "count1"
    } else if (n == 2L) {
        new_col <- "count2"
    } else if (n == 3L) {
        new_col <- "count3"
    }
    
    dfm(x, 
        toLower = TRUE, 
        ignoredFeatures = c(profanity), 
        removeNumbers = TRUE, 
        removePunct = TRUE, 
        removeSeparators = TRUE,
        ngrams = n, 
        concatenator = " "
    ) %>%
        docfreq() %>%
        as_tibble() %>%
        rownames_to_column(col_name) %>%
        rename_(.dots = set_names(list(~value), new_col)) 
}

calc_mle <- function(x, y = NULL, n = 1L) {
    sep_cols <- c("word1", "word2")
    join_by <- "word1"

    if (n == 1L) {
        m <- list(~count1 / sum(count1))
        nm <- "mle1"
    } else if (n == 2L) {
        m <- list(~count2 / count1)
        nm <- "mle2"
    } else if (n == 3L) {
        sep_cols <- c(sep_cols, "word3")
        join_by <- c(join_by, "word2")
        m <- list(~count3 / count2)
        nm <- "mle3"
    }

    if (!is.null(y)) {
        x <- separate(x, "phrase", sep_cols, sep = " ") %>%
            left_join(y, by = join_by)
    }

    mutate_(x, .dots = set_names(m, nm))
}

calc_discount <- function(r, m, n) {
    if_else(r > 0 & r <= 5, ((r + 1) / r) * (n / m), 1) 
}

calc_prob_remain <- function(disc, mle) {
    1 - sum(disc * mle)
}

# read data --------------------------------------------

profanity <- read_lines("data/external/profanity.txt") 

blogs <- read_rds("data/tidy/train_blogs.Rds") %>%
    corpus() %>%
    tokenize("sentence", simplify = TRUE)

news <- read_rds("data/tidy/train_news.Rds") %>%
    corpus() %>%
    tokenize("sentence", simplify = TRUE)

tweets <- read_rds("data/tidy/train_tweets.Rds") %>%
    corpus() %>%
    tokenize("sentence", simplify = TRUE)

words <- c(blogs, news, tweets)
rm(blogs, news, tweets)

# make ngrams ------------------------------------------

words_1gram <- make_ngram(words, 1L) %>%
    calc_mle()

words_2gram <- make_ngram(words, 2L) %>%
    calc_mle(words_1gram, 2L)

words_3gram <- make_ngram(words, 3L) %>%
    calc_mle(words_2gram, 3L)

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

words_1gram_gt <- words_1gram %>%
    left_join(gt_freq, by = c("count1" = "count")) %>%
    mutate(discount = calc_discount(count1, uni, uni_next)) %>%
    group_by(word1) %>%
    mutate(remain = calc_prob_remain(discount, mle1))

words_2gram_gt <- words_2gram %>%
    left_join(gt_freq, by = c("count2" = "count")) %>%
    mutate(discount = calc_discount(count2, bi, bi_next)) %>%
    group_by(word1) %>%
    mutate(remain = calc_prob_remain(discount, mle2))

words_3gram_gt <- words_3gram %>%
    left_join(gt_freq, by = c("count3" = "count")) %>%
    mutate(discount = calc_discount(count3, tri, tri_next)) %>%
    group_by(word1, word2) %>%
    mutate(remain = calc_prob_remain(discount, mle3))
    
write_rds(words_1gram_gt, "data/final/pred_1gram_gt.Rds")
write_rds(words_2gram_gt, "data/final/pred_2gram_gt.Rds")
write_rds(words_3gram_gt, "data/final/pred_3gram_gt.Rds")
