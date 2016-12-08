# probability tables

library(tidyverse)
library(stringr)

calc_mle <- function(x, y = NULL, ng = 1L) {
    sep_cols <- map_chr(1:ng, ~paste0("word", .x))
    join_by <- map_chr(1:(ng - 1), ~paste0("word", .x))
    
    if (ng == 1L) {
        m <- list(~value / sum(value))
    } else {
        m <- list(lazyeval::interp(~ value / cnt, cnt = as.name(paste0("count", ng - 1))))
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

calc_good_turing <- function(x, y, z) {
    gt_1gram <- table(x$count1) %>%
        as_tibble() %>%
        rename(uni = n) 
    
    gt_2gram <- table(y$count2) %>%
        as_tibble() %>%
        rename(bi = n) 
    
    gt_3gram <- table(z$count3) %>%
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
}

calc_discount <- function(r, m, n) {
    if_else(r > 0 & r <= 5, ((r + 1) / r) * (n / m), 1) 
}

calc_prob_remain <- function(disc, mle) {
    1 - sum(disc * mle)
}

make_prob_tables <- function(x, corp) {
    words_1gram <- calc_mle(x)
    words_2gram <- calc_mle(x, words_1gram, 2L)
    words_3gram <- calc_mle(x, words_2gram, 3L)
    gt_freq <- calc_good_turing(words_1gram, words_2gram, words_3gram)
    
    pred_3gram <- words_3gram %>%
        left_join(gt_freq, by = c("count3" = "count")) %>%
        mutate(discount = calc_discount(count3, tri, tri_next)) %>%
        group_by(word1, word2) %>%
        mutate(remain = calc_prob_remain(discount, mle3))
    
    pred_2gram <- words_2gram %>%
        left_join(gt_freq, by = c("count2" = "count")) %>%
        mutate(discount = calc_discount(count2, bi, bi_next)) %>%
        group_by(word1) %>%
        mutate(remain = calc_prob_remain(discount, mle2))
    
    pred_1gram <- words_1gram %>%
        left_join(gt_freq, by = c("count1" = "count")) %>%
        mutate(discount = calc_discount(count1, uni, uni_next)) %>%
        group_by(word1) %>%
        mutate(remain = calc_prob_remain(discount, mle1))
    
    write_rds(pred_1gram, paste0("data/final/pred_1gram_", corp, ".Rds"))
    write_rds(pred_2gram, paste0("data/final/pred_2gram_", corp, ".Rds"))
    write_rds(pred_3gram, paste0("data/final/pred_3gram_", corp, ".Rds"))
}

read_rds("data/final/tokens_blogs.Rds") %>%
    make_prob_tables("blogs")

read_rds("data/final/tokens_news.Rds") %>%
    make_prob_tables("news")

read_rds("data/final/tokens_tweets.Rds") %>%
    make_prob_tables("tweets")
