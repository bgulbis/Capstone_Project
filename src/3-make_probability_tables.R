# probability tables

library(tidyverse)
library(stringr)
library(data.table)

calc_mle_dt <- function(uni, bi, tri) {
    x <- as.data.table(uni, TRUE)
    y <- as.data.table(bi, TRUE)
    z <- as.data.table(tri, TRUE)
    
    x[, mle1 := uni / sum(uni)]
    setnames(x, c("rn", "uni"), c("word1", "count1"))
    
    y[, c("word1", "word2") := tstrsplit(rn, "_", fixed = TRUE)]
    y[, freq2 := sum(bi), by = word1]
    y[, mle2 := bi / freq2]
    y[, rn := NULL]
    setnames(y, "bi", "count2")
    
    z[, c("word1", "word2", "word3") := tstrsplit(rn, "_", fixed = TRUE)]
    z[, freq3 := sum(tri), by = c("word1", "word2")]
    z[, mle3 := tri / freq3]
    z[, rn := NULL]
    setnames(z, "tri", "count3")

    list(x, y, z)
}

calc_gt_dt <- function(x) {
    gt_1gram <- table(x[[1]]$count1) %>%
        as_tibble() %>%
        rename(uni = n) 
    
    gt_2gram <- table(x[[2]]$count2) %>%
        as_tibble() %>%
        rename(bi = n) 
    
    gt_3gram <- table(x[[3]]$count3) %>%
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

make_prob_dt <- function(x, y, z, corp) {
    mle <- calc_mle_dt(x, y, z)
    gt_freq <- calc_gt_dt(mle)
    
    pred_3gram <- mle[[3]] %>%
        left_join(gt_freq, by = c("count3" = "count")) %>%
        mutate(discount = calc_discount(count3, tri, tri_next)) %>%
        group_by(word1, word2) %>%
        mutate(remain = calc_prob_remain(discount, mle3)) %>%
        select(word1, word2, word3, mle3, discount, remain)
    
    pred_2gram <- mle[[2]] %>%
        left_join(gt_freq, by = c("count2" = "count")) %>%
        mutate(discount = calc_discount(count2, bi, bi_next)) %>%
        group_by(word1) %>%
        mutate(remain = calc_prob_remain(discount, mle2)) %>%
        select(word1, word2, mle2, discount, remain)
    
    pred_1gram <- mle[[1]] %>%
        left_join(gt_freq, by = c("count1" = "count")) %>%
        mutate(discount = calc_discount(count1, uni, uni_next)) %>%
        group_by(word1) %>%
        mutate(remain = calc_prob_remain(discount, mle1)) %>%
        select(word1, mle1, discount, remain)

    write_rds(pred_1gram, paste0("data/final/pred_1gram_", corp, ".Rds"), compress = "gz")
    write_rds(pred_2gram, paste0("data/final/pred_2gram_", corp, ".Rds"), compress = "gz")
    write_rds(pred_3gram, paste0("data/final/pred_3gram_", corp, ".Rds"), compress = "gz")
    
}

# read in all of the tokens files
x <- list.files("data/tidy", "tokens_", full.names = TRUE)
nm <- list.files("data/tidy", "tokens_")
nm <- str_replace_all(nm, ".Rds", "")
files <- map(x, read_rds)
names(files) <- nm
list2env(files, .GlobalEnv)

make_prob_dt(tokens_blogs1, tokens_blogs2, tokens_blogs3, "blogs")
make_prob_dt(tokens_news1, tokens_news2, tokens_news3, "news")
make_prob_dt(tokens_tweets1, tokens_tweets2, tokens_tweets3, "tweets")
