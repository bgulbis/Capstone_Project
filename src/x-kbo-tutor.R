
library(tidyverse)
library(stringr)
library(data.table)
library(feather)
library(quanteda)
library(text2vec)
library(doParallel)
registerDoParallel()

x <- list.files("data/tidy", "tokens_blogs", full.names = TRUE)
nm <- list.files("data/tidy", "tokens_blogs")
nm <- str_replace_all(nm, ".Rds", "")
files <- map(x, read_rds)
names(files) <- nm
list2env(files, .GlobalEnv)
rm(files)

test <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

words <- str_to_lower(test) %>%
    word(-2, -1) %>%
    str_split(" ") %>%
    unlist()

x <- data.table(word1 = names(tokens_blogs1), count1 = tokens_blogs1)

y <- data.table(words = names(tokens_blogs2), count2 = tokens_blogs2)
y[, c("word1", "word2") := tstrsplit(words, "_", fixed = TRUE)]
y[, words := NULL]

z <- data.table(words = names(tokens_blogs3), count3 = tokens_blogs3)
z[, c("word1", "word2", "word3") := tstrsplit(words, "_", fixed = TRUE)]
z[, words := NULL]

bigram_count <- y[word1 == words[1] & word2 == words[2], sum(count2)]
unigram_count <- x[word1 == words[2], sum(count1)]

calc_gt_dt <- function(x, y , z) {
    gt_1gram <- table(x) %>%
        as_tibble() %>%
        rename(Var1 = x, uni = n) 
    
    gt_2gram <- table(y) %>%
        as_tibble() %>%
        rename(Var1 = y, bi = n) 
    
    gt_3gram <- table(z) %>%
        as_tibble() %>%
        rename(Var1 = z, tri = n) 
    
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

gt <- calc_gt_dt(tokens_blogs1, tokens_blogs2, tokens_blogs3) %>% as.data.table()

calc_discount <- function(r, m, n) {
    if_else(r > 0 & r <= 5, ((r + 1) / r) * (n / m), 1) 
}

calc_prob_remain <- function(disc, mle) {
    1 - sum(disc * mle)
}

obs3 <- z[word1 == words[1] & word2 == words[2], ]
gt3 <- gt[, .(count3 = count, tri, tri_next)]
setkey(obs3, count3)
setkey(gt3, count3)

obs3_gt <- obs3[gt3, nomatch = 0][, discount := calc_discount(count3, tri, tri_next)]
obs3_gt[, qbo3 := (count3 - discount) / bigram_count]

check_obs3_prob_sum <- obs3_gt[, sum(qbo3)]

unobs_words <- x[!(word1 %in% obs3_gt$word3), ]

obs2 <- y[word1 == words[2], ]
gt2 <- gt[, .(count2 = count, bi, bi_next)]
setkey(obs2, count2)
setkey(gt2, count2)

obs_big <- obs2[gt2, nomatch = 0][, discount := calc_discount(count2, bi, bi_next)][, .(word1, word2, count2, qbo2 = (count2 - discount) / unigram_count)]

check_obs2_prob_sum <- sum(obs2_gt$qbo2)

alpha2 <- 1 - sum(obs_big$qbo2)

unobs_big <- unobs_words[!(word1 %in% obs2_gt$word2), ][, .(word1 = words[2], word2 = word1, count2 = count1, qbo2 = alpha2 * (count1 / sum(count1)))]

check_unobs2_prob_sum <- sum(unobs_big$qbo2)

qbo_big <- rbind(obs_big, unobs_big)

alpha3 <- 1 - sum(obs3_gt$qbo3)

unobs_trig <- unobs_words[, .(word1 = words[1], word2 = words[2], word3 = word1, count1)]
setkey(unobs_trig, word2, word3)
setkey(qbo_big, word1, word2)
qbo_trig_unobs <- unobs_trig[qbo_big, nomatch = 0][, .(word1, word2, word3, qbo3 = alpha3 * (qbo2 / sum(qbo2)))]
qbo_trig_obs <- obs3_gt[, .(word1, word2, word3, qbo3)]
qbo_trig <- rbind(qbo_trig_obs, qbo_trig_unobs)[order(-qbo3)]
