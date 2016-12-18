
library(tidyverse)
library(stringr)
library(data.table)
library(feather)

x <- list.files("data/tidy", "tokens_blogs", full.names = TRUE)
nm <- list.files("data/tidy", "tokens_blogs")
nm <- str_replace_all(nm, ".Rds", "")
files <- map(x, read_rds)
names(files) <- nm
list2env(files, .GlobalEnv)
rm(files)

test <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

library(microbenchmark)
microbenchmark(x <- predict_words(test), times = 10)

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
write_feather(gt, "data/final/gt_table.feather")

calc_discount <- function(r, m, n) {
    if_else(r > 0 & r <= 5, ((r + 1) / r) * (n / m), 1) 
}

x <- data.table(word1 = names(tokens_blogs1), count1 = tokens_blogs1)

y <- data.table(words = names(tokens_blogs2), count2 = tokens_blogs2)
y[, c("word1", "word2") := tstrsplit(words, "_", fixed = TRUE)]
y[, words := NULL]

z <- data.table(words = names(tokens_blogs3), count3 = tokens_blogs3)
z[, c("word1", "word2", "word3") := tstrsplit(words, "_", fixed = TRUE)]
z[, words := NULL]

write_feather(x, "data/final/tokens_blogs1.feather")
write_feather(y, "data/final/tokens_blogs2.feather")
write_feather(z, "data/final/tokens_blogs3.feather")

predict_words <- function(phrase) {
    x <- read_feather("data/final/tokens_blogs1.feather") %>% as.data.table()
    y <- read_feather("data/final/tokens_blogs2.feather") %>% as.data.table()
    z <- read_feather("data/final/tokens_blogs3.feather") %>% as.data.table()
    gt <- read_feather("data/final/gt_table.feather") %>% 
        as.data.table() %>%
        setkey(count)
    
    words <- str_to_lower(phrase) %>%
        word(-2, -1) %>%
        str_split(" ") %>%
        unlist()
    
    bigram_count <- y[word1 == words[1] & word2 == words[2], sum(count2)]
    unigram_count <- x[word1 == words[2], sum(count1)]
    
    obs_trigram <- z[word1 == words[1] & word2 == words[2], ]
    setkey(obs_trigram, count3)

    qbo_obs_tri <- obs_trigram[gt, nomatch = 0][, discount := calc_discount(count3, tri, tri_next)][, .(word1, word2, word3, qbo = (count3 - discount) / bigram_count)]
    
    unobs_words <- x[!(word1 %in% qbo_obs_tri$word3), ]
    
    obs_bigram <- y[word1 == words[2], ]
    setkey(obs_bigram, count2)

    qbo_obs_bi <- obs_bigram[gt, nomatch = 0][, discount := calc_discount(count2, bi, bi_next)][, .(word1, word2, count2, qbo2 = (count2 - discount) / unigram_count)]
    
    alpha2 <- 1 - sum(qbo_obs_bi$qbo2)
    
    qbo_unobs_bi <- unobs_words[!(word1 %in% qbo_obs_bi$word2), ][, .(word1 = words[2], word2 = word1, count2 = count1, qbo2 = alpha2 * (count1 / sum(count1)))]
    
    qbo_bigram <- rbind(qbo_obs_bi, qbo_unobs_bi)
    setkey(qbo_bigram, word1, word2)
    
    alpha3 <- 1 - sum(qbo_obs_tri$qbo)
    
    unobs_trigram <- unobs_words[, .(word1 = words[1], word2 = words[2], word3 = word1, count1)]
    setkey(unobs_trigram, word2, word3)

    qbo_unobs_tri <- unobs_trigram[qbo_bigram, nomatch = 0][, .(word1, word2, word3, qbo = alpha3 * (qbo2 / sum(qbo2)))]
    qbo_trig <- rbind(qbo_obs_tri, qbo_unobs_tri)[order(-qbo)]
}
