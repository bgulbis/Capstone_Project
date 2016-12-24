
library(tidyverse)
library(data.table)

x <- read_rds("data/final/tokens_all1.Rds")
y <- read_rds("data/final/tokens_all2.Rds")
z <- read_rds("data/final/tokens_all3.Rds")
gt <- read_rds("data/final/discount_table_all.Rds") %>% as.data.table
setkey(gt, count)

predict_words <- function(phrase, return_n = 5, keep_stop = FALSE) {
    require(tidyverse)
    require(stringr)
    require(data.table)
    # require(feather)
    
    calc_discount <- function(r, m, n) {
        if_else(r > 0 & r <= 5, ((r + 1) / r) * (n / m), 1) 
    }
    
    words <- str_to_lower(phrase) %>%
        word(-2, -1) %>%
        str_split(" ") %>%
        unlist()
    
    bigram_count <- y[.(words[1], words[2]), sum(count2)]
    unigram_count <- x[words[2], sum(count1)]
    
    obs_trigram <- z[.(words[1], words[2])]
    setkey(obs_trigram, count3)

    qbo_obs_tri <- obs_trigram[gt, nomatch = 0
                               ][, discount := calc_discount(count3, tri, tri_next)
                                 ][, .(word1, word2, word3, qbo = (count3 - discount) / bigram_count)]
    
    unobs_words <- x[!(word1 %in% qbo_obs_tri$word3), ]
    
    obs_bigram <- y[words[2]]
    setkey(obs_bigram, count2)

    qbo_obs_bi <- obs_bigram[gt, nomatch = 0
                             ][, discount := calc_discount(count2, bi, bi_next)
                               ][, .(word1, word2, count2, qbo2 = (count2 - discount) / unigram_count)]
    
    alpha2 <- 1 - sum(qbo_obs_bi$qbo2)
    
    qbo_unobs_bi <- unobs_words[!(word1 %in% qbo_obs_bi$word2)
                                ][, .(word1 = words[2], word2 = word1, count2 = count1, qbo2 = alpha2 * (count1 / sum(count1)))]
    
    qbo_bigram <- rbind(qbo_obs_bi, qbo_unobs_bi)
    setkey(qbo_bigram, word1, word2)
    
    alpha3 <- 1 - sum(qbo_obs_tri$qbo)
    
    unobs_trigram <- unobs_words[, .(word1 = words[1], word2 = words[2], word3 = word1, count1)]
    setkey(unobs_trigram, word2, word3)

    qbo_unobs_tri <- unobs_trigram[qbo_bigram, nomatch = 0
                                   ][, .(word1, word2, word3, qbo = alpha3 * (qbo2 / sum(qbo2)))]
    qbo_trig <- rbind(qbo_obs_tri, qbo_unobs_tri)[order(-qbo)]
    
    if (keep_stop == FALSE) {
        qbo_trig[!(word3 %in% quanteda::stopwords("english"))][1:return_n]
    } else {
        qbo_trig[1:return_n]
    }
}
