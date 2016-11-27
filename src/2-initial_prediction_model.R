
library(tidyverse)
library(stringr)

# prob_df <- read_rds("data/final/initial_prediction.Rds")
dirr::get_rds("data/final")

calc_prob <- function(disc, freq, freq_sum) {
    disc * freq / freq_sum
}

predict_text <- function(x) {
    words <- str_to_lower(x) %>%
        word(-2, -1) %>%
        str_split(" ") %>%
        unlist()
    
    pred_3gram <- initial_prediction_3gram_gt %>%
        filter(word1 == words[1],
               word2 == words[2]) %>%
        mutate(prob = calc_prob(discount, count3, sum(count3))) %>%
        arrange(desc(prob)) %>%
        ungroup() %>%
        select(word = word3, prob, remain)

    pred_2gram <- initial_prediction_2gram_gt %>%
        filter(word1 == words[2],
               !(word1 %in% pred_3gram$word)) %>%
        mutate(prob = calc_prob(discount, count2, sum(count2)),
               alpha = pred_3gram$remain[[1]] / sum(prob),
               prob2 = alpha * prob) %>%
        arrange(desc(prob2)) %>%
        ungroup() %>%
        select(word = word2, prob = prob2, remain)
    
    pred_1gram <- initial_prediction_1gram_gt %>%
        filter(!word1 %in% pred_3gram$word,
               !word1 %in% pred_2gram$word) %>%
        mutate(prob = calc_prob(discount, count1, sum(count1)),
               alpha = pred_2gram$remain[[1]] / sum(prob),
               prob2 = alpha * prob) %>%
        arrange(desc(prob2)) %>%
        select(word = word1, prob = prob2)
    
    # list(tri = pred_3gram, bi = pred_2gram, uni = pred_1gram)
    bind_rows(pred_3gram, pred_2gram, pred_1gram) %>%
        arrange(desc(prob)) %>%
        top_n(3, prob)
}

x <- predict_text("Go on a romantic date at the")
