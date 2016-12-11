
# library(tidyverse)
# library(stringr)
# library(data.table)

# prob_df <- read_rds("data/final/initial_prediction.Rds")
# dirr::get_rds("data/final")

# words_compare <- function(x, n = 2) {
#     y <- quanteda::tokenize(
#         x,
#         removeNumbers = TRUE, 
#         removePunct = TRUE, 
#         removeSymbols = TRUE, 
#         removeTwitter = TRUE, 
#         removeURL = TRUE
#     ) %>%
#         quanteda::removeFeatures(c(stopwords("english"), profanity))
#     
#     y[[1]][(length(y[[1]]) - (n - 1)):length(y[[1]])]
# }

predict_text <- function(x, corp = "blogs", return_n = 5, keep_stop = FALSE) {
    require(tidyverse)
    require(stringr)
    require(data.table)
    # words <- words_compare(x)
    
    words <- str_to_lower(x) %>%
        word(-2, -1) %>%
        str_split(" ") %>%
        unlist()
    
    print(c("Searching by: ", words))

    # prob <- list.files("data/final", paste0("^pred(.*)", corp), full.names = TRUE) %>%
    #     map(read_rds) %>%
    #     map(as.data.table)
    
    prob <- read_rds(paste0("data/final/pred_3gram_", corp, ".Rds")) %>%
        as.data.table()
    
    pred <- prob[word1 == words[1] & word2 == words[2], .(word = word3, prob = discount * mle3)][order(-prob)]
    
    if (nrow(pred) == 0) {
        print("There were no matching trigrams, searching bigrams")
        prob <- read_rds(paste0("data/final/pred_2gram_", corp, ".Rds")) %>%
            as.data.table()
        pred <- prob[word1 == words[2], .(word = word2, prob = (remain / sum(discount * mle2)) * (discount * mle2))][order(-prob)]
        
        if (nrow(pred) == 0) {
            print("There were no matching bigrams, searching unigrams")
            prob <- read_rds(paste0("data/final/pred_1gram_", corp, ".Rds")) %>%
                as.data.table()
            pred <- prob[, .(word = word1, prob = (remain / sum(discount * mle1)) * (discount * mle1))][order(-prob)]
        }
    } 

    if (!keep_stop) {
        pred[!(word %in% quanteda::stopwords("english"))]
    } 
    
    pred[1:return_n]
}
