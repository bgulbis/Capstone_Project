
library(tidyverse)
library(stringr)

# prob_df <- read_rds("data/final/initial_prediction.Rds")
# dirr::get_rds("data/final")

words_compare <- function(x, n = 2) {
    y <- quanteda::tokenize(
        x,
        removeNumbers = TRUE, 
        removePunct = TRUE, 
        removeSymbols = TRUE, 
        removeTwitter = TRUE, 
        removeURL = TRUE
    ) %>%
        quanteda::removeFeatures(c(stopwords("english"), profanity))
    
    y[[1]][(length(y[[1]]) - (n - 1)):length(y[[1]])]
}

predict_text <- function(x, corp = "blogs") {
    # words <- words_compare(x)
    
    words <- str_to_lower(x) %>%
        word(-2, -1) %>%
        str_split(" ") %>%
        unlist()
    
    print(c("Searching by: ", words))

    prob <- list.files("data/final", paste0("^pred(.*)", corp), full.names = TRUE) %>%
        map(read_rds)
    
    # 
    # if (corp == "blogs") {
    #     tokens <- tokens_blogs
    # } else if (corp == "news") {
    #     tokens <- tokens_news
    # } else if (corp == "Twitter") {
    #     tokens <- tokens_tweets
    # } else {
    #     stop("Invalid corpus")
    # }
    
    pred <- prob[[3]] %>%
        filter(word1 == words[1],
               word2 == words[2]) %>%
        mutate(prob = discount * mle3) %>%
        ungroup() %>%
        select(word = word3, prob)
    
    if (nrow(pred) == 0) {
        print("There were no matching trigrams, searching bigrams")
        
        pred <- prob[[2]] %>%
            filter(word1 == words[2]) %>%
            mutate(est = discount * mle2,
                   alpha = remain / sum(est),
                   prob = alpha * est) %>%
            ungroup() %>%
            select(word = word2, prob)
        
        if (nrow(pred) == 0) {
            print("There were no matching bigrams, searching unigrams")
            pred <- prob[[1]] %>%
                mutate(est = discount * mle1,
                       alpha = remain / sum(est),
                       prob = alpha * est) %>%
                select(word = word1, prob)
        }
    } 

    arrange(pred, desc(prob)) %>%
        top_n(3, prob)
}

x1 <- predict_text("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", "news")
x2 <- predict_text("You're the reason why I smile everyday. Can you follow me please? It would mean the")
x3 <- predict_text("Hey sunshine, can you follow me and make me the")
x4 <- predict_text("Very early observations on the Bills game: Offense still struggling but the")
x5 <- predict_text("Go on a romantic date at the")
x6 <- predict_text("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
x7 <- predict_text("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
x8 <- predict_text("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
x9 <- predict_text("Be grateful for the good times and keep the faith during the")
x10 <- predict_text("If this isn't the cutest thing you've ever seen, then you must be")
