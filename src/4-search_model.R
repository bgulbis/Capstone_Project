
library(tidyverse)
library(stringr)
library(quanteda)
library(text2vec)
library(doParallel)
registerDoParallel()

blogs <- read_rds("data/tidy/train_blogs.Rds") %>%
    as_tibble()

news <- read_rds("data/tidy/train_news.Rds") %>%
    as_tibble()

tweets <- read_rds("data/tidy/train_tweets.Rds") %>%
    as_tibble()

profanity <- read_lines("data/external/profanity.txt") 

words_compare <- function(x, n = 2) {
    require(quanteda)
    y <- tokenize(
        x,
        removeNumbers = TRUE, 
        removePunct = TRUE, 
        removeSymbols = TRUE, 
        removeTwitter = TRUE, 
        removeURL = TRUE
    ) %>%
        # quanteda::removeFeatures(profanity)
        quanteda::removeFeatures(c(stopwords("english"), profanity))
    
    y[[1]][(length(y[[1]]) - (n - 1)):length(y[[1]])]
}

search_corpus <- function(words, n = 2) {
    # lookup <- map_chr(1:n, ~ str_c(words[(length(words) - .x + 1):length(words)], collapse = " "))

    # extr <- list(~str_detect(value, paste0("(", str_c(lookup, collapse = ")|("), ")")))

    extr <- list(~str_detect(value, str_c(words[length(words)])))
    
    x <- filter_(blogs, .dots = extr) 
    
    y <- filter_(news, .dots = extr)
    
    z <- filter_(tweets, .dots = extr)
    
    bind_rows(x, y, z)$value %>%
        tokenize("sentence", simplify = TRUE, verbose = TRUE)
}
    
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

make_ngrams <- function(corp) {
    it <- itoken(corp, toLower, my_tokenizer)
    
    vocab <- create_vocabulary(
        it, 
        ngram = c(1L, 3L), 
        stopwords = c(stopwords("english"), profanity)
    ) 
    
    vect <- vocab_vectorizer(vocab)
    
    create_dtm(it, vect)
}

calc_discount <- function(r, m, n) {
    if_else(r > 0 & r <= 5, ((r + 1) / r) * (n / m), 1) 
}

calc_prob_remain <- function(disc, mle) {
    1 - sum(disc * mle)
}


predict_text <- function(x) {
    words <- words_compare(x)
    corp <- search_corpus(words)
    dtm <- make_ngrams(corp)
    
    print(c("Searching by: ", words))

    count_dtm <- colSums(dtm) %>% 
        as_tibble() %>%
        rownames_to_column("ngram") %>%
        mutate(n = str_count(ngram, "_") + 1) 
    
    words_1gram <- count_dtm %>%
        filter(n == 1) %>%
        mutate(mle1 = value / sum(value)) %>%
        select(word1 = ngram, count1 = value, mle1)
    
    words_2gram <- count_dtm %>%
        filter(n == 2) %>%
        separate(ngram, c("word1", "word2"), sep = "_") %>%
        left_join(words_1gram, by = "word1") %>%
        mutate(mle2 = value / count1) %>%
        select(word1, word2, count2 = value, mle2)
    
    words_3gram <- count_dtm %>%
        filter(n == 3) %>%
        separate(ngram, c("word1", "word2", "word3"), sep = "_") %>%
        left_join(words_2gram, by = c("word1", "word2")) %>%
        mutate(mle3 = value / count2) %>%
        select(word1, word2, word3, count3 = value, mle3)
    
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
    
    pred <- pred_3gram_gt %>%
        filter(word1 == words[1],
               word2 == words[2]) %>%
        mutate(prob = discount * mle3) %>%
        ungroup() %>%
        select(word = word3, prob)
    
    if (nrow(pred) == 0) {
        print("There were no matching trigrams, searching bigrams")
        
        pred <- pred_2gram_gt %>%
            filter(word1 == words[2]) %>%
            mutate(est = discount * mle2,
                   alpha = remain / sum(est),
                   prob = alpha * est) %>%
            ungroup() %>%
            select(word = word2, prob)
        
        if (nrow(pred) == 0) {
            print("There were no matching bigrams, searching unigrams")
            pred <- pred_1gram_gt %>%
                mutate(est = discount * mle1,
                       alpha = remain / sum(est),
                       prob = alpha * est) %>%
                select(word = word1, prob)
        }
    } 
    
    arrange(pred, desc(prob)) 
}

x1 <- predict_text("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
x2 <- predict_text("You're the reason why I smile everyday. Can you follow me please? It would mean the")
x3 <- predict_text("Hey sunshine, can you follow me and make me the")
x4 <- predict_text("Very early observations on the Bills game: Offense still struggling but the")
x5 <- predict_text("Go on a romantic date at the")
x6 <- predict_text("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
x7 <- predict_text("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
x8 <- predict_text("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
x9 <- predict_text("Be grateful for the good times and keep the faith during the")
x10 <- predict_text("If this isn't the cutest thing you've ever seen, then you must be")

q1 <- filter(x1, word %in% c("soda", "cheese", "beer", "pretzels"))
q2 <- filter(x2, word %in% c("world", "most", "best", "universe"))
q3 <- filter(x3, word %in% c("smelliest", "saddest", "happiest", "bluest"))
q4 <- filter(x4, word %in% c("players", "defense", "crowd", "referees"))
q5 <- filter(x5, word %in% c("mall", "movies", "beach", "grocery"))
q6 <- filter(x6, word %in% c("way", "motorcycle", "phone", "horse"))
q7 <- filter(x7, word %in% c("thing", "time", "years", "weeks"))
q8 <- filter(x8, word %in% c("toes", "fingers", "ears", "eyes"))
q9 <- filter(x9, word %in% c("hard", "bad", "sad", "worse"))
q10 <- filter(x10, word %in% c("callous", "asleep", "insensitive", "insane"))
