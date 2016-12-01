
library(tidyverse)
library(stringr)
library(quanteda)

blogs <- read_rds("data/tidy/train_blogs.Rds") %>%
    as_tibble()

news <- read_rds("data/tidy/train_news.Rds") %>%
    as_tibble()

tweets <- read_rds("data/tidy/train_tweets.Rds") %>%
    as_tibble()

search_corpus <- function(phrase) {
    extr <- list(~str_extract(value, paste0(phrase, "(.*)")),
                 ~word(val, end = 3))
    
    nm <- list("val", "phrase")
    
    x <- filter_(blogs, .dots = list(~str_detect(value, phrase))) %>%
        mutate_(.dots = set_names(extr, nm))
    
    y <- filter_(news, .dots = list(~str_detect(value, phrase))) %>%
        mutate_(.dots = set_names(extr, nm))
    
    z <- filter_(tweets, .dots = list(~str_detect(value, phrase))) %>%
        mutate_(.dots = set_names(extr, nm))
    
    bind_rows(x, y, z)$phrase %>%
        corpus() %>%
        tokenize("sentence", simplify = TRUE)
}
    
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

profanity <- read_lines("data/external/profanity.txt") 

# words <- search_corpus("case of")

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

pred_1gram_gt <- words_1gram_gt
pred_2gram_gt <- words_2gram_gt
pred_3gram_gt <- words_3gram_gt

make_phrase <- function(x) {
    words <- str_to_lower(x) %>%
        word(-2, -1) 
        # str_split(" ") %>%
        # unlist()
    
    # print(c("Searching by: ", words))
    words
}

predict_text <- function(phrase) {
    words <- make_phrase(phrase) 
    
    corp <- search_corpus(words)
    
    words_1gram <- make_ngram(corp, 1L) %>%
        calc_mle()
    
    words_2gram <- make_ngram(corp, 2L) %>%
        calc_mle(words_1gram, 2L)
    
    words_3gram <- make_ngram(corp, 3L) %>%
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
    
    
    words <- str_split(words, " ") %>%
        unlist()
    
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
        # top_n(3, prob)
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
