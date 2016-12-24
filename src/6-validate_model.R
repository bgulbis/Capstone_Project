# validation

source("src/4-prediction_function.R")

# library(tidyverse)

make_set <- function(x) {
    require(tidyverse)
    require(stringr)
    require(quanteda)
    
    x <- x %>%
        tokenize("sentence", simplify = TRUE, verbose = TRUE) %>%
        tokenize(removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, 
                 removeTwitter = TRUE, removeURL = TRUE)
    
    drop_sent <- map_lgl(x, ~ length(.x) < 4)
    
    # drop last word from sentence
    sent <- x[!drop_sent] %>%
        map(~ str_c(.x[1:length(.x) - 1], collapse = " ")) %>%
        unlist()

    # extract last word
    last <- x[!drop_sent] %>%
        map(~ .x[length(.x)]) %>%
        unlist()

    tibble(sentence = sent, word = last) %>%
        dmap(str_to_lower)
}

valid_blogs <- read_rds("data/tidy/valid_blogs.Rds") %>%
    make_set() 

valid_news <- read_rds("data/tidy/valid_news.Rds") %>%
    make_set()

valid_tweets <- read_rds("data/tidy/valid_tweets.Rds") %>%
    make_set()

quiz2 <- tibble(
    sentence = c(
        "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
        "You're the reason why I smile everyday. Can you follow me please? It would mean the",
        "Hey sunshine, can you follow me and make me the",
        "Very early observations on the Bills game: Offense still struggling but the", 
        "Go on a romantic date at the", 
        "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", 
        "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", 
        "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", 
        "Be grateful for the good times and keep the faith during the", 
        "If this isn't the cutest thing you've ever seen, then you must be"
    ),
    word = c("beer", "world", "happiest", "defense", "beach", "way", "time", "fingers", "bad", "insane")
)


# src <- c("blogs", "news", "tweets", "all")

microbenchmark::microbenchmark(predict_words(quiz2$sentence[1], "all"), times = 10)

test_quiz2 <- quiz2 %>%
    by_row(~ predict_words(.x$sentence, return_n = 1), .collate = "rows") %>%
    select(sentence, word, pred_all = word3) 
    
test_quiz2_accuracy <- test_quiz2 %>%
    summarize(pred_all = sum(pred_all == word) / n())

valid_set <- bind_rows(valid_blogs, valid_news, valid_tweets) %>%
    sample_n(500) 

test1 <- valid_set %>%
    by_row(~ predict_words(.x$sentence, return_n = 1), .collate = "rows") %>%
    select(sentence, word, pred_all = word3) 

test1_accuracy <- test1 %>%
    summarize(pred_all = sum(word == pred_all) / n())

test2 <- valid_set %>%
    by_row(~ predict_words(.x$sentence, return_n = 1, keep_stop = TRUE), .collate = "rows") %>%
    select(sentence, word, pred_all = word3) 

test21_accuracy <- test2 %>%
    summarize(pred_all = sum(word == pred_all) / n())

x1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
y1 <- c("eat", "sleep", "die", "give")
check_prob(x1, y1)
# die

x2 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
y2 <- c("spiritual", "horticultural", "marital", "financial")
check_prob(x2, y2)
# marital

x3 <- "I'd give anything to see arctic monkeys this"
y3 <- c("decade", "weekend", "morning", "month")
check_prob(x3, y3)
# weekend

x4 <- "Talking to your mom has the same effect as a hug and helps reduce your"
y4 <- c("sleepiness", "stress", "happiness", "hunger")
check_prob(x4, y4)
# stress

x5 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
y5 <- c("walk", "look", "minute", "picture")
check_prob(x5, y5)
# picture

x6 <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
y6 <- c("case", "matter", "incident", "account")
check_prob(x6, y6)
# matter
a6 <- predict_web(x6)
a a6 <- predict_web("and a jury to")

x7 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
y7 <- c("finger", "toe", "hand", "arm")
check_prob(x7, y7)
# hand

x8 <- "Every inch of you is perfect from the bottom to the"
y8 <- c("side", "center", "top", "middle")
check_prob(x8, y8)
# top

x9 <- "I’m thankful my childhood was filled with imagination and bruises from playing"
y9 <- c("inside", "outside", "weekly", "daily")
check_prob(x9, y9)
# outside

x10 <- "I like how the same people are in almost all of Adam Sandler's"
y10 <- c("stories", "movies", "novels", "pictures")
check_prob(x10, y10)
# movies
