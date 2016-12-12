# quiz 2

library(microbenchmark)

source("src/4-prediction_model.R")
source("src/4a-prediction_model_feather.R")

n <- 25

questions <- list(
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
)

microbenchmark(predict_text(questions[[1]], "blogs", n), times = 5L)
microbenchmark(predict_text_feather(questions[[1]], "blogs", n), times = 5L)

microbenchmark(answers_blogs <- purrr::map(questions, predict_text, corp = "blogs", return_n = n), times = 5L)
microbenchmark(answers_blogs2 <- purrr::map(questions, predict_text_feather, corp = "blogs", return_n = n), times = 5L)

answers_news <- purrr::map(questions, predict_text, corp = "news", return_n = n)
answers_news2 <- purrr::map(questions, predict_text, corp = "news", return_n = n, keep_stop = TRUE)

answers_tweets <- purrr::map(questions, predict_text, corp = "tweets", return_n = n)
answers_tweets2 <- purrr::map(questions, predict_text, corp = "tweets", return_n = n, keep_stop = TRUE)
