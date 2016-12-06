# microsoft api

library(httr)
library(jsonlite)
library(stringr)
library(tidyverse)

# Ocp-Apim-Subscription-Key
api <- "39b702f50fd04d1f9730243bbbd6e0e8"
names(api) <- "Ocp-Apim-Subscription-Key"

url <- "https://api.projectoxford.ai/text/weblm/v1.0/generateNextWords"

# model: body
# words
# order: 1-5, optional
# maxNumOfCandidatesReturned, default = 5; optional

predict_web <- function(x, n = 5) {
    words <- str_to_lower(x) %>%
        word((-1 * (n - 1)), -1) 
    
    r <- POST(url, add_headers(api), 
              query = list(model = "body", order = n, words = words))
    
    fromJSON(content(r, "text"))$candidates
}

cond_url <- "https://api.projectoxford.ai/text/weblm/v1.0/calculateConditionalProbability"

check_prob <- function(x, y, n = 5) {
    words <- str_to_lower(x) %>%
        word((-1 * (n - 1)), -1) 

    z <- map(y, ~ list(words = words, word = .x))
    
    r <- POST(cond_url, add_headers(api), 
              query = list(model = "body", order = n),
              body = list(queries = z),
              encode = "json")
    
    fromJSON(content(r, "text"))$results
}

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
