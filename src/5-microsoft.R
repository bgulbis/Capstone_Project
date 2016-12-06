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

predict_web <- function(x) {
    words <- str_to_lower(x) %>%
        word(-4, -1) 
    
    r <- POST(url, add_headers(api), 
              query = list(model = "body", words = words))
    
    fromJSON(content(r, "text"))$candidates
}

cond_url <- "https://api.projectoxford.ai/text/weblm/v1.0/calculateConditionalProbability"

check_prob <- function(x, y) {
    words <- str_to_lower(x) %>%
        word(-4, -1) 

    z <- map(y, ~ list(words = words, word = .x))
    
    r <- POST(cond_url, add_headers(api), 
              query = list(model = "body"),
              body = list(queries = z),
              encode = "json")
    
    fromJSON(content(r, "text"))$results
}

x1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
y1 <- c("eat", "sleep", "die", "give")

q1 <- check_prob(x1, y1)

x2 <- predict_web("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his")
x3 <- predict_web("I'd give anything to see arctic monkeys this")
x4 <- predict_web("Talking to your mom has the same effect as a hug and helps reduce your")
x5 <- predict_web("When you were in Holland you were like 1 inch away from me but you hadn't time to take a")
x6 <- predict_web("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the")
x7 <- predict_web("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each")
x8 <- predict_web("Every inch of you is perfect from the bottom to the")
x9 <- predict_web("I’m thankful my childhood was filled with imagination and bruises from playing")
x10 <- predict_web("I like how the same people are in almost all of Adam Sandler's")
