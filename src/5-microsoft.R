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
    
    ans <- fromJSON(content(r, "text"))$candidates
    
    if (length(ans) == 0) {
        data.frame(word = "Unable to find a prediction", probability = 0)
    } else {
        ans
    }
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
