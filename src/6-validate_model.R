# validation

library(tidyverse)

make_set <- function(x) {
    require(tidyverse)
    require(stringr)
    require(quanteda)
    
    x <- x %>%
        tokenize("sentence", simplify = TRUE, verbose = TRUE) %>%
        tokenize(removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, 
                 removeTwitter = TRUE, removeURL = TRUE)
    
    drop_sent <- map_lgl(x, ~ length(.x) == 1)
    
    # drop last word from sentence
    sent <- x[!drop_sent] %>%
        map(~ str_c(.x[1:length(.x) - 1], collapse = " ")) %>%
        unlist()

    # extract last word
    last <- x[!drop_sent] %>%
        map(~ .x[length(.x)]) %>%
        unlist()

    tibble(sentence = sent, word = last)
}

valid_blogs <- read_rds("data/tidy/valid_blogs.Rds") %>%
    make_set() 

valid_news <- read_rds("data/tidy/valid_news.Rds") %>%
    make_set()

valid_tweets <- read_rds("data/tidy/valid_tweets.Rds") %>%
    make_set()


