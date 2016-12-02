library(tidyverse)
library(quanteda)

# collocation ------------------------------------------

profanity <- read_lines("data/external/profanity.txt") 

size = 1000
blogs <- read_lines("data/tidy/train_blogs.Rds") %>%
    corpus() %>%
    sample(size = size) %>%
    tokenize("sentence", simplify = TRUE)

blogs_col <- collocations(blogs, method = "all", size = 3)

blogs_dfm <- dfm(blogs,
                 toLower = TRUE, 
                 ignoredFeatures = c(profanity), 
                 removeNumbers = TRUE, 
                 removePunct = TRUE, 
                 removeSeparators = TRUE) 

blogs_tfidf <- tfidf(blogs_dfm)
