
library(tidyverse)
library(quanteda)
# library(topicmodels)

profanity <- read_lines("data/external/profanity.txt") 

set.seed(77123)
blogs <- read_lines("data/raw/en_US.blogs.txt.gz") %>%
    corpus() %>%
    sample(size = 1000)
    
blogs_tokens <- tokenize(blogs, "sentence", simplify = TRUE)

blogs_1gram <- dfm(blogs_tokens, toLower = TRUE, ignoredFeatures = c(profanity), 
                   removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE)
blogs_2gram <- dfm(blogs_tokens, toLower = TRUE, ignoredFeatures = c(profanity), 
                   removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, 
                   ngrams = 2L, concatenator = " ")
blogs_3gram <- dfm(blogs_tokens, toLower = TRUE, ignoredFeatures = c(profanity), 
                   removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE, 
                   ngrams = 3L, concatenator = " ")

blogs_1gram_freq <- docfreq(blogs_1gram, scheme = "count") %>%
    as_tibble() %>%
    rownames_to_column("unigram")

blogs_uni_freq <- tibble(uni = table(blogs_1gram_freq$value))

blogs_2gram_freq <- docfreq(blogs_2gram, scheme = "count") %>%
    as_tibble() %>%
    rownames_to_column("bigram") %>%
    separate(bigram, c("first", "second"), sep = " ", remove = FALSE)

blogs_bi_freq <- tibble(bi = table(blogs_2gram_freq$value))

blogs_3gram_freq <- docfreq(blogs_3gram, scheme = "count") %>%
    as_tibble() %>%
    rownames_to_column("trigram") %>%
    separate(trigram, c("first", "second", "third"), sep = " ", remove = FALSE)

blogs_tri_freq <- tibble(tri = table(blogs_3gram_freq$value))
