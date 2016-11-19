
library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(wordcloud)

blogs <- read_lines("data/raw/en_US.blogs.txt.gz") %>%
    as_tibble() %>%
    dmap_at("value", iconv, from = "latin1", to = "ASCII", sub = " ")

# blogs_corp <- Corpus(VectorSource(blogs$value))

blogs_2gram <- blogs %>%
    unnest_tokens(word, value, token = "ngrams", n = 2)

blogs_3gram <- blogs %>%
    unnest_tokens(word, value, token = "ngrams", n = 3)

blogs_tokens <- blogs %>%
    unnest_tokens(word, value)

profanity <- read_lines("data/external/profanity.txt") %>% 
    as_tibble()

blogs_prof <- anti_join(blogs_tokens, profanity, by = c("word" = "value"))

blogs_count <- blogs_prof %>%
    # filter(!str_detect(word, "[0-9]")) %>%
    count(word, sort = TRUE)

blogs_count %>%
    with(wordcloud(word, n, max.words = 100))

blogs_pos <- blogs_count %>%
    left_join(parts_of_speech, by = "word")

# length(blogs)
# nchar(blogs)

# data("stop_words")
