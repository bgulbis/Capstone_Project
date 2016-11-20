
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
    count(word, sort = TRUE) %>%
    filter(!str_detect(word, "[0-9]")) 

blogs_count_2gram <- blogs_2gram %>%
    count(word, sort = TRUE)

blogs_count_3gram <- blogs_3gram %>%
    count(word, sort = TRUE)

blogs_count %>%
    with(wordcloud(word, n, max.words = 100))

blogs_pos <- blogs_count %>%
    left_join(parts_of_speech, by = "word")

blogs_sum_words <- blogs_count %>%
    mutate(run_total = cumsum(n))

blogs_50p <- blogs_sum_words %>%
    filter(run_total <= 0.5 * nrow(blogs_tokens))

blogs_90p <- blogs_sum_words %>%
    filter(run_total <= 0.9 * nrow(blogs_tokens))


# length(blogs)
# nchar(blogs)

# data("stop_words")
