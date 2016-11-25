
library(tidyverse)
library(tidytext)
library(stringr)
library(forcats)
library(tm)
library(wordcloud)

profanity <- read_lines("data/external/profanity.txt") %>%
    as_tibble()

blogs <- read_lines("data/raw/en_US.blogs.txt.gz") %>%
    as_tibble() 

set.seed(77123)
blogs_sample <- blogs %>%
    sample_frac(0.2)

lines <- nrow(blogs)
chars_line <- nchar(blogs$value)

# blogs_utf8 <- blogs %>%
#     dmap_at("value", iconv, from = "latin1", to = "ASCII", sub = "") 

# chars_utf8 <- nchar(blogs_utf8$value)

# blogs_corp <- Corpus(VectorSource(blogs$value))

# removes punctuation and unusual characters automatically
blogs_tokens <- blogs %>%
    unnest_tokens(word, value) %>%
    anti_join(profanity, by = c("word" = "value"))

blogs_tokens_sample <- blogs_sample %>%
    unnest_tokens(word, value) %>%
    anti_join(profanity, by = c("word" = "value")) %>%
    filter(!str_detect(word, "[[:digit:]]"))

blogs_2gram <- blogs_sample %>%
    unnest_tokens(word, value, token = "ngrams", n = 2, collapse = FALSE) %>%
    separate(word, c("first", "second"), sep = " ", remove = FALSE) %>%
    anti_join(profanity, by = c("first" = "value")) %>%
    anti_join(profanity, by = c("second" = "value")) %>%
    select(word) %>%
    filter(!str_detect(word, "[[:digit:]]"))

blogs_3gram <- blogs_sample %>%
    unnest_tokens(word, value, token = "ngrams", n = 3, collapse = FALSE) %>%
    separate(word, c("first", "second", "third"), sep = " ", remove = FALSE) %>%
    anti_join(profanity, by = c("first" = "value")) %>%
    anti_join(profanity, by = c("second" = "value")) %>%
    anti_join(profanity, by = c("third" = "value")) %>%
    select(word) %>%
    filter(!str_detect(word, "[[:digit:]]"))

# blogs_sentences <- blogs %>%
#     unnest_tokens(word, value, token = "sentences", collapse = FALSE)

blogs_count <- blogs_tokens %>%
    count(word, sort = TRUE) 

blogs_count_sample <- blogs_tokens_sample %>%
    count(word, sort = TRUE)

blogs_count_2gram <- blogs_2gram %>%
    count(word, sort = TRUE) 

blogs_count_3gram <- blogs_3gram %>%
    count(word, sort = TRUE) 

blogs_count_sample %>%
    with(wordcloud(word, n, max.words = 250))

blogs_pos <- blogs_count_sample %>%
    left_join(parts_of_speech, by = "word")

blogs_sum_words <- blogs_count %>%
    mutate(run_total = cumsum(n))

blogs_50p <- blogs_sum_words %>%
    filter(run_total <= 0.5 * nrow(blogs_tokens))

blogs_90p <- blogs_sum_words %>%
    filter(run_total <= 0.9 * nrow(blogs_tokens))

blogs_count_sample %>%
    top_n(25, n) %>%
    dmap_at("word", fct_inorder) %>%
    ggplot(aes(x = word, y = n)) +
    geom_bar(stat = "identity")

blogs_count_2gram %>%
    top_n(25, n) %>%
    dmap_at("word", fct_inorder) %>%
    ggplot(aes(x = word, y = n)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

blogs_count_3gram %>%
    top_n(25, n) %>%
    dmap_at("word", fct_inorder) %>%
    ggplot(aes(x = word, y = n)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# library(wordnet)
# setDict("dict")
# 
# if (initDict("dict")) filter <- getTermFilter("ExactMatchFilter", blogs_count_sample$word[1], TRUE)
# if (initDict("dict")) getLemma(blogs_count_sample$word[1])
# 
# check_dict <- head(blogs_count_sample, 20) %>%
#     mutate(dict = getTermFilter("ExactMatchFilter", word, TRUE))
# getDict()
    
# length(blogs)
# nchar(blogs)

# data("stop_words")
