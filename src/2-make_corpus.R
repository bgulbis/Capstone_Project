
library(tidyverse)
library(stringr)
library(tm)
library(SnowballC)

profanity <- read_lines("data/external/profanity.txt")

# blogs ------------------------------------------------
blogs <- read_lines("data/raw/en_US.blogs.txt.gz") %>%
    str_to_lower() 

keep <- map_lgl(blogs, ~ runif(1) >= 0.9)

blogs <- blogs[keep]

write_rds(blogs, "data/tidy/blogs.Rds")

corpus_blogs <- Corpus(VectorSource(blogs)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, c(profanity, stopwords("english"))) %>%
    tm_map(stemDocument)

write_rds(corpus_blogs, "data/tidy/corpus_blogs.Rds")

dtm_blogs <- DocumentTermMatrix(corpus_blogs)

# news -------------------------------------------------
news <- read_lines("data/raw/en_US.news.txt.gz") %>%
    str_to_lower()

keep <- map_lgl(news, ~ runif(1) >= 0.9)

news <- news[keep]

write_rds(news, "data/tidy/news.Rds")

corpus_news <- Corpus(VectorSource(news)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, c(profanity, stopwords("english"))) %>%
    tm_map(stemDocument)

write_rds(corpus_news, "data/tidy/corpus_news.Rds")

dtm_news <- DocumentTermMatrix(corpus_news)

# tweets -----------------------------------------------
tweets <- read_lines("data/raw/en_US.twitter.txt.gz") %>%
    str_to_lower()

keep <- map_lgl(tweets, ~ runif(1) >= 0.9)

tweets <- tweets[keep]

write_rds(tweets, "data/tidy/tweets.Rds")

corpus_tweets <- Corpus(VectorSource(tweets)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, c(profanity, stopwords("english"))) %>%
    tm_map(stemDocument)

write_rds(corpus_tweets, "data/tidy/corpus_tweets.Rds")

dtm_tweets <- DocumentTermMatrix(corpus_tweets)

