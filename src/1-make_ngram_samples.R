# Initial prediction model

library(tidyverse)

profanity <- read_lines("data/external/profanity.txt") %>%
    as_tibble()

blogs <- read_lines("data/raw/en_US.blogs.txt.gz") %>%
    as_tibble() 

news <- read_lines("data/raw/en_US.news.txt.gz") %>%
    as_tibble()

tweets <- read_lines("data/raw/en_US.twitter.txt.gz") %>%
    as_tibble() 

frac <- 0.2

set.seed(77123)
blogs_sample <- blogs %>%
    select(value) %>%
    sample_frac(frac) 

news_sample <- news %>%
    select(value) %>%
    sample_frac(frac) 

tweets_sample <- tweets %>%
    select(value) %>%
    sample_frac(frac) 

dirr::save_rds("data/tidy", "sample")
