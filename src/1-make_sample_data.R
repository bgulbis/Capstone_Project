# Initial prediction model

library(tidyverse)
library(stringr)
library(tidytext)
library(caret)

frac <- 0.3

set.seed(77123)
blogs <- read_lines("data/raw/en_US.blogs.txt.gz") %>%
    as_tibble() %>%
    sample_frac(frac)

set.seed(77123)
news <- read_lines("data/raw/en_US.news.txt.gz") %>%
    as_tibble() %>%
    sample_frac(frac)

set.seed(77123)
tweets <- read_lines("data/raw/en_US.twitter.txt.gz") %>%
    as_tibble() %>%
    sample_frac(frac)

pval = 5/6
make_partition <- function(df, p = 5/6) {
    set.seed(77123)
    createDataPartition(df$value, p = p)
}

set.seed(77123)
blogs_part <- make_partition(blogs)
blogs_train <- blogs[blogs_part, ]

news_part <- createDataPartition(news$value, p = pval)
tweets_part <- createDataPartition(tweets$value, p = pval)


words_train <- words[words_part, ]
words_test <- words[-words_part, ]

write_rds(words_train, "data/tidy/words_train.Rds")
write_rds(words_test, "data/tidy/words_test.Rds")
