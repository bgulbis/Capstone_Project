# Initial prediction model

library(tidyverse)

frac <- 500

blogs <- read_lines("data/raw/en_US.blogs.txt.gz") 

set.seed(77123)
x <- sample.int(length(blogs), frac)
test_blogs <- blogs[x]
train_blogs <- blogs[-x]

news <- read_lines("data/raw/en_US.news.txt.gz") 

set.seed(77123)
x <- sample.int(length(news), frac)
test_news <- news[x]
train_news <- news[-x]

tweets <- read_lines("data/raw/en_US.twitter.txt.gz") 

set.seed(77123)
x <- sample.int(length(tweets), frac)
test_tweets <- tweets[x]
train_tweets <- tweets[-x]

write_rds(train_blogs, "data/final/train_blogs.Rds", compress = "gz")
write_rds(train_news, "data/final/train_news.Rds", compress = "gz")
write_rds(train_tweets, "data/final/train_tweets.Rds", compress = "gz")

write_rds(test_blogs, "data/final/test_blogs.Rds", compress = "gz")
write_rds(test_news, "data/final/test_news.Rds", compress = "gz")
write_rds(test_tweets, "data/final/test_tweets.Rds", compress = "gz")
