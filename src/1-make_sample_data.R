# Initial prediction model

library(tidyverse)

frac <- 0.01

blogs <- read_lines("data/raw/en_US.blogs.txt.gz") 

set.seed(77123)
x <- sample.int(length(blogs), frac * length(blogs))
test_blogs <- blogs[x]
train_blogs <- blogs[-x]

news <- read_lines("data/raw/en_US.news.txt.gz") 

set.seed(77123)
x <- sample.int(length(news), frac * length(news))
test_news <- news[x]
train_news <- news[-x]

tweets <- read_lines("data/raw/en_US.twitter.txt.gz") 

set.seed(77123)
x <- sample.int(length(tweets), frac * length(tweets))
test_tweets <- tweets[x]
train_tweets <- tweets[-x]

write_rds(train_blogs, "data/tidy/train_blogs.Rds")
write_rds(train_news, "data/tidy/train_news.Rds")
write_rds(train_tweets, "data/tidy/train_tweets.Rds")

write_rds(test_blogs, "data/final/test_blogs.Rds")
write_rds(test_news, "data/final/test_news.Rds")
write_rds(test_tweets, "data/final/test_tweets.Rds")
