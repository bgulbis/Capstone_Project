# Initial prediction model

library(tidyverse)

frac <- 0.01
valid <- 0.1

blogs <- read_lines("data/raw/en_US.blogs.txt.gz") 

set.seed(77123)
x <- sample.int(length(blogs), frac * length(blogs))
test_blogs <- blogs[x]
train_blogs <- blogs[-x]

y <- sample.int(length(train_blogs), valid * length(train_blogs))
valid_blogs <- blogs[y]
train_blogs <- blogs[-y]

news <- read_lines("data/raw/en_US.news.txt.gz") 

set.seed(77123)
x <- sample.int(length(news), frac * length(news))
test_news <- news[x]
train_news <- news[-x]

y <- sample.int(length(train_news), valid * length(train_news))
valid_news <- news[y]
train_news <- news[-y]

tweets <- read_lines("data/raw/en_US.twitter.txt.gz") 

set.seed(77123)
x <- sample.int(length(tweets), frac * length(tweets))
test_tweets <- tweets[x]
train_tweets <- tweets[-x]

y <- sample.int(length(train_tweets), valid * length(train_tweets))
valid_tweets <- tweets[y]
train_tweets <- tweets[-y]

write_rds(train_blogs, "data/tidy/train_blogs.Rds", compress = "gz")
write_rds(train_news, "data/tidy/train_news.Rds", compress = "gz")
write_rds(train_tweets, "data/tidy/train_tweets.Rds", compress = "gz")

write_rds(valid_blogs, "data/tidy/valid_blogs.Rds", compress = "gz")
write_rds(valid_news, "data/tidy/valid_news.Rds", compress = "gz")
write_rds(valid_tweets, "data/tidy/valid_tweets.Rds", compress = "gz")

write_rds(test_blogs, "data/final/test_blogs.Rds", compress = "gz")
write_rds(test_news, "data/final/test_news.Rds", compress = "gz")
write_rds(test_tweets, "data/final/test_tweets.Rds", compress = "gz")
