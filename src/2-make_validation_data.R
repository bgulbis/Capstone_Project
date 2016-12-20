# Initial prediction model

library(tidyverse)

frac <- 500

blogs <- read_rds("data/final/train_blogs.Rds")

y <- sample.int(length(blogs), frac)
valid_blogs <- blogs[y]
train_blogs <- blogs[-y]

news <- read_rds("data/final/train_news.Rds") 

y <- sample.int(length(news), frac)
valid_news <- news[y]
train_news <- news[-y]

tweets <- read_rds("data/final/train_tweets.Rds")

y <- sample.int(length(tweets), frac)
valid_tweets <- tweets[y]
train_tweets <- tweets[-y]

write_rds(train_blogs, "data/tidy/train_blogs.Rds", compress = "gz")
write_rds(train_news, "data/tidy/train_news.Rds", compress = "gz")
write_rds(train_tweets, "data/tidy/train_tweets.Rds", compress = "gz")

write_rds(valid_blogs, "data/tidy/valid_blogs.Rds", compress = "gz")
write_rds(valid_news, "data/tidy/valid_news.Rds", compress = "gz")
write_rds(valid_tweets, "data/tidy/valid_tweets.Rds", compress = "gz")
