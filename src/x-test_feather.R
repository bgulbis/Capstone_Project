
library(tidyverse)
library(stringr)
library(feather)

x <- list.files("data/final", "pred_", full.names = TRUE)
nm <- list.files("data/final", "pred_")
nm <- str_replace_all(nm, ".Rds", "")
files <- map(x, read_rds)
names(files) <- nm
list2env(files, .GlobalEnv)

write_feather(pred_1gram_blogs, "data/final/pred_1gram_blogs.feather")
write_feather(pred_2gram_blogs, "data/final/pred_2gram_blogs.feather")
write_feather(pred_3gram_blogs, "data/final/pred_3gram_blogs.feather")
write_feather(pred_1gram_news, "data/final/pred_1gram_news.feather")
write_feather(pred_2gram_news, "data/final/pred_2gram_news.feather")
write_feather(pred_3gram_news, "data/final/pred_3gram_news.feather")
write_feather(pred_1gram_tweets, "data/final/pred_1gram_tweets.feather")
write_feather(pred_2gram_tweets, "data/final/pred_2gram_tweets.feather")
write_feather(pred_3gram_tweets, "data/final/pred_3gram_tweets.feather")
