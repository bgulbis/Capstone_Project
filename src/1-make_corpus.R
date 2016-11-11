# make corpus

library(tidyverse)
library(stringr)
library(tm)
library(SnowballC)

profanity <- read_lines("data/external/profanity.txt")

#' This function reads in a text file, creates a corpus and Document Term 
#' Matrix, and saves the files as Rds objects.
#' 
#' @param pattern name of the text file to read in
#' @param name name to append to each saved data file
#' @param read_dir directory where the raw data files are located; defaults to
#'   data/raw
#' @param save_dir directory where the data files should be saved; defaults to
#'   data/tidy
#' @param perc percent of lines to keep in corpus; defaults to 0.1 (10%)
make_corpus <- function(pattern, name, read_dir = "data/raw", save_dir = "data/tidy", perc = 0.1) {
    corp <- Corpus(DirSource("data/raw", pattern = pattern, encoding = "UTF-8", mode = "text")) %>%
        tm_map(content_transformer(function(x) iconv(x, from = "latin1", to = "ASCII", sub = " "))) %>%
        # tm_map(content_transformer(function(x) iconv(x, to = "UTF-8", sub = " "))) %>%
        tm_map(content_transformer(tolower)) %>%
        tm_map(removeWords, profanity) %>%
        tm_map(removePunctuation) %>%
        tm_map(removeNumbers) %>% 
        tm_map(stemDocument) %>%
        tm_map(stripWhitespace)
    
    # tm_map(content_transformer(stringr::str_to_lower))
    # keep <- map_lgl(x, ~ runif(1) >= 1 - perc)
    # x <- x[keep]
    
    write_rds(corp, paste0(save_dir, "/corpus_", name, ".Rds"))

    # dtm_x <- DocumentTermMatrix(corpus_x)
    # 
    # write_rds(dtm_x, paste0(dir, "/dtm_", name, ".Rds"))
}

# read text --------------------------------------------

make_corpus("en_US.blogs.txt.gz", "blogs", perc = 0.03)
make_corpus("en_US.news.txt.gz", "news", perc = 0.03) 
make_corpus("en_US.twitter.txt.gz", "tweets", perc = 0.03) 

# tmp <- read_lines("data/raw/en_US.blogs.txt.gz")
# x <- which(str_detect(tmp, "So little A, who's 7 now"))

blogs <- read_rds("data/tidy/corpus_blogs.Rds")
tdm <- TermDocumentMatrix(blogs)
write_rds(tdm, "data/tidy/tdm_blogs.Rds")
rm(blogs)

news <- read_rds("data/tidy/corpus_news.Rds")
tdm <- TermDocumentMatrix(news)
write_rds(tdm, "data/tidy/tdm_news.Rds")
rm(news)

tweets <- read_rds("data/tidy/corpus_tweets.Rds")
tdm <- TermDocumentMatrix(tweets)
write_rds(tdm, "data/tidy/tdm_tweets.Rds")
rm(tweets)


