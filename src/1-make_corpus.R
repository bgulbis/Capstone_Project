# make corpus

library(tidyverse)
library(stringr)
library(tm)
library(SnowballC)

profanity <- read_lines("data/external/profanity.txt")

#' This function reads in a text file, creates a corpus and Document Term
#' Matrix, and saves the files as Rds objects.
#' 
#' @param raw name of the text file to read in
#' @param name name to append to each saved data file
#' @param dir directory where the data files are saved; defaults to data/tidy
make_corpus <- function(raw, name, dir = "data/tidy") {
    x <- read_lines(raw) %>%
        str_to_lower()
    
    keep <- map_lgl(x, ~ runif(1) >= 0.9)
    
    x <- x[keep]
    
    write_rds(x, paste0(dir, "/", name, ".Rds"))
    
    corpus_x <- Corpus(VectorSource(x)) %>%
        tm_map(removePunctuation) %>%
        tm_map(removeNumbers) %>%
        tm_map(removeWords, c(profanity, stopwords("english"))) %>%
        tm_map(stemDocument)
    
    write_rds(corpus_x, paste0(dir, "/corpus_", name, ".Rds"))

    dtm_x <- DocumentTermMatrix(corpus_x)
    
    write_rds(dtm_x, paste0(dir, "/dtm_", name, ".Rds"))
}

# read text --------------------------------------------

make_corpus("data/raw/en_US.blogs.txt.gz", "blogs")
make_corpus("data/raw/en_US.news.txt.gz", "news") 
make_corpus("data/raw/en_US.twitter.txt.gz", "tweets") 
