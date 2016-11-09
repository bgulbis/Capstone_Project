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
#' @param perc percent of lines to keep in corpus; defaults to 0.1 (10%)
make_corpus <- function(raw, name, dir = "data/tidy", perc = 0.1) {
    x <- read_lines(raw) %>%
        str_to_lower()
    
    keep <- map_lgl(x, ~ runif(1) >= 1 - perc)
    
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

make_corpus("data/raw/en_US.blogs.txt.gz", "blogs", perc = 0.03)
make_corpus("data/raw/en_US.news.txt.gz", "news", perc = 0.03) 
make_corpus("data/raw/en_US.twitter.txt.gz", "tweets", perc = 0.03) 

blogs <- read_lines("data/raw/en_US.blogs.txt.gz") 
blogs3 <- readLines("data/raw/en_US.blogs.txt.gz")

y <- str_detect(blogs, "\\\xdf")

x <- which(str_detect(blogs, "So little A, who's 7 now"))

blogs2 <- str_replace_all(blogs3, "[^[:graph:]]", " ")

blogs2[x]

iconv(blogs2[x], "UTF-8", "UTF-8", sub = "")

blogs[x]

# corp <- Corpus(VectorSource(blogs))
corp <- Corpus(DirSource("data/raw", pattern = "en_US.blogs.txt.gz", encoding = "ASCII", mode = "text"))

corp[[1]]$content[x]

t <- tm_map(corp, function(y) iconv(enc2utf8(y), sub = "byte"))

low <- tm_map(corp, tolower)

x <- blogs

corp2 <- Corpus(VectorSource(x))
low2 <- tm_map(corp, tolower)

dtm <- DocumentTermMatrix(corp)

x <- str_detect(blogs, "[^[:alnum:]]")
y <- grepl("[^[:alnum:]]", blogs)

    str_replace_all("[^[:alnum:]]", " ")
