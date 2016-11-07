# make_sample
# 
# make a sample of each of the 3 text data files

library(tidyverse)

read_sample <- function(x, n = 10000) {
    con <- file(x, open = "r")
    head <- readLines(con, 1)
    sampdat <- readLines(con, n)
    k <- n
    while (length(curline <- readLines(con, 1))) {
        k <- k + 1
        if (runif(1) < n/k) {
            sampdat[sample(n, 1)] <- curline
        }
    }
    close(con)

    sampdat    
}

blogs <- read_sample("data/raw/en_US.blogs.txt.gz")
news <- read_sample("data/raw/en_US.news.txt.gz")
tweets <- read_sample("data/raw/en_US.twitter.txt.gz")
