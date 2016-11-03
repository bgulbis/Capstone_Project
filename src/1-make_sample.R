# make_sample
# 
# make a sample of each of the 3 text data files

library(tidyverse)

# make a file connection
con <- file("data/raw/en_US.blogs.txt.gz", "r")

readsizeof <- 20000
nooflines <- 0

while((linesread <- length(readLines(con, readsizeof))) > 0) {
    nooflines <- nooflines + linesread
} 
close(con)
nooflines
x <- sample(1:nooflines, nooflines * 0.1)
y <- order(x)
# check encoding, should be UTF-8
# guess_encoding("data/raw/en_US.blogs.txt.gz")
# guess_encoding("data/raw/en_US.news.txt.gz")
# guess_encoding("data/raw/en_US.twitter.txt.gz")

blog <- read_lines("data/raw/en_US.blogs.txt.gz", n_max = 10, skip = 5)
close(con)
