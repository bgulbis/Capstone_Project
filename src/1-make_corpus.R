# make corpus

library(tidyverse)
library(stringr)
library(tm)
library(SnowballC)
library(slam)

#' This function takes the name of a file with text, reads it in and returns a
#' sample of the lines as a character vector
#' 
#' @param x
#'   
#' @return character vector
make_sample <- function(x) {
    tmp <- read_lines(x)
    set.seed(77123)
    keep <- sample.int(length(tmp), 0.1 * length(tmp))
    tmp[keep]
}


#' This function takes a character vector and creates a corpus, performing some
#' transformations on the words
#' 
#' @param x character vector
#' 
#' @return VCorpus object
make_corpus <- function(x) {
    Corpus(VectorSource(x)) %>%
        tm_map(content_transformer(function(y) iconv(y, from = "latin1", to = "ASCII", sub = " ")))
        # tm_map(content_transformer(tolower)) 
        # tm_map(removeWords, profanity) %>%
        # tm_map(removePunctuation) %>%
        # tm_map(removeNumbers) %>% 
        # tm_map(stemDocument) %>%
        # tm_map(stripWhitespace)
}

# sample text ------------------------------------------
blogs <- make_sample("data/raw/en_US.blogs.txt.gz")
news <- make_sample("data/raw/en_US.news.txt.gz") 
tweets <- make_sample("data/raw/en_US.twitter.txt.gz") 

# make corpus ------------------------------------------
corpus_blogs <- make_corpus(blogs)
corpus_news <- make_corpus(news)
corpus_tweets <- make_corpus(tweets)

# x <- which(str_detect(tmp, "So little A, who's 7 now"))

# Term Document Matrices -------------------------------
profanity <- read_lines("data/external/profanity.txt")
tdm_ctrl <- list(bounds = list(global = c(3, Inf)),
                 tolower = TRUE,
                 stopwords = profanity, 
                 removePunctuation = TRUE, 
                 removeNumbers = TRUE,
                 stemming = TRUE)

tdm_blogs <- TermDocumentMatrix(corpus_blogs, control = tdm_ctrl)
tdm_news <- TermDocumentMatrix(corpus_news, control = tdm_ctrl)
tdm_tweets <- TermDocumentMatrix(corpus_tweets, control = tdm_ctrl)

# word frequencies -------------------------------------
freq_blogs <- row_sums(tdm_blogs, na.rm = TRUE)
freq_news <- row_sums(tdm_news, na.rm = TRUE)
freq_tweets <- row_sums(tdm_tweets, na.rm = TRUE)

tbl_blogs <- as_tibble(freq_blogs) %>%
    rownames_to_column("word") %>%
    arrange(desc(value))

tbl_news <- as_tibble(freq_news) %>%
    rownames_to_column("word") %>%
    arrange(desc(value))

tbl_tweets <- as_tibble(freq_tweets) %>%
    rownames_to_column("word") %>%
    arrange(desc(value))

# save files -------------------------------------------

write_rds(corpus_blogs, "data/tidy/corpus_blogs_sample.Rds")
write_rds(corpus_news, "data/tidy/corpus_news_sample.Rds")
write_rds(corpus_tweets, "data/tidy/corpus_tweets_sample.Rds")

write_rds(tdm_blogs, "data/tidy/tdm_blogs_sample.Rds")
write_rds(tdm_news, "data/tidy/tdm_news_sample.Rds")
write_rds(tdm_tweets, "data/tidy/tdm_tweets_sample.Rds")

# library(wordcloud)
# wordcloud(names(freq), freq, min.freq = 2000)

