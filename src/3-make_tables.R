# make n-gram models

# library(tidyverse)
# library(stringr)
# library(data.table)
# library(quanteda)

my_tokenizer <- function(x) {
    quanteda::tokenize(
        x,
        removeNumbers = TRUE, 
        removePunct = TRUE, 
        removeSymbols = TRUE, 
        removeTwitter = TRUE, 
        removeURL = TRUE
    )
}

make_dtm_count <- function(x, ngram = 1L, term_min = 1L) {
    require(tidyverse)
    require(quanteda)
    require(text2vec)
    require(doParallel)
    registerDoParallel()
    
    profanity <- read_lines("data/external/profanity.txt") 
    
    it <- itoken(x, toLower, my_tokenizer)
    
    vocab <- create_vocabulary(
        it, 
        ngram = c(ngram, ngram), 
        stopwords = profanity
    ) %>%
        prune_vocabulary(term_count_min = term_min)
    
    vect <- vocab_vectorizer(vocab)

    dtm <- create_dtm(it, vect)
    
    tfidf <- TfIdf$new()
    nrml_dtm <- tfidf$fit_transform(dtm)

    list(dtm, nrml_dtm)
}

make_token_files <- function(x, y) {
    require(tidyverse)
    require(quanteda)
    
    sum_dtm <- function(z) {
        colSums(z) %>%
            ceiling()
    }
    sentences <- read_rds(x) %>%
        tokenize("sentence", simplify = TRUE, verbose = TRUE) 
    
    uni <- make_dtm_count(sentences, 1L, term_min = 10L) %>%
        map(sum_dtm)
    
    bi <- make_dtm_count(sentences, 2L, term_min = 5L) %>%
        map(sum_dtm)

    tri <- make_dtm_count(sentences, 3L, term_min = 3L) %>%
        map(sum_dtm)

    make_token_tables(uni[[1]], bi[[1]], tri[[1]], y)
    make_token_tables(uni[[2]], bi[[2]], tri[[2]], paste0(y, "_nrml"))
    make_discount_table(uni[[1]], bi[[1]], tri[[1]], y)
    make_discount_table(uni[[2]], bi[[2]], tri[[2]], paste0(y, "_nrml"))
}

make_token_tables <- function(x, y, z, nm) {
    require(data.table)
    require(feather)
    
    uni <- data.table(word1 = names(x), count1 = x)
    
    bi <- data.table(words = names(y), count2 = y)
    bi[, c("word1", "word2") := tstrsplit(words, "_", fixed = TRUE), by = words]
    bi[, words := NULL]
    
    tri <- data.table(words = names(z), count3 = z)
    tri[, c("word1", "word2", "word3") := tstrsplit(words, "_", fixed = TRUE), by = words]
    tri[, words := NULL]
    
    write_feather(uni, paste0("data/final/tokens_", nm, "1.feather"))
    write_feather(bi, paste0("data/final/tokens_", nm, "2.feather"))
    write_feather(tri, paste0("data/final/tokens_", nm, "3.feather"))
}

make_discount_table <- function(x, y, z, nm) {
    require(tidyverse)
    require(feather)
    
    gt_1gram <- table(x) %>%
        as_tibble() %>%
        rename(Var1 = x, uni = n) 
    
    gt_2gram <- table(y) %>%
        as_tibble() %>%
        rename(Var1 = y, bi = n) 
    
    gt_3gram <- table(z) %>%
        as_tibble() %>%
        rename(Var1 = z, tri = n) 
    
    gt_freq <- full_join(gt_1gram, gt_2gram, by = "Var1") %>%
        full_join(gt_3gram, by = "Var1") %>%
        rename(count = Var1) %>%
        dmap_at("count", as.integer) %>%
        arrange(count) %>%
        mutate(uni_next = if_else(lead(count) == count + 1, lead(uni), 0L),
               bi_next = if_else(lead(count) == count + 1, lead(bi), 0L),
               tri_next = if_else(lead(count) == count + 1, lead(tri), 0L)) %>%
        dmap(~ coalesce(.x, 0L)) 
    
    write_feather(gt_freq, paste0("data/final/discount_table_", nm, ".feather"))
}

combine_tokens <- function(nrml = "") {
    require(tidyverse)
    require(data.table)
    require(feather)
    
    my_files <- list.files("data/final", paste0("tokens_(blogs|news|tweets)", nrml, "[1-3]"), full.names = TRUE)
    nm <- list.files("data/final", paste0("tokens_(blogs|news|tweets)[1-3]"))
    nm <- stringr::str_replace_all(nm, ".feather", "")
    files <- map(my_files, read_feather)
    names(files) <- nm
    list2env(files, .GlobalEnv)
    rm(files)
    
    uni <- rbind(tokens_blogs1, tokens_news1, tokens_tweets1) %>% 
        as.data.table()
    uni <- uni[, .(count1 = sum(count1)), by = "word1"]

    bi <- rbind(tokens_blogs2, tokens_news2, tokens_tweets2) %>%
        as.data.table()
    bi <- bi[, .(count2 = sum(count2)), by = c("word1", "word2")]
    
    tri <- rbind(tokens_blogs3, tokens_news3, tokens_tweets3) %>%
        as.data.table()
    tri <- tri[, .(count3 = sum(count3)), by = c("word1", "word2", "word3")]
    
    make_discount_table(uni$count1, bi$count2, tri$count3, paste0("all", nrml))
}

# scripts ----------------------------------------------

make_token_files("data/tidy/train_blogs.Rds", "blogs")
make_token_files("data/tidy/train_news.Rds", "news")
make_token_files("data/tidy/train_tweets.Rds", "tweets")


combine_tokens()
combine_tokens("_nrml")

# x <- list.files("data/tidy", "tokens_", full.names = TRUE)
# nm <- list.files("data/tidy", "tokens_")
# nm <- str_replace_all(nm, ".Rds", "")
# files <- map(x, read_rds)
# names(files) <- nm
# list2env(files, .GlobalEnv)
# rm(files)
# 
# make_token_tables(tokens_blogs1, tokens_blogs2, tokens_blogs3, "blogs")
# make_token_tables(tokens_news1, tokens_news2, tokens_news3, "news")
# make_token_tables(tokens_tweets1, tokens_tweets2, tokens_tweets3, "tweets")
# 
# tokens_all1 <- combine_tokens(tokens_blogs1, tokens_news1, tokens_tweets1)
# write_rds(tokens_all1, "data/tidy/tokens_all1.Rds")
# 
# tokens_all2 <- combine_tokens(tokens_blogs2, tokens_news2, tokens_tweets2)
# write_rds(tokens_all2, "data/tidy/tokens_all2.Rds")
# 
# tokens_all3 <- combine_tokens(tokens_blogs3, tokens_news3, tokens_tweets3)
# write_rds(tokens_all3, "data/tidy/tokens_all3.Rds")
# 
# make_token_tables(tokens_all1, tokens_all2, tokens_all3, "all")
# 
# make_token_tables(tokens_blogs_nrml1, tokens_blogs_nrml2, tokens_blogs_nrml3, "blogs_nrml")
# make_token_tables(tokens_news_nrml1, tokens_news_nrml2, tokens_news_nrml3, "news_nrml")
# make_token_tables(tokens_tweets_nrml1, tokens_tweets_nrml2, tokens_tweets_nrml3, "tweets_nrml")
# 
# tokens_all_nrml1 <- combine_tokens(tokens_blogs_nrml1, tokens_news_nrml1, tokens_tweets_nrml1)
# write_rds(tokens_all_nrml1, "data/tidy/tokens_all_nrml1.Rds")
# 
# tokens_all_nrml2 <- combine_tokens(tokens_blogs_nrml2, tokens_news_nrml2, tokens_tweets_nrml2)
# write_rds(tokens_all_nrml2, "data/tidy/tokens_all_nrml2.Rds")
# 
# tokens_all_nrml3 <- combine_tokens(tokens_blogs_nrml3, tokens_news_nrml3, tokens_tweets_nrml3)
# write_rds(tokens_all_nrml3, "data/tidy/tokens_all_nrml3.Rds")
# 
# make_token_tables(tokens_all_nrml1, tokens_all_nrml2, tokens_all_nrml3, "all_nrml")
# 
# make_discount_table(tokens_blogs1, tokens_blogs2, tokens_blogs3, "blogs")
# make_discount_table(tokens_news1, tokens_news2, tokens_news3, "news")
# make_discount_table(tokens_tweets1, tokens_tweets2, tokens_tweets3, "tweets")
# make_discount_table(tokens_all1, tokens_all2, tokens_all3, "all")
# make_discount_table(tokens_blogs_nrml1, tokens_blogs_nrml2, tokens_blogs_nrml3, "blogs_nrml")
# make_discount_table(tokens_news_nrml1, tokens_news_nrml2, tokens_news_nrml3, "news_nrml")
# make_discount_table(tokens_tweets_nrml1, tokens_tweets_nrml2, tokens_tweets_nrml3, "tweets_nrml")
# make_discount_table(tokens_all_nrml1, tokens_all_nrml2, tokens_all_nrml3, "all")
