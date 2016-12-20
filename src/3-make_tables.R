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

    make_token_tables(uni, 1, y)
    make_token_tables(bi, 2, y)
    make_token_tables(tri, 3, y)

    make_discount_table(uni[[1]], bi[[1]], tri[[1]], y)
    make_discount_table(uni[[2]], bi[[2]], tri[[2]], paste0(y, "_nrml"))
}

make_token_tables <- function(x, i, nm) {
    require(data.table)
    require(feather)
    
    if (i == 1) {
        y <- data.table(word1 = names(x[[1]]), count1 = x[[1]])
        setkey(y, word1)
        z <- data.table(word1 = names(x[[2]]), nrml_count1 = x[[2]])
        setkey(z, word1)
        tbl <- y[z, nomatch = 0]
    } else if (i == 2) {
        y <- data.table(words = names(x[[1]]), count2 = x[[1]])
        setkey(y, words)
        z <- data.table(words = names(x[[2]]), nrml_count2 = x[[2]])
        setkey(z, words)
        tbl <- y[z, nomatch = 0]
        tbl[, c("word1", "word2") := tstrsplit(words, "_", fixed = TRUE), by = words]
        tbl[, words := NULL]
    } else if (i == 3) {
        y <- data.table(words = names(x[[1]]), count3 = x[[1]])
        setkey(y, words)
        z <- data.table(words = names(x[[2]]), nrml_count3 = x[[2]])
        setkey(z, words)
        tbl <- y[z, nomatch = 0]
        tbl[, c("word1", "word2", "word3") := tstrsplit(words, "_", fixed = TRUE), by = words]
        tbl[, words := NULL]
    }

    write_feather(tbl, paste0("data/final/tokens_", nm, i, ".feather"))
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

combine_tokens <- function() {
    require(tidyverse)
    require(data.table)
    require(feather)
    
    my_files <- list.files("data/final", "tokens_", full.names = TRUE)
    nm <- list.files("data/final", "tokens_")
    nm <- stringr::str_replace_all(nm, ".feather", "")
    files <- map(my_files, read_feather)
    names(files) <- nm
    list2env(files, .GlobalEnv)
    rm(files)
    
    uni <- rbind(tokens_blogs1, tokens_news1, tokens_tweets1) %>% 
        as.data.table()
    uni <- uni[, .(count1 = sum(count1), nrml_count1 = sum(nrml_count1)), by = "word1"]

    bi <- rbind(tokens_blogs2, tokens_news2, tokens_tweets2) %>%
        as.data.table()
    bi <- bi[, .(count2 = sum(count2), nrml_count2 = sum(nrml_count2)), by = c("word1", "word2")]
    
    tri <- rbind(tokens_blogs3, tokens_news3, tokens_tweets3) %>%
        as.data.table()
    tri <- tri[, .(count3 = sum(count3), nrml_count3 = sum(nrml_count3)), by = c("word1", "word2", "word3")]
    
    write_feather(uni, "data/final/tokens_all1.feather")
    write_feather(bi, "data/final/tokens_all2.feather")
    write_feather(tri, "data/final/tokens_all3.feather")
    
    make_discount_table(uni$count1, bi$count2, tri$count3, "all")
    make_discount_table(uni$nrml_count1, bi$nrml_count2, tri$nrml_count3, "all_nrml")
}

# scripts ----------------------------------------------

make_token_files("data/tidy/train_blogs.Rds", "blogs")
make_token_files("data/tidy/train_news.Rds", "news")
make_token_files("data/tidy/train_tweets.Rds", "tweets")
combine_tokens()
