# make n-gram models

# library(tidyverse)
# library(stringr)
# library(data.table)
# library(quanteda)

my_preprocess <- function(x) {
    require(stringi)
    
    punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'
    punct <- stri_replace_first_regex(punct, "'", "")

    x <- iconv(x, from = "latin1", to = "ASCII", sub = "")
    x <- stri_replace_all_regex(x, punct, "")
}

my_tokenizer <- function(x) {
    tokenizers::tokenize_words(x)
}

make_dtm_count <- function(x, ngram = 1L) {
    require(tidyverse)
    require(quanteda)
    require(text2vec)
    require(doParallel)
    registerDoParallel()
    
    profanity <- read_lines("data/external/profanity.txt") 
    
    it <- itoken(x, tokenizer = my_tokenizer)
    
    vocab <- create_vocabulary(
        it, 
        ngram = c(ngram, ngram), 
        stopwords = profanity
    ) 
        # prune_vocabulary(term_count_min = term_min)
    
    vect <- vocab_vectorizer(vocab)

    dtm <- create_dtm(it, vect) %>%
        colSums()
    
    # tfidf <- TfIdf$new()
    # nrml_dtm <- tfidf$fit_transform(dtm)
    # 
    # list(dtm, nrml_dtm)
}

make_token_files <- function(x, y) {
    require(tidyverse)
    require(quanteda)
    require(stringi)
    
    punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'
    punct <- stri_replace_first_regex(punct, "'", "")
    
    sentences <- read_rds(x) %>%
        tokenize("sentence", simplify = TRUE, verbose = TRUE) %>%
        iconv(from = "latin1", to = "ASCII", sub = "") 
        # stri_replace_all_regex(punct, "")
    
    uni <- make_dtm_count(sentences, 1L)
    bi <- make_dtm_count(sentences, 2L) 
    tri <- make_dtm_count(sentences, 3L) 
    
    write_rds(uni, paste0("data/tidy/tokens_", y, "1.Rds"), compress = "gz")
    write_rds(bi, paste0("data/tidy/tokens_", y, "2.Rds"), compress = "gz")
    write_rds(tri, paste0("data/tidy/tokens_", y, "3.Rds"), compress = "gz")
}

convert_tokens <- function(src) {
    require(tidyverse)

    uni <- read_rds(paste0("data/tidy/tokens_", src, "1.Rds"))
    tbl1 <- make_token_tables(uni, 1)
    write_rds(tbl1, paste0("data/final/tokens_", src, "1.Rds"), compress = "gz")
    rm(uni)

    bi <- read_rds(paste0("data/tidy/tokens_", src, "2.Rds"))
    tbl2 <- make_token_tables(bi, 2)
    write_rds(tbl2, paste0("data/final/tokens_", src, "2.Rds"), compress = "gz")
    rm(bi)
    
    tri <- read_rds(paste0("data/tidy/tokens_", src, "3.Rds"))
    tbl3 <- make_token_tables(tri, 3)
    write_rds(tbl3, paste0("data/final/tokens_", src, "3.Rds"), compress = "gz")
    rm(tri)
    
    disc <- make_discount_table(tbl1$count1, tbl2$count2, tbl3$count3)
    write_rds(disc, paste0("data/final/discount_table_", src, ".Rds"), compress = "gz")
}

make_token_tables <- function(x, i) {
    require(data.table)
    require(stringi)

    if (i == 1) {
        tbl <- data.table(word1 = names(x), count1 = x, key = "word1")
        tbl <- tbl[stri_detect_charclass(word1, "[:digit:]", negate = TRUE)]
    } else if (i == 2) {
        tbl <- data.table(words = names(x), count2 = x)
        tbl <- tbl[stri_detect_charclass(words, "[:digit:]", negate = TRUE)
                   ][, c("word1", "word2") := tstrsplit(words, "_", fixed = TRUE), by = words
                     ][word1 != "" & word2 != ""
                       ][, words := NULL]
        setkey(tbl, word1, word2)
        
        tbl <- tbl[word1 != "" & word2 != ""]
        # [stri_detect_regex(word1, punct, negate = TRUE) & stri_detect_regex(word2, punct, negate = TRUE)]
        
    } else if (i == 3) {
        tbl <- data.table(words = names(x), count3 = x)
        tbl <- tbl[stri_detect_charclass(words, "[:digit:]", negate = TRUE)
                   ][, c("word1", "word2", "word3") := tstrsplit(words, "_", fixed = TRUE), by = words
                     ][word1 != "" & word2 != "" & word3 != ""
                       ][, words := NULL]
              # [nchar(word3) > 1 & !(word3 %in% quanteda::stopwords()) & !stringr::str_detect(word3, "[:digit:]")]
        setkey(tbl, word1, word2)
    }

    tbl
}

make_discount_table <- function(x, y, z) {
    require(tidyverse)

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
    
}

combine_tokens <- function(n) {
    require(tidyverse)
    require(data.table)

    x <- read_rds(paste0("data/final/tokens_blogs", n, ".Rds"))
    y <- read_rds(paste0("data/final/tokens_news", n, ".Rds"))
    z <- read_rds(paste0("data/final/tokens_tweets", n, ".Rds"))
    
    # e <- environment()
    # my_files <- list.files("data/final", paste0("tokens_(blogs|news|tweets)", n), full.names = TRUE)
    # nm <- list.files("data/final", paste0("tokens_(blogs|news|tweets)", n))
    # nm <- stringr::str_replace_all(nm, ".Rds", "")
    # files <- map(my_files, read_rds)
    # files <- map(files, as.data.table)
    # names(files) <- nm
    # list2env(files, e)
    # rm(files)
    
    if (n == 1) {
        setnames(x, "count1", "count1_blogs")
        setnames(y, "count1", "count1_news")
        setnames(z, "count1", "count1_tweets")
        
        tbl <- merge(x, y, by = "word1", all = TRUE) %>%
            merge(z, by = "word1", all = TRUE)
        tbl <- tbl[, .(count1 = sum(count1_blogs, count1_news, count1_tweets, na.rm = TRUE)), by = word1]
        # setkey(tbl, word1)
        
    } else if (n == 2) {
        setnames(x, "count2", "count2_blogs")
        setnames(y, "count2", "count2_news")
        setnames(z, "count2", "count2_tweets")
        
        tbl <- merge(x, t, by = c("word1", "word2"), all = TRUE) %>%
            merge(z, by = c("word1", "word2"), all = TRUE)
        tbl <- tbl[, .(count2 = sum(count2_blogs, count2_news, count2_tweets, na.rm = TRUE)), by = c("word1", "word2")]
        # setkey(tbl, word1, word2)
        
    } else if (n == 3) {
        setnames(x, "count3", "count3_blogs")
        setnames(y, "count3", "count3_news")
        setnames(z, "count3", "count3_tweets")

        tbl <- merge(x, y, by = c("word1", "word2", "word3"), all = TRUE) %>%
            merge(z, by = c("word1", "word2", "word3"), all = TRUE)
        tbl <- tbl[, .(count3 = sum(count3_blogs, count3_news, count3_tweets, na.rm = TRUE)), by = c("word1", "word2", "word3")]
        # setkey(tbl, word1, word2)
    }
    
    write_rds(tbl, paste0("data/tidy/tokens_all", n, ".Rds"))
}

trim_tokens <- function(n) {
    require(tidyverse)
    require(data.table)

    x <- read_rds("data/tidy/tokens_all1.Rds") 
    y <- read_rds("data/tidy/tokens_all2.Rds")
    z <- read_rds("data/tidy/tokens_all3.Rds")
    
    x <- x[count1 > n]
    y <- y[count2 > n]
    z <- z[count3 > n]
    
    write_rds(x, "data/final/tokens_all1.Rds")
    write_rds(y, "data/final/tokens_all2.Rds")
    write_rds(z, "data/final/tokens_all3.Rds")
    
    d <- make_discount_table(x$count1, y$count2, z$count3)
    write_rds(d, "data/final/discount_table_all.Rds")
}

# scripts ----------------------------------------------

make_token_files("data/tidy/train_blogs.Rds", "blogs")
make_token_files("data/tidy/train_news.Rds", "news")
make_token_files("data/tidy/train_tweets.Rds", "tweets")

convert_tokens("blogs")
convert_tokens("news")
convert_tokens("tweets")

combine_tokens(1)
combine_tokens(2)
combine_tokens(3)

trim_tokens(3)

# library(tidyverse)
# library(data.table)

# trim_tokens(x, y, z, 3)

# d <- make_discount_table(x$count1, y$count2, z$count3)
# write_rds(d, "data/final/discount_table_all.Rds")


