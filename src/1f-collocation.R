library(tidyverse)
library(quanteda)

# collocation ------------------------------------------

size = 100
blogs <- read_lines("data/tidy/train_blogs.Rds") %>%
    corpus() %>%
    sample(size = size) %>%
    tokenize("sentence", simplify = TRUE)

blogs_col <- collocations(blogs, size = 3)

library(wordVectors)
