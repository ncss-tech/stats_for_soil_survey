library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

x <- readLines('winter-2018-pre-course-survey.txt')

x <- as.data.frame(x, stringsAsFactors = FALSE)
names(x) <- 'text'


words <- x %>% unnest_tokens(word, text)

data("stop_words")
words <- words %>%
  anti_join(stop_words)

words %>%  count(word, sort = TRUE) 


words %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# https://www.tidytextmining.com/ngrams.html

# ngrams <- x %>% unnest_tokens(ngrams, text, token = "ngrams", n = 3)
# 
# ngrams %>%  count(ngrams, sort = TRUE) 
# 
# ngrams_separated <- ngrams %>%
#   separate(ngrams, c("word1", "word2", "word3"), sep = " ")
# 
# ngrams_filtered <- ngrams_separated %>%
#   filter(!word1 %in% stop_words$word) %>%
#   filter(!word2 %in% stop_words$word) %>%
#   filter(!word3 %in% stop_words$word)
# 
# ngram_counts <- ngrams_filtered %>% 
#   count(word1, word2, word3, sort = TRUE)
# 
# ngram_counts
# 
# ngrams_united <- ngram_counts %>%
#   unite(ngram, word1, word2, word3, sep = " ")
# 
# 
# ngrams_united %>%
#   count(ngram, sort = TRUE) %>%
#   # filter(n > 1) %>%
#   # mutate(ngram = reorder(ngram, n)) %>%
#   ggplot(aes(ngram, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip()



