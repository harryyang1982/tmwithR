library(tidyverse)
library(tidytext)
library(janeaustenr)

# Tokenizing by N-gram

austen_bigrams <- austen_books() %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

## Counting and Filtering N-grams

austen_bigrams %>% 
  count(bigram, sort = T)

bigrams_separated <- austen_bigrams %>% 
  separate(bigram, c("word1", "word2", sep = " "))

bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

bigrams_filtered

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = T)

bigram_counts

bigrams_united <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = " ")

bigrams_united

trigram_filtered <- austen_books() %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word)

trigram_filtered

trigram_united <- trigram_filtered %>% 
  unite(trigram, word1, word2, word3, sep = " ")

trigram_united


## Analyzing Bigrams

bigrams_filtered %>% 
  filter(word2 == "street") %>% 
  count(book, word1, sort = T)

bigram_tf_idf <- bigrams_united %>% 
  count(book, bigram) %>% 
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf))

bigram_tf_idf

bigram_tf_idf %>% 
  group_by(book) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  mutate(bigram = reorder(bigram, tf_idf)) %>% 
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = F) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

trigram_tf_idf <- trigram_united %>% 
  count(book, trigram) %>% 
  bind_tf_idf(trigram, book, n) %>% 
  arrange(desc(tf_idf))

trigram_tf_idf

trigram_tf_idf %>% 
  group_by(book) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  mutate(trigram = reorder(trigram, tf_idf)) %>% 
  ggplot(aes(trigram, tf_idf, fill = book)) +
  geom_col(show.legend = F) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

## Using Bigrams to Provide Context in Sentiment Analysis

bigrams_separated %>% 
  filter(word1 == "not") %>% 
  count(word1, word2, sort = T)

AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word2, score, sort = TRUE) %>% 
  ungroup()

not_words

not_words %>% 
  mutate(contribution = n * score) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = F) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

negation_words <- c("not", "no", "never", "without")
negated_words <- bigrams_separated %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word1, word2, score, sort = T) %>% 
  ungroup()

negated_words

negated_plots <- negated_words %>% 
  mutate(contribution = n * score,
         word1 = factor(word1)) %>% 
  group_by(word1) %>% 
  top_n(20, abs(contribution)) %>% 
  ungroup()

negated_plots

negated_plots %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = F) +
  xlab("Words preced by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  facet_wrap(~word1, ncol = 2, scales = "free") +
  coord_flip()

## Visualizing a Network of Bigrams with ggraph

library(igraph)

bigram_counts

bigram_graph <- bigram_counts %>% 
  filter(n > 20) %>% 
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = F,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Visualizing Bigrams in Other Texts

count_bigrams <- function(dataset) {
  dataset %>% 
    unnest_tokens(bigram, text, token = "ngrams", n =2) %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>% 
    count(word1, word2, sort = T)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

library(gutenbergr)
kjv <- gutenberg_download(10)

kjv_bigrams <- kjv %>% 
  count_bigrams()

kjv_bigrams %>% 
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>% 
  visualize_bigrams()

# Counting and Correlating Pairs of Words with the widyr Package

austen_section_words <- austen_books() %>% 
  filter(book == "Pride & Prejudice") %>% 
  mutate(section = row_number() %/% 10) %>% 
  filter(section > 0) %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word)

library(widyr)

word_pairs <- austen_section_words %>% 
  pairwise_count(word, section, sort = T)

word_pairs

word_pairs %>% 
  filter(item1 == "darcy")

## Examining Pairwise Correlation

word_cors <- austen_section_words %>% 
  group_by(word) %>% 
  filter(n() >= 20) %>% 
  pairwise_cor(word, section, sort = T)

word_cors

word_cors %>% 
  filter(item1 == "pounds")

word_cors %>% 
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>% 
  group_by(item1) %>% 
  top_n(6) %>% 
  ungroup() %>% 
  mutate(item2 = reorder(item2, correlation)) %>% 
  ggplot(aes(item2, correlation)) +
  geom_col() +
  facet_wrap(~item1, scales = "free") +
  coord_flip()

set.seed(2016)

word_cors %>% 
  filter(correlation > .15) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = F) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = T) +
  theme_void()
