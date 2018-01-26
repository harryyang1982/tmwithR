# Preprocessing

library(tidyverse)

training_folder <- "data/20news-bydate/20news-bydate-train/"

# Define a function to read all files from a folder into a data frame
read_folder <- function(infolder) {
  data_frame(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}

raw_text <- data_frame(folder = dir(training_folder, full.names = TRUE)) %>%
  unnest(map(folder, read_folder)) %>%
  transmute(newsgroup = basename(folder), id, text)

raw_text

raw_text %>% 
  group_by(newsgroup) %>% 
  summarise(messages = n_distinct(id)) %>% 
  ggplot(aes(newsgroup, messages)) +
  geom_col() +
  coord_flip()

## Preprocessing Text

cleaned_text <- raw_text %>% 
  group_by(newsgroup, id) %>% 
  filter(cumsum(text == "") >0,
         cumsum(str_detect(text, "^--")) == 0) %>% 
  ungroup()

cleaned_text <- cleaned_text %>% 
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "",
         !str_detect(text, "writes(:|\\.\\.\\.)$"),
         !str_detect(text, "^In article <"),
         !id %in% c(9704, 9985))

library(tidytext)

usenet_words <- cleaned_text %>% 
  unnest_tokens(word, text) %>% 
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

# Words in Newsgroups

usenet_words %>% 
  count(word, sort = T)

words_by_newsgroup <- usenet_words %>% 
  count(newsgroup, word, sort = T) %>% 
  ungroup()

words_by_newsgroup

## Finding tf-idf Within Newsgroups

tf_idf <- words_by_newsgroup %>% 
  bind_tf_idf(word, newsgroup, n) %>% 
  arrange(desc(tf_idf))

tf_idf

tf_idf %>% 
  filter(str_detect(newsgroup, "^sci\\.")) %>% 
  group_by(newsgroup) %>% 
  top_n(12, tf_idf) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf, fill = newsgroup)) +
  geom_col(show.legend = F) +
  facet_wrap(~ newsgroup, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

library(widyr)

newsgroup_cors <- words_by_newsgroup %>% 
  pairwise_cor(newsgroup, word, n, sort = T)

newsgroup_cors

library(ggraph)
library(igraph)
set.seed(2017)

newsgroup_cors %>% 
  filter(correlation > .4) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = T) +
  theme_void()

## Topic Modeling

word_sci_newsgroups <- usenet_words %>% 
  filter(str_detect(newsgroup, "^sci")) %>% 
  group_by(word) %>% 
  mutate(word_total = n()) %>% 
  ungroup() %>% 
  filter(word_total > 50)

word_sci_newsgroups

sci_dtm <- word_sci_newsgroups %>% 
  unite(document, newsgroup, id) %>% 
  count(document, word) %>% 
  cast_dtm(document, word, n)

library(topicmodels)
sci_lda <- LDA(sci_dtm, k = 4, control = list(seed = 2016))

sci_lda %>% 
  tidy() %>% 
  group_by(topic) %>% 
  top_n(8, beta) %>% 
  ungroup() %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free_y") +
  coord_flip()

sci_lda %>% 
  tidy(matrix = "gamma") %>% 
  separate(document, c("newsgroup", "id"), sep = "_") %>% 
  mutate(newsgroup = reorder(newsgroup, gamma * topic)) %>% 
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~newsgroup) +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")

# Sentiment Analysis

newsgroup_sentiments <- words_by_newsgroup %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(newsgroup) %>% 
  summarise(score = sum(score * n) / sum(n))

newsgroup_sentiments %>% 
  mutate(newsgroup = reorder(newsgroup, score)) %>% 
  ggplot(aes(newsgroup, score, fill = score > 0)) +
  geom_col(show.legend = F) +
  coord_flip() +
  ylab("Average sentiment score")

## Sentiment Analysis by Word

contributions <- usenet_words %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(word) %>% 
  summarise(occurences = n(),
            contribution = sum(score))

contributions

contributions %>% 
  top_n(25, abs(contribution)) %>% 
  mutate(word = reorder(word, contribution)) %>% 
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = F) +
  coord_flip()

top_sentiment_words <- words_by_newsgroup %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  mutate(contribution = score * n / sum(n))

top_sentiment_words

top_sentiment_words %>% 
  group_by(newsgroup) %>% 
  top_n(12, abs(contribution)) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, contribution)) %>% 
  ggplot(aes(word, contribution, fill = score > 0)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~newsgroup) +
  ylab("Sentiment score * # of occurrences")

## Sentiment Analysis by Message

sentiment_messages <- usenet_words %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(newsgroup, id) %>% 
  summarise(sentiment = mean(score),
            words = n()) %>% 
  ungroup() %>% 
  filter(words >= 5)

sentiment_messages %>% 
  arrange(desc(sentiment))

print_message <- function(group, message_id) {
  result <- cleaned_text %>%
    filter(newsgroup == group, id == message_id, text != "")
  
  cat(result$text, sep = "\n")
}

print_message("rec.sport.hockey", 53560)

sentiment_messages %>% 
  arrange(sentiment)

print_message("rec.sport.hockey", 53907)

## N-gram Analysis

usenet_bigrams <- cleaned_text %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

usenet_bigrams_counts <- usenet_bigrams %>% 
  count(newsgroup, bigram, sort = T) %>% 
  ungroup() %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

negate_words <- c("not", "without", "no", "can't", "don't", "won't")

usenet_bigrams_counts %>% 
  filter(word1 %in% negate_words) %>% 
  count(word1, word2, wt = n, sort = T) %>% 
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>% 
  mutate(contribution = score * nn) %>% 
  group_by(word1) %>% 
  top_n(10, abs(contribution)) %>% 
  ungroup() %>% 
  mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>% 
  ggplot(aes(word2, contribution, fill = contribution >0)) +
  geom_col(show.legend = F) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by a negation") +
  ylab("Sentiment score * # of occurrences") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()
