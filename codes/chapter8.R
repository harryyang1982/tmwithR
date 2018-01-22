# How Data Is Oragnized at NASA

library(jsonlite)
metadata <- fromJSON("https://data.nasa.gov/data.json")

class(metadata$dataset$title)
class(metadata$dataset$description)
class(metadata$dataset$keyword)

## Wrangling and Tidying the Data

library(tidyverse)

nasa_title <- tibble(id = metadata$dataset$`_id`$`$oid`,
                     title = metadata$dataset$title)
nasa_title

nasa_desc <- tibble(id = metadata$dataset$`_id`$`$oid`,
                    desc = metadata$dataset$description)

nasa_desc %>% 
  select(desc) %>% 
  sample_n(5)

nasa_keyword <- tibble(id = metadata$dataset$`_id`$`$oid`,
                       keyword = metadata$dataset$keyword) %>% 
  unnest(keyword)

nasa_keyword

library(tidytext)

nasa_title <- nasa_title %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words)

nasa_desc <- nasa_desc %>% 
  unnest_tokens(word, desc) %>% 
  anti_join(stop_words)

nasa_title
nasa_desc

## Some Initial Simple Exploration

nasa_title %>% 
  count(word, sort = T)

nasa_desc %>% 
  count(word, sort = T)

my_stopwords <- data_frame(word = c(as.character(1:10),
                                    "v1", "v03", "l2", "l3", "l4", "v5.2.0",
                                    "v003", "v004", "v005", "v006", "v7"))

nasa_title <- nasa_title %>% 
  anti_join(my_stopwords)

nasa_desc <- nasa_desc %>% 
  anti_join(my_stopwords)

nasa_keyword %>% 
  group_by(keyword) %>% 
  count(sort = T)

nasa_keyword <- nasa_keyword %>% 
  mutate(keyword = toupper(keyword))

nasa_keyword %>% 
  count(keyword, sort = T)

# Word Co-occurrences and Correlations

## Networks of Description and Title Words

library(widyr)

title_word_pairs <- nasa_title %>% 
  pairwise_count(word, id, sort = T, upper = F)

title_word_pairs
desc_word_pairs <- nasa_desc %>% 
  pairwise_count(word, id, sort = T, upper = F)

desc_word_pairs

library(igraph)
library(ggraph)

set.seed(1234)
title_word_pairs %>% 
  filter(n >= 250) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

set.seed(1234)
desc_word_pairs %>% 
  filter(n >= 5000) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
<<<<<<< HEAD
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
=======
  geom_edge_link(aes(edge_alpha = n, edge_width =n), edge_colour = "darkred") +
>>>>>>> 7363f62c1d2b0346a68f72b87c7749d6e437fcd2
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

## Networks of Keywords

keyword_pairs <- nasa_keyword %>% 
  pairwise_count(keyword, id, sort = T, upper = F)

<<<<<<< HEAD
keyword_pairs

set_seed(1234)
=======
set.seed(1234)
>>>>>>> 7363f62c1d2b0346a68f72b87c7749d6e437fcd2
keyword_pairs %>% 
  filter(n >= 700) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
<<<<<<< HEAD
  geom_edge_link(aes(edge_alpha = n, edge_width = n),
                 edge_colour = "royalblue") +
=======
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
>>>>>>> 7363f62c1d2b0346a68f72b87c7749d6e437fcd2
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

keyword_cors <- nasa_keyword %>% 
  group_by(keyword) %>% 
  filter(n() >= 50) %>% 
  pairwise_cor(keyword, id, sort = T, upper = F)

keyword_cors

set.seed(1234)
keyword_cors %>% 
  filter(correlation > .6) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation),
                 edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# Calculating tf-idf for the Description Fields

<<<<<<< HEAD
## What Is tf-idf for the Description Field Words?
=======
## What is tf-idf for the Description Field Words?
>>>>>>> 7363f62c1d2b0346a68f72b87c7749d6e437fcd2

desc_tf_idf <- nasa_desc %>% 
  count(id, word, sort = T) %>% 
  ungroup() %>% 
  bind_tf_idf(word, id, n)

desc_tf_idf %>% 
  arrange(-tf_idf) %>% 
  select(-id)

## Connecting Description Fields to Keywords

desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword, by = "id")
<<<<<<< HEAD

desc_tf_idf %>% 
  filter(!near(tf, 1)) %>% 
  filter(keyword %in% c("SOLAR ACTIVITY", "CLOUDS",
                        "SEISMOLOGY", "ASTROPHYSICS",
                        "HUMAN HEALTH", "BUDGET")) %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(keyword) %>% 
  distinct(word, keyword, .keep_all = T) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  ggplot(aes(word, tf_idf, fill = keyword)) +
  geom_col(show.legend = F) +
  facet_wrap(~keyword, ncol = 3, scales = "free") +
  coord_flip() +
  labs(title = "Highest tf-idf words in NASA metadata description fields",
       caption = "NASA metadata from https://data.nasa.gov/data.json",
       x = NULL, y = "tf-idf")

# Topic Modeling

## Casting to a Document-Term Matrix

my_stop_words <- bind_rows(stop_words,
                           data_frame(word = c("nbsp", "amp", "gt", "lt",
                                               "timesnewromanpsmt", "font",
                                               "td", "li", "br", "tr", "quot",
                                               "st", "img", "src", "strong",
                                               "http", "file", "files",
                                               as.character(1:12)),
                                      lexicon = rep("custom", 30)))

word_counts <- nasa_desc %>% 
  anti_join(my_stop_words) %>% 
  count(id, word, sort = T) %>% 
  ungroup()

word_counts

desc_dtm <- word_counts %>% 
  cast_dtm(id, word, n)

desc_dtm

## Ready for Topic Modeling

library(topicmodels)

desc_lda <- LDA(desc_dtm, k = 24, control = list(seed = 1234))
desc_lda

## Interpreting the Topic Model

tidy_lda <- tidy(desc_lda)
tidy_lda

top_terms <- tidy_lda %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

top_terms

top_terms %>% 
  mutate(term = reorder(term, beta)) %>% 
  group_by(topic, term) %>% 
  arrange(desc(beta)) %>% 
  ungroup() %>% 
  mutate(term = factor(paste(term, topic, sep = "__"),
                       levels = rev(paste(term, topic, sep = "__")))) %>% 
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legedn = F) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~topic, ncol = 3, scales = "free")

lda_gamma <- tidy(desc_lda, matrix = "gamma")

lda_gamma

ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = F) +
  facet_wrap(~topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))

## Connecting Topic Modeling with Keywords

lda_gamma <- full_join(lda_gamma, nasa_keyword, by = c("document" = "id"))
lda_gamma

top_keywords <- lda_gamma %>% 
  filter(gamma > 0.9) %>% 
  count(topic, keyword, sort = T)

top_keywords

top_keywords %>% 
  group_by(topic) %>% 
  top_n(5, n) %>% 
  group_by(topic, keyword) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(keyword = factor(paste(keyword, topic, sep = "__"),
                          levels = rev(paste(keyword, topic, sep = "__")))) %>% 
  ggplot(aes(keyword, n, fill = as.factor(topic))) +
  geom_col(show.legend = F) +
  labs(title = "Top keywords for each LDA topic",
       x = NULL, y = "Number of documents") +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~topic, ncol = 3, scales = "free")

=======
>>>>>>> 7363f62c1d2b0346a68f72b87c7749d6e437fcd2
