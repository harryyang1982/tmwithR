library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)

text_raw <- readLines("j.txt")

txt_df <- tibble(document = 1,
                 text=text_raw)

txt_df <- txt_df %>% 
  unnest_tokens(word, text) %>% 
  count(word)
  
myCorpus_ <- Corpus(VectorSource(txt_df$word))
myCorpus_ <- tm_map(myCorpus_, removePunctuation)
myCorpus_ <- tm_map(myCorpus_, removeNumbers)
myCorpus_ <- tm_map(myCorpus_, tolower)

myStopwords <- c(stopwords("english"), "rt")
myCorpus_ <- tm_map(myCorpus_, removeWords, myStopwords)

mydtm <- DocumentTermMatrix(myCorpus_, control = list(wordLengths = c(2, Inf)))

rowTotals <- apply(mydtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- mydtm[rowTotals> 0, ]           #remove all docs without words

my_lda <- LDA(dtm.new, k = 10, control = list(seed = 2018))

lda_topics <- tidy(my_lda, matrix = "beta")

lda_top_terms <- lda_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

lda_top_terms %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
