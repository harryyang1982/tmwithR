library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(NLP4kec)

text <- readLines("ma_thesis_yang.txt")
text_par <- r_parser_r(text, language = "ko")

txt_df <- tibble(document = 1,
                 text=text_par)

corp <- VCorpus(VectorSource(text_par))
corp <- tm_map(corp, removePunctuation)
# corp <- tm_map(corp, removeNumbers)
# corp <- tm_map(corp, tolower)

myStopwords <- c("있다", "하다", "경우", "보다", "대하다", "되다", "이러하다", "관련", "않다", "없다", "같다", "주다", "오다", "그것", "이야기", "옮기다", "만들다", "통하다", "받다", "많다", "보내다", "드러내다", "때문", "문제", "그거", "우리", "연구자", "연구", "분석")

corp <- tm_map(corp, removeWords, myStopwords)


mydtm <- DocumentTermMatrix(corp, control = list(wordLengths = c(2, Inf)))

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
