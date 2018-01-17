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

library(igraph)
library(ggraph)

set.seed(1234)
