# Preprocessing

library(tidyverse)

training_folder <- "data/20news-bydate/20news-bydate-train/"

read_folder <- function(infolder) {
  data_frame(file = dir(infolder, full.names = T)) %>% 
    mutate(text = map(file, read_lines)) %>% 
    transmute(id = basename(file), text) %>% 
    unnest(text)
}

raw_text <- data_frame(folder = dir(training_folder, full.names = T)) %>% 
  unnest(map(folder, read_folder)) %>% 
  transmute(newsgroup = basename(folder), id, text)
