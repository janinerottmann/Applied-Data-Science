# Problem Set 4
# Question 2

# Name: Janine Rottmann
# Matrikelnummer: 1979840

#----------------------------2a--------------------------------

library(tidyverse)
library(foreign)
library(tidytext)
library(topicmodels)
library(ggplot2)


setwd("./Tutorial 4/bbc/")
files = list.files(recursive = TRUE, pattern = "*.txt")

getDocument = function(x){
  paste(unlist(scan(x, what="character",quiet = T)),collapse = " ")
}

getTopic = function(x){
  paste(unlist(strsplit(x, split='/', fixed=TRUE))[1])
}

file = data.frame(document=1:2225,
                  text = map_chr(files, getDocument),
                  topic = map_chr(files, getTopic))

file$topic = as.character(file$topic)
file$text = as.character(file$text)

file %>%
  unnest_tokens(., output = words,
                   input = text, 
                   token = "words", 
                   to_lower = TRUE, 
                   drop = TRUE,
                   collapse = NULL) -> wordsByTopic

#-------------------------------2b----------------------------------

data("stop_words")

wordsByTopic %>%
  anti_join(stop_words, by = c('words' = 'word')) %>%
  group_by(topic) %>%
  count(words, sort = TRUE) %>%
  mutate(inverseTermFrequency = n/sum(n)) -> wordsFrequency


#------------------------------2c-----------------------------------

#prepare Term Document Matrix
wordsFrequency %>%
  cast_dtm(term = words, document = topic, value = n) -> TermDocumentMatrix

#create LDA Model with 5 topics
LDA <- LDA(TermDocumentMatrix, k = 5, control = list(seed = 1234))

LDA_tidy <- tidy(LDA)

#top 5 terms by topic
top_terms <- LDA_tidy %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#visualization
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x") +
  theme(axis.text.x = element_text(angle=60, hjust=1))






