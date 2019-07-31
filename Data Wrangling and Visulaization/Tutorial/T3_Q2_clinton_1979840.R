# Problem Set 3
# Question 2

# Name: Janine Rottmann
# Matrikelnummer: 1979840

library(jsonlite)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(RCurl)

#extract data
if (!file.exists("clinton_emails.rda")) {
  clinton_emails <- fromJSON("http://graphics.wsj.com/hillary-clinton-email-documents/api/search.php?subject=&text=&to=&from=&start=&end=&sort=docDate&order=desc&docid=&limit=27159&offset=0")$rows
  save(clinton_emails, file="clinton_emails.rda")
}

#load data
load("clinton_emails.rda")

#transform data
clinton_emails %>% 
  mutate(from=trimws(from),
         to=trimws(to)) %>% 
  filter(from != "") %>% 
  filter(to != "") %>% 
  filter(!grepl(";", from)) %>% 
  filter(!grepl(";", to)) -> clinton_emails

#-------------------2a-----------------------

#count number of mails
clinton_emails %>%
  group_by(from, to) %>%
  summarise(weight = n()) %>% 
  ungroup() -> numberOfMails

#transform into table graph
numberOfMails %>%
  arrange(-weight) %>%
  as_tbl_graph(directed = F) %>%
  activate(nodes) %>%
  ggraph() + 
  geom_edge_link(alpha = 0.1) + 
  geom_node_point() +
  geom_node_text(aes(label = name))

#-------------------2b-----------------------

numberOfMails %>%
  filter(from == 'Hillary Clinton') -> fromClinton

numberOfMails %>%
  filter(to == 'Hillary Clinton') -> toClinton

fromClinton %>%
  bind_rows(toClinton) %>%
  arrange(-weight) -> onlyClintonMails

onlyClintonMails %>%
  as_tbl_graph(directed = F) %>%
  activate(nodes) %>%
  ggraph() + 
  geom_edge_link(alpha = 0.1) + 
  geom_node_point() +
  geom_node_text(aes(label = name))

#--------------------2c----------------------

as_tbl_graph(onlyClintonMails, directed = F) %>%
  mutate(neighbors = centrality_degree(),
         group = group_infomap(),
         keyplayer = node_is_keyplayer(k = 10),
         centrality = centrality_authority()) %>% 
  activate(edges) %>% 
  filter(!edge_is_multiple()) -> table_graph1

layout1 <- create_layout(table_graph1,layout = "fr")

ggraph(layout1) + 
  geom_edge_density() +
  geom_edge_link(alpha = 0.2) + 
  geom_node_point(aes(size = log(centrality))) +
  geom_node_text(aes(filter=centrality > 0.051, label = name),repel = T, color = 'red')

#---------------------2d----------------------

#most frequently contacted persons & persons Clinton contacted most frequently
onlyClintonMails %>%
  gather(x, name, from:to) %>%
  group_by(name) %>%
  summarise(sum_weight = sum(weight)) %>%
  ungroup() %>%
  arrange(desc(sum_weight)) -> MostFrequentlyContacted

#transform into table graph
as_tbl_graph(onlyClintonMails, directed = FALSE) %>%
  mutate(neighbors = centrality_degree(),
         group = group_infomap(),
         keyplayer = node_is_keyplayer(k = 10),
         centrality = centrality_authority()) %>% 
  left_join(MostFrequentlyContacted) %>%
  activate(edges) %>% 
  filter(!edge_is_multiple()) -> table_graph2

#create layout
layout2 <- create_layout(table_graph2,layout = 'sphere')

#plot network graph
ggraph(layout2) + 
  geom_edge_density() +
  geom_edge_link(aes(width = weight),alpha = 0.2) + 
  geom_node_point(aes(size=log(sum_weight))) +
  geom_node_text(aes(filter=centrality > 0.051, label = name),repel = T, color = 'red') +
  theme_graph() +
  theme(legend.position = "none")











