# Problem Set 2
# Question 1

# Name: Janine Rottmann
# Matrikelnummer: 1979840

library(tidyverse)
library(rvest)


#------------------------------------------1a)--------------------------------------------------

url = 'http://quotes.toscrape.com/' 

getQuotes = function(url){
  
  read_html(url) -> data
  
  data %>%
    html_nodes('.text') %>%
    html_text() -> quotes
  
  data %>%
    html_nodes('.author') %>%
    html_text() -> author
  
  data %>%
    html_nodes('.tags') %>%
    html_text() -> tags
  
  Quotes <- data.frame(quotes, author, tags)
  return(Quotes)
  
}

Quotes <- getQuotes(url)

#-------------------------------------------1b)-----------------------------------------------------

#scrape next 10 pages by altering url 
url_base = 'http://quotes.toscrape.com/page/%d/' 
i = 1:10
sprintf(url_base,i) -> allURL

#apply function to all 10 pages
map_df(allURL,getQuotes) -> Quotes


#-------------------------------------------1c)----------------------------------------------------

#get authors URL by scraping authors link and pasting to base_link
getAuthorsURL = function(url){
  
  read_html(url) -> data

  data %>%
    html_nodes('.text') %>%
    html_text() -> quotes
  
  data %>%
    html_nodes('.author') %>%
    html_text() -> author
  
  data %>%
    html_nodes('.tags') %>%
    html_text() -> tags
  
  data %>%
    html_nodes('.quote span a') %>%
    html_attr('href') -> AuthorPage
    
  base_link = 'http://quotes.toscrape.com/'
  paste0(base_link, AuthorPage) -> AuthorsURL
  
  Quotes <- data.frame(quotes, author, tags, AuthorsURL)
  return(Quotes)
}

#apply function to all 10 pages
map_df(allURL,getAuthorsURL) -> Quotes

#-------------------------------------- 1d)-------------------------------------------------------

#scrape author details unsing 1 author URL
getDetailsAuthor = function(url){
  
  read_html(url) -> data
  
  data %>%
    html_nodes('.author-title') %>%
    html_text() -> author
  
  data %>%
    html_nodes('.author-description') %>%
    html_text() -> description
  
  data %>%
    html_nodes('.author-born-date') %>%
    html_text() -> bornDate
  
  Author <- data.frame(author, description, bornDate)
  return(Author)
}

#apply function to all authors URLs
allAuthors <- Quotes$AuthorsURL
map_df(as.character(allAuthors),getDetailsAuthor) -> AuthorDetails

#reduce duplicates to one row for each author
AuthorDetails %>%
  unique() %>%
  arrange(author) -> AuthorDetails

#---------------------------------------- 1e)------------------------------------------

#-------seperate bornDate------------------------
AuthorDetails %>%
  separate(bornDate, c('Month', 'Day', 'Year')) %>%
  select(author, description, Day, Month, Year) -> AuthorDetails

#-------authors born in the 19th century---------
AuthorDetails %>%
  filter(Year >= 1800,
         Year <= 1899) %>%
  select(author, Year) %>%
  arrange(Year) -> Born19thCentury

#-------most quotes by author--------------------
Quotes %>%
  mutate(count = 1) %>%
  group_by(author) %>%
  summarise(QuotesByAuthor = sum(count)) %>%
  arrange(-QuotesByAuthor) -> QuotesByAuthor

QuotesByAuthor %>%
  head(1)

#-------average quote by author------------------

QuotesByAuthor %>%
  summarise(AvgNumberQuotes = mean(QuotesByAuthor))

#-------'life' tag------------------------------

Quotes %>%
  filter(grepl(pattern = 'life', .$tags)) -> QuotesWithLifeTag

#-------join both dataframes--------------------

#remove Whitespace in authors column (necessary for merging)    
Quotes %>%
  mutate(author = str_trim(.$author)) -> Quotes 
AuthorDetails %>%
  mutate(author = str_trim(.$author)) -> AuthorDetails 

#merge dataframes by author
Quotes %>%
  merge(.,AuthorDetails, by.x = 'author', by.y = 'author', all = T) -> mergedDataframe





