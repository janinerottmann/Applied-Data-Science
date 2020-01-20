# Problem Set 2
# Question 2


library(tidyverse)
library(rvest)

#---------------------------------------------1a)-----------------------------------------------

# scrape 1000 latest deals. 
# Not working. Differing number of rows on every page. Probably wrong nodes selected.
# Show first 10 deals of every page instead. (implies missing deals between pages)

url_base = 'https://www.mydealz.de/?page=%d'
read_html(url_base)
i = 1:100
sprintf(url_base,i) -> allURL


getDeals = function(url){
  
  read_html(url) -> data

  data %>%
    html_nodes('.thread-title--list') %>%
    html_text() %>%
    head(10) -> title

  data %>%
    html_nodes('.thread--deal .border--color-borderGrey') %>%
    html_text() %>%
    head(10)-> temperature

  data %>%
    html_nodes('.thread-username') %>%
    html_text() %>%
    head(10)-> author

  data %>%
    html_nodes('.boxAlign-jc--all-c') %>%
    html_attr('href') %>%
    head(10)-> deepLink

  data %>%
    html_nodes('.footerMeta-actionSlot .btn--mode-boxSec') %>%
    html_text() %>%
    head(10)-> numberOfComments
  
  Deals <- data.frame(title, temperature, author, deepLink, numberOfComments)
  return(Deals)
  
}

map_df(allURL,getDeals) -> Deals


#--------------------------------------------1b)-----------------------------------------------

# No arcane symbols since encoded with UTF-8. 
# Suggested solution by stack overflow:

# Deals %>%
#   mutate(temperature = iconv(.$temperature, 'latin1', 'ASCII'),
#          title = iconv(.$title, 'latin1', 'ASCII'))-> Deals


# clean temperature column
cleanDataframe = function(df) {
  
  df %>%
    mutate(temperature = str_remove(temperature,'°Abgelaufen')) %>%
    mutate(temperature = str_remove(temperature, '°')) %>%
    mutate(temperature = as.numeric(temperature)) -> df
  
  return(df)
}

cleanDataframe(Deals) -> Deals


#--------------------------------------------1c)-----------------------------------------------

# ----------temperature share----------------

getTemperatureShare = function(df){
 
   df %>%
    mutate(Hot = if_else(temperature > 0, 1, 0),
           Cold = if_else(temperature < 0, 1, 0)) %>%
    summarise(ShareHot = sum(Hot),
              ShareCold = sum(Cold),
              MeanTemperature = mean(temperature)) -> temperatureShare
  
  return(temperatureShare)
}

getTemperatureShare(Deals) -> temperatureShare


# ----------comments by temperature---------

getCommentsByTemperature = function(df) {
 
   df %>%
    mutate(numberOfComments = as.numeric(numberOfComments)) %>%
    mutate(HotComments  = if_else(temperature > 0, numberOfComments, 0),
           ColdComments = if_else(temperature < 0, numberOfComments, 0)) %>%
    summarise(AvgHotComments  = mean(HotComments),
              AvgColdComments = mean(ColdComments)) -> CommentsByTemperature
  
  return(CommentsByTemperature)
}

getCommentsByTemperature(Deals) -> CommentsByTemperature


#-----------deals by author----------------

getDealsByAuthor = function(df){
  
  df %>%
    mutate(count = 1) %>%
    group_by(author) %>%
    summarise(dealsByAuthor = sum(count)) -> DealsByAuthor
  
  #--------max deals--------
  DealsByAuthor %>%
    arrange(-dealsByAuthor) %>%
    head(1) -> max
  
  #--------mean deals--------
  DealsByAuthor %>%
    summarise(meanDeals = mean(dealsByAuthor)) -> mean
  
  #--------merge-------------
  max %>%
    merge(mean) -> merged
  
  return(merged)
}

getDealsByAuthor(Deals)


#-----------share of Xiaomi products------

product = 'Xiaomi'

getProductDeals = function(df){
  
  df %>%
    filter(grepl(pattern = product, .$title, ignore.case = T)) -> ProductDeals
  
  return(ProductDeals)
}

getProductDeals(Deals) -> ProductDeals






