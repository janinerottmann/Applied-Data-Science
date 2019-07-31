# Problem Set 1
# Question 2

# Name: Janine Rottmann
# Matrikelnummer: 1979840

#### set Working Directory ####
getwd()
setwd("/Users/janine/Dropbox/Master/1. Semester/ADS/Repositories/ADS19/Problem Sets/01")

#### Task ####

# Transform data from google’s book API in Data Frame. Use the following Code:

URL = "https://www.googleapis.com/books/v1/volumes?q=george+r+r+martin&maxResults=40"
response_parsed <- fromJSON(getURL(URL,ssl.verifyhost = 0L, ssl.verifypeer = 0L))

#### Libraries ####

library(tidyverse)
library(RCurl)
library(RJSONIO)
library(data.table)


#### Aufgabe a) ####

#Describe the structure of the response object and explaine dimensions and nesting of the elements.

summary(response_parsed)

#### Aufgabe b) ####

# Extract the author, the title, publishing date and the rating of each book.

# for 1 item:
response_parsed$items[[1]]$volumeInfo$authors

# for all items: (or use map function instead)
authors <- lapply(response_parsed$items, function(x){x$volumeInfo$authors})
title <- lapply(response_parsed$items, function(x){x$volumeInfo$title})
publishing_date <- lapply(response_parsed$items, function(x){x$volumeInfo$publishedDate})
rating <- lapply(response_parsed$items, function(x){x$volumeInfo$averageRating})

#### Aufgabe c) ####

# Combine your individual calls in one function.

getBookList = function(numberOfItems) {
  df_books <- data.table(authors,title, publishing_date,rating,stringsAsFactors = FALSE)
  return(df_books)
}
getBookList(numberOfItems)

# Sort the list by date and title and return it.
# ERROR: argument is of unsupported type list.
df_books <- getBookList(x)
df_books %>%
  arrange(publishing_date, title)

#### Aufgabe d) ####

# Create a function which provided with a string argument specifying a book ID.
# This includes: where this book is available, price and a buy link.

getBookSalesInfo = function(response) {
  df_books_new <- data.table(authors, title, publishing_date, rating)
  df_books_new$ID <- lapply(response_parsed$items, function(x) {x$id})
  df_books_new$location <- lapply(response_parsed$items, function(x) {x$accessInfo$country})
  df_books_new$link <- lapply(response_parsed$items, function(x) {x$saleInfo$buyLink})
  df_books_new$price <- lapply(response_parsed$items, function(x) {x$saleInfo$listPrice$amount})
  return(df_books_new)
}
df_booksnew <- getBookSalesInfo(response)
df_booksnew

