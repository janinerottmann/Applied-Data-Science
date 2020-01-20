# Problem Set 1
# Question 1


#### Set Working Directory ####

getwd()
setwd("/Users/janine/Dropbox/Master/1. Semester/ADS/Repositories/ADS19/Problem Sets/01/Data") 

#### Task ####

# You are helping a regional car dealer’s marketing department. 
# Create banners like the following.

#   **************************
#   * Toyota Corolla         *
#   * Horsepower: 47         *
#   * Cylinders: 4           *
#   * Fuel Efficiency: 34mpg *
#   * 1/4 mile time: 19sec   *
#   **************************

#### Libraries ####

library(tidyverse)
library(stats)



#### Aufgabe a) ####

# Create single row data.frame equivalent to the internal data set mtcars.

df <- mtcars
df$names <- rownames(df)

x <- sample(ncol(df),1)

createAd <- function(x){
  return(df[x, ])
}

createAd(x)

#### Aufgabe b) ####

# Using sprintf or paste, length and repeat format your output analogue to the above.

createFormattedAd <- function(x){
  line1 <- sprintf("* %s",rownames_to_column(df)[x,1])
  line2 <- sprintf("* Horsepower: %s",df[x,4])
  line3 <- sprintf("* Cylinders: %s", df[x,2])
  line4 <- sprintf("* Fuel Efficiency: %s mpg", df[x,1])
  line5 <- sprintf("* 1/4 mile time: %s sec",df[x,7])
  lines <- c(line1, line2, line3, line4, line5)
  line6 <- paste(rep("*",max(nchar(lines))), collapse = "")
  lines_ins <- c(line6,lines,line6)
  return(tibble(lines_ins))
}

createFormattedAd(6)

#### Aufgabe c) ####

# To improve marketing chances, the vendor wants to include a relative ranking.
# If the fuel efficiency rating, horsepower or quarter mile time is in the top 10% of the data set: 
# include “(Top x%)” behind the corresponding entry.

createFormattedAdWithComparison <- function(x){
  
  # ---- create df with selected car data only ----
  seleted.car <- df[x, ]
  
  # ----- hp ----  
  perc <- ecdf(df[,'hp'])(seleted.car['hp'])
  perc.neu <- (1-perc)*100
  
  if(perc.neu <= 10){
    top.10.hp <- floor(perc.neu)
    top.string.hp <- sprintf("(Top %s%s)", top.10.hp, "%")
    line2 <- sprintf("* Horsepower: %s%s",df[x, ]$hp,top.string.hp)
  }
  else{
    line2 <- sprintf("* Horsepower: %s",df[x, ]$hp)
  }
  
  # ----- mpg -----
  perc <- ecdf(df[,'mpg'])(seleted.car['mpg'])
  perc.neu <- (1-perc)*100
  
  if(perc.neu <= 10){
    top.10.mpg <- floor(perc.neu)
    top.string.mpg <- sprintf("(Top %s%s)", top.10.mpg, "%")
    line4 <- sprintf("* Fuel Efficiency: %s mpg %s", df[x, ]$mpg, top.string.mpg)
  }
  else{
    line4 <- sprintf("* Fuel Efficiency: %s mpg", df[x, ]$mpg)
  }
  
  # ----- qsec -----
  perc <- ecdf(df[,'qsec'])(seleted.car['qsec'])
  perc.neu <- (1-perc)*100
  
  if(perc.neu <= 10){
    top.10.qsec <- floor(perc.neu)
    top.string.qsec <- sprintf("(Top %s%s)", top.10.qsec, "%")
    line5 <- sprintf("* 1/4 mile time: %s sec %s",df[x, ]$qsec, top.string.qsec)
  }
  else{
    line5 <- sprintf("* 1/4 mile time: %s sec",df[x, ]$qsec)
  }
  
  line1 <- sprintf("* %s",df[x, ]$names)
  line3 <- sprintf("* Cylinders: %s", df[x, ]$cyl)
  lines <- c(line1, line2, line3, line4, line5)
  line6 <- paste(rep("*",max(nchar(lines))), collapse = "")
  lines_ins <- c(line6,lines,line6)
  return(tibble(lines_ins))
}

createFormattedAdWithComparison(x)


#### Aufgabe d) ####

# To automate the campaign, expand your function to take a data.frame with multiple cars.
# Add an integer argument n which specifies the number of ads that should be created.
# Then randomly sample n rows and create the ads for these vehicles.
# Watch out not to create the same ad twice

n <- 4 # --> integer argument which specifies the number of ads that should be created
random_cars <- sample(nrow(mtcars), n, replace = F) # --> create random sample of n cars
random_cars # --> check if function provides random sample of n cars.

map(random_cars,createFormattedAd) # --> for every dimension in random_cars apply function createAutomatedAd (Aufgabe c))

#### Aufgabe e) ####

# Combine the provided data files carMileage.csv and carPrices.csv with mtcars.
# Include unformatted price and mileage statements in your Ad function.
# Create a vector or list with all the ads of cars in this expanded mtcars.

cars_Mileage <- read.csv2("carMileage.csv", header = T, sep = ",")
cars_Prices <- read.csv2("carPrices.csv", header = T, sep = ",")

createExtendedAd <- function(x){
  a <- head(sort(mtcars$mpg, decreasing = TRUE), 4)
  b <- ifelse(x >= a[4], " (Top 10%)", "")
  c <- head(sort(mtcars$hp, decreasing = TRUE), 4)
  d <- ifelse(x >= c[4], " (Top 10%)", "")
  e <- head(sort(mtcars$qsec, decreasing = FALSE), 4)
  f <- ifelse(x >= e[4], " (Top 10%)", "")
  
  line1 <- sprintf("* %s",rownames_to_column(mtcars)[x,1])
  line2 <- sprintf("* Horsepower: %s%s",mtcars[x,4], d)
  line3 <- sprintf("* Cylinders: %s", mtcars[x,2])
  line4 <- sprintf("* Fuel Efficiency: %s mpg %s", mtcars[x,1],b)
  line5 <- sprintf("* 1/4 mile time: %s sec %s",mtcars[x,7], f)
  line6 <- sprintf("* Mileage: %s", cars_Mileage[x,2])
  line7 <- sprintf("* Price: %s", cars_Prices[x,2])
  
  lines <- c(line1, line2, line3, line4, line5, line6, line7)
  line0 <- paste(rep("*",max(nchar(lines))), collapse = "")
  lines_ins <- c(line0,lines,line0)
  
  return(tibble(lines_ins))
}

n <- 4 
random_cars <- sample(1:32, n, replace = F) 

map(random_cars,createExtendedAd)

