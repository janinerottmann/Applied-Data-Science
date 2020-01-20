# Problem Set 3
# Question 1

#---------------------1a---------------------
library(tidyverse)

url = 'https://www.bundeswahlleiter.de/dam/jcr/5441f564-1f29-4971-9ae2-b8860c1724d1/ew19_kerg2.csv'
data <- read_csv2(url, skip = 9)

data %>%
  filter(Gebietsart == 'Land',
         Gruppenart == 'Partei') -> election

#---------------------1b--------------------

election %>%
  filter(Gruppenname %in% c('CDU','CSU', 'SPD', 'AfD', 'DIE LINKE', 'GRÜNE', 'FDP')) %>%
  group_by(Gebietsname, Gruppenname) %>%
  summarise(Prozent)  -> ShareOfVotes

party_colors <- c(
  'AfD'= 'blue',
  'CDU'= 'black',
  'CSU'='black',
  'DIE LINKE'='purple',
  'FDP'='yellow',
  'GRÜNE'='seagreen',
  'SPD'='red')

ShareOfVotes %>%
  ggplot(aes(x = as.factor(Gebietsname), y = Prozent, fill = Gruppenname)) +
  geom_col() +
  scale_fill_manual(values= party_colors) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('Gebietsname')

ShareOfVotes %>%
  ggplot() +
  geom_col(mapping = aes(x = Gruppenname, y = Prozent, fill = Gruppenname)) +
  facet_wrap(~ Gebietsname, nrow = 4) +
  scale_fill_manual(values= party_colors) +
  theme(axis.text.x = element_text(angle = 90))

#---------------------1c--------------------------

ShareOfVotes %>%
  ggplot(aes(x = Gebietsname, y = Prozent)) +
  geom_col( aes(fill = Gruppenname)) +
  facet_wrap(~ Gruppenname, nrow = 7) +
  scale_fill_manual(values= party_colors) +
  theme(axis.text.x = element_text(angle = 90))

#--------------------1d---------------------

library(raster)
library(rvest)
library(leaflet)

#winner of each state
ShareOfVotes %>%
  filter(Prozent == max(Prozent)) -> Winner

#geografic data
germany_states = getData('GADM', country = 'Germany', level = 1)

#merge
merge(germany_states, Winner, by.x = 'NAME_1', by.y = 'Gebietsname') -> WinnersPerState

#set aestetics
pal <- colorFactor( palette = c('blue','black','grey', 'purple','yellow','seagreen','red'),
                    levels = c('AfD', 'CDU', 'CSU', 'DIE LINKE', 'FDP', 'GRÜNE', 'SPD'))

polygon_popup <- paste0('<strong>Name: </strong>', WinnersPerState$NAME_1, '<br>',
                        '<strong>Wahlsieger: </strong>', WinnersPerState$Gruppenname,'<br>',
                        '<strong>Prozent: </strong>', WinnersPerState$Prozent,'<br>')

#leaflet map
leaflet() %>%
  addTiles() %>%
  addPolygons(data = WinnersPerState, 
              fillOpacity = 0.4,
              weight = 2,
              fillColor = ~pal(Gruppenname),
              popup = polygon_popup,
              color = "black")

#----------------------1e----------------------

#winner of each constituency
data %>%
  filter(Gebietsart == 'Kreis',
         Gruppenart == 'Partei',
         Gruppenname %in% c('AfD', 'CDU', 'CSU', 'DIE LINKE', 'FDP', 'GRÜNE', 'SPD')) %>%
  group_by(Gebietsname, Gruppenname) %>%
  summarise(Prozent) %>%
  filter(Prozent == max(Prozent)) -> WinnerConstituency

#geografic data
germany_constituency = getData('GADM', country = 'Germany', level = 2)

#merge
merge(germany_constituency, WinnerConstituency, by.x = 'NAME_2', by.y = 'Gebietsname') -> WinnersPerConstituency

#leaflet map
leaflet() %>%
  addTiles() %>%
  addPolygons(data = WinnersPerConstituency, 
              fillOpacity = ~Prozent/50,
              weight = 2,
              fillColor = ~pal(Gruppenname),
              popup = polygon_popup,
              color = "black")


