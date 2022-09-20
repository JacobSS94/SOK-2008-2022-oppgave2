install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readr")
install.packages("plyr")
install.packages("dplyr")

#setwd("FILEPATH")
setwd("~/SKOLE/SOK- 2008 - DEN NORDIKSE MODELLEN")

library(readr) 
library(ggplot2)     
library(tidyverse)  

union<- read_csv("https://uit-sok-2008-h22.github.io/Assets/union_unempl.csv") 
View(union) 

union$country <- gsub("United Kingdom", "UK", union$country)
View(union) 

# Renaming a variable. The below code renames the variable "Country" to "Region".
names(union)[names(union) == "country"] <- "region"
View(union) 

# Creating a new variable. To create a map showing "Excess coverage", you need to create a new variable. The below code shows how to create a new variable in R. 
union$newvar2<-union$var1 + union$var2 #A sum
union$newvar3<-(union$var1 + union$var2)/2 # A mean value


# Oppgave 1 
mapdata <- map_data("world") 
mapdata <- left_join(mapdata, union, by="region")

mapdata1 <- mapdata %>% filter(!is.na(mapdata$unempl))

map1 <- ggplot(mapdata1, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=unempl), color = "black")+
  scale_fill_gradient(name = "Arbeidsledighetsrate", low = "red", high = "green")
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map1

# Oppgave 2 

# kart over fagforeningsdensitet  
map2 <- ggplot(mapdata1, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=density), color = "black")+
  scale_fill_gradient(name = "Fagforeningsdensitet", low = "red", high = "green")
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map2

# kart over Excess coverage
mapdata1$excess_coverage<-mapdata1$coverage - mapdata1$density

map3 <- ggplot(mapdata1, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=excess_coverage), color = "black")+
  scale_fill_gradient(low = "red", high = "green")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map3

# kart over Koordinering av lønnsfastsettelse
map4 <- ggplot(mapdata1, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=coord), color = "black")+
  scale_fill_brewer(name = "koordinering av lønnsfastsettelse", palette = "Set1")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map4