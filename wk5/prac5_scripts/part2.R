##Load all our data
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)


#London Borough data is already in 277000
Londonborough <- st_read(here::here("Prac5_data",
                                    "statistical-gis-boundaries-london", 
                                    "ESRI", 
                                    "London_Borough_Excluding_MHW.shp"))%>%
  st_transform(., 27700)


#rightmove data
Rightmove_2 <- read_csv("prac5_data/rightmove_05-11-2020.csv",) %>% 
  st_as_sf(.,coords= c("longitude","latitude"), crs = 4326) %>%
  st_transform(., 27700) %>% 
  

