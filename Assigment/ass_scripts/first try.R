##Load all our data
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)



gminy <- st_read(here::here("ass_data", 
                                  "geo", 'jednostki_administracyjne',
                                  "Gminy.shp"))# %>%st_transform(., 27700)

wojewodztwa<- st_read(here::here("ass_data", 
                                   "geo", 'jednostki_administracyjne',
                                   "Wojewodztwa.shp"))# %>%st_transform(., 27700)


powiaty<- st_read(here::here("ass_data", 
                                 "geo", 'jednostki_administracyjne',
                                 "Powiaty.shp"))# %>%st_transform(., 27700)

country<- st_read(here::here("ass_data", 
                             "geo", 'jednostki_administracyjne',
                             "Panstwo.shp"))# %>%st_transform(., 27700)






plot(Jednostki_ewidencyjne$geometry)