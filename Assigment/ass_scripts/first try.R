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


lesserpoland <- wojewodztwa %>% 
  filter(JPT_NAZWA_=='małopolskie')

powiaty_LP <- powiaty %>% 
  filter(str_detect(JPT_KOD_JE,'^12'))

gminy_LP <- gminy %>% 
  filter(str_detect(JPT_KOD_JE,'^12'))


#csv 

furnaces <- read_csv(here::here("ass_data", 
                                "csv", 
                                "furnaces.csv")) %>% 
              select(area_code,area_name,podregion,powiat_name,
                     typ_gminy,'2013','2014','2015','2016',
                     '2017','2018') %>% 
              drop_na(area_code)


furnaces_final <- furnaces %>% 
  mutate(type_n=case_when(str_detect(typ_gminy, "City|Urban$") ~ "1",
                          str_detect(typ_gminy, "Urban-rural") ~ "3",
                          str_detect(typ_gminy, "Rural") ~ "2")) %>% 
  mutate(area_code=paste(area_code, type_n,sep=""))



gminy_LP_fur <- gminy_LP %>%
  left_join(.,furnaces_final,
            by = c("JPT_KOD_JE" = "area_code"))


# quick plot
tmap_mode("view")

tm_shape(lesserpoland) +
  tm_polygons(col = NA, alpha = 1) +
  tm_shape(powiaty_LP) +
  tm_polygons(col = NA, alpha=0.5) +
  tm_shape(gminy_LP_fur) +
  tm_polygons(col = "2018", breaks=c(0,25,50,75,100,150,200,250,300,500,1000,2000,4000),alpha=0.5) #seq(100, 4300 ,by=10)
