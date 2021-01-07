##Load all our data
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
library(MazamaSpatialUtils)
library(MazamaCoreUtils)
library(maps)
library(sp)
library(PWFSLSmoke)
library(con2aqi)
library(ggmap)
library(ggplot2)
library(raster)
library(janitor)
library(gstat)
library(con2aqi)
library(broom)
library(dplyr)
library(elevatr)
library(jsonlite)
library(purrr)
library(data.table)
library(fs)
##Data loading

#spatial
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
  filter(JPT_NAZWA_=='małopolskie')%>% 
  st_transform(., 2180)

st_bbox(lesserpoland)

powiaty_LP <- powiaty %>% 
  filter(str_detect(JPT_KOD_JE,'^12'))%>% 
  st_transform(., 2180)

gminy_LP <- gminy %>% 
  filter(str_detect(JPT_KOD_JE,'^12'))%>% 
  st_transform(., 2180)


gminy_codes <- gminy_LP %>% 
  dplyr::select(.,"JPT_KOD_JE","JPT_NAZWA_") %>% 
  st_drop_geometry() %>% 
  mutate(JPT_KOD_JE = as.numeric(JPT_KOD_JE))

gminy_LP_grouped<-gminy_LP %>% 
  group_by(JPT_KOD_JE) %>% 
  group_split()

ls_border_pl <- lesserpoland %>%
  st_transform(., 2180) %>% 
  as(., 'Spatial')

ls_border <- lesserpoland %>%
  st_transform(., 4326) %>% 
  as(., 'Spatial')

ls_bbox<- ls_border %>% 
  bbox(.)

# WorldClim Data
# look in our folder, find the files that end with .tif and 

poland_crs<- "+proj=tmerc +lat_0=0 +lon_0=19 +k=0.9993 +x_0=500000 +y_0=-5300000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "

tmin_listfiles<-dir_info("ass_data/wea/tmin") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

tmin <- tmin_listfiles %>%
  raster::stack() %>% 
  crop(., ls_bbox) %>% 
  projectRaster(tmin, crs=poland_crs) %>% 
  raster::overlay(.,fun=mean) %>% 
  projectRaster(., crs=poland_crs)
  

plot(lesserpoland$geometry,add=T)

tmax_listfiles<-dir_info("ass_data/wea/tmax") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

tmax <- tmax_listfiles %>%
  raster::stack() %>% 
  crop(., ls_bbox) %>% 
  projectRaster(tmin, crs=poland_crs) %>% 
  raster::overlay(.,fun=mean) %>% 
  projectRaster(., crs=poland_crs)

prec_listfiles<-dir_info("ass_data/wea/prec") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

prec <- prec_listfiles %>%
  raster::stack() %>% 
  crop(., ls_bbox) %>% 
  projectRaster(tmin, crs=poland_crs) %>% 
  raster::overlay(.,fun=mean) %>% 
  projectRaster(., crs=poland_crs)

plot(tmin)
plot(lesserpoland$geometry)
plot(tmax)
plot(prec)

#extract data from raster 

tmin_gminy<- lapply(gminy_LP_grouped, function(g) raster::extract(tmin,g,small=TRUE))

tmax_gminy<- lapply(gminy_LP_grouped, function(g) raster::extract(tmax,g,small=TRUE))

prec_gminy<- lapply(gminy_LP_grouped, function(g) raster::extract(prec,g,small=TRUE))

#calculation mean for every region
tmin_gminy_mean<-  tmin_gminy %>% 
        lapply(.,function(g) 
          mean(as.numeric(as.character(unlist(g))))) %>% 
  as.data.frame(.) %>% 
  rename_at(vars(colnames(.)), function(x) gminy_codes_v) %>% 
  pivot_longer(everything(),
               names_to = "area_code",
               values_to = "tmin") 

tmax_gminy_mean<-  tmax_gminy %>% 
  lapply(.,function(g) 
    mean(as.numeric(as.character(unlist(g))))) %>% 
  as.data.frame(.) %>% 
  rename_at(vars(colnames(.)), function(x) gminy_codes_v) %>% 
  pivot_longer(everything(),
               names_to = "area_code",
               values_to = "tmax") 

prec_gminy_mean<-  prec_gminy %>% 
  lapply(.,function(g) 
    mean(as.numeric(as.character(unlist(g))))) %>% 
  as.data.frame(.) %>% 
  rename_at(vars(colnames(.)), function(x) gminy_codes_v) %>% 
  pivot_longer(everything(),
               names_to = "area_code",
               values_to = "prec") 


#transfering to data frame

gminy_codes <- gminy_LP %>% 
  dplyr::select(.,"JPT_KOD_JE","JPT_NAZWA_") %>% 
  st_drop_geometry() %>% 
  mutate(JPT_KOD_JE = as.numeric(JPT_KOD_JE)) %>% 
  arrange(.,JPT_KOD_JE)

gminy_codes_v<- as.vector(gminy_codes$JPT_KOD_JE)

w_gminy<- tmin_gminy_mean %>% 
  left_join(.,tmax_gminy_mean,by="area_code") %>% 
  left_join(.,prec_gminy_mean,by="area_code")

#writing to csv file

write.csv(w_gminy,here::here("ass_data",
                              "csv",
                              "weather.csv"), row.names = FALSE)
  


  