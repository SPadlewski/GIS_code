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
library("plyr") 
library(here)
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
  filter(JPT_NAZWA_=='małopolskie') %>% 
  st_transform(., 2180)
crs(lesserpoland)

powiaty_LP <- powiaty %>% 
  filter(str_detect(JPT_KOD_JE,'^12'))%>% 
  st_transform(., 2180)

gminy_LP <- gminy %>% 
  filter(str_detect(JPT_KOD_JE,'^12'))%>% 
  st_transform(., 2180)

#pollution

Stations <- read_csv(here::here("ass_data", 
                                "csv", 
                                "Stacje.csv"))

###Load all pollutants 

pollutants <- c("pm10","pm25","so2","o3","nox","no2","co","c6h6")

pollutants_csv <- list()

for(i in pollutants){
  #loading csv for whole country and slicing first row
  filepath <- file.path(here::here("ass_data","csv"),paste(toupper(i),".csv",sep=""))
  assign(sprintf("%s_poland",i), slice(read_csv(filepath),-1)) 
  pollutants_csv[[match(i,pollutants)]] <- sprintf("%s_poland",i)
  
}

for (i in pollutants_csv) {
  # joining with stations dataframe by station code
  filename <- eval(parse(text = i))
  assign(sprintf("%s_sf",i), left_join(filename,Stations,
                                       by = c("Kod stacji" = "Kod stacji"))) 
  # droping na from "WGS84 φ N" column
  filename_2 <- eval(parse(text = sprintf("%s_sf",i)))
  assign(sprintf("%s_sf",i), drop_na(filename_2,"WGS84 φ N") ) 
  # converting to SF
  filename_2 <- eval(parse(text = sprintf("%s_sf",i)))
  assign(sprintf("%s_sf",i), st_as_sf(filename_2, coords = c("WGS84 λ E","WGS84 φ N"), crs = 4326)) 
  # filtering for lesserpoland
  #  filename_2 <- eval(parse(text = sprintf("%s_sf",i)))
  #  assign(sprintf("%s_ls",pollutants[[match(i,pollutants_csv)]]), dplyr::filter(filename_2,Województwo.x=='małopolskie'))
}

#####  Interpolation

plot(lesserpoland$geometry)
plot(st_geometry(pm10_ls), add=TRUE)

ls_border <- lesserpoland %>%
  st_transform(., 2180) %>% 
  as(., 'Spatial')



co_poland_sf_t<-co_poland_sf %>% 
  group_by(Rok) %>% 
  group_split()


# SF -> SP yearly - All pollutants
for (i in pollutants_csv) {
  filename <- eval(parse(text = sprintf("%s_sf",i)))
  assign(sprintf("%s_sf_all",i), group_split(group_by(filename,Rok)))
  filename <- eval(parse(text = sprintf("%s_sf_all",i)))
  assign(sprintf("%s_sp_all",i), lapply(filename,function(i) as(st_transform(i,2180), 'Spatial')))
}


## creating a grind
emptygrd <- as.data.frame(spsample(ls_border, n=1000, type="regular", cellsize=1000))

names(emptygrd) <- c("X", "Y")

coordinates(emptygrd) <- c("X", "Y")

gridded(emptygrd) <- TRUE  # Create SpatialPixel object
fullgrid(emptygrd) <- TRUE  # Create SpatialGrid object
#proj4string(emptygrd) <- proj4string(pm10_poland_sp)

pm10_poland_sp_all[1]

# creating raster - yearly- All pollutants
for (i in pollutants_csv) {
  filename <- eval(parse(text = sprintf("%s_sp_all",i)))
  # Add the projection to the grid
  proj4string(emptygrd)<- proj4string(filename[[1]])
  # Interpolate the grid cells using a power value of 2 
  assign("interpolate_list", lapply(filename,function(i) gstat::idw(Średnia ~ 1, i, newdata=emptygrd, idp=2.0)))
  # Convert output to raster object 
  assign("ras", lapply(interpolate_list,function(i) raster(i)))
  # Clip the raster to Lesserpoland outline
  assign(sprintf("%s_rs_all",i), lapply(ras,function(i) mask(i, ls_border)))
  }
  

# Plot the raster
plot(pm10_poland_rs_all[[20]])

#extracting mean from a raster and adding it to dataframe 2000-2008
for (i in pollutants) {
  filename <- eval(parse(text = sprintf("%s_poland_rs_all",i)))
  assign(sprintf("%s_ls_mean",i),  lapply(filename,function(x) raster::extract(x,lesserpoland,small=TRUE)))
  filename <- eval(parse(text = sprintf("%s_ls_mean",i)))
  assign(sprintf("%s_ls_mean",i),  lapply(filename,function(x) as.numeric(as.character(unlist(x)))))
  filename <- eval(parse(text = sprintf("%s_ls_mean",i)))
  assign(sprintf("%s_ls_mean",i),  lapply(filename,function(x) mean(x)))
}  

#to dataframe
pm10_all<-as.data.frame(pm10_ls_mean) %>% 
  dplyr::rename("2000"=1,"2001"=2,"2002"=3,"2003"=4,"2004"=5,"2005"=6,"2006"=7,
         "2007"=8,"2008"=9,"2009"=10,"2010"=11,"2011"=12,"2012"=13,"2013"=14,
         "2014"=15,"2015"=16,"2016"=17,"2017"=18,"2018"=19,"2019"=20,)%>% 
  mutate(pollutant="pm10") 
so2_all<-as.data.frame(so2_ls_mean) %>% 
  dplyr::rename("2000"=1,"2001"=2,"2002"=3,"2003"=4,"2004"=5,"2005"=6,"2006"=7,
         "2007"=8,"2008"=9,"2009"=10,"2010"=11,"2011"=12,"2012"=13,"2013"=14,
         "2014"=15,"2015"=16,"2016"=17,"2017"=18,"2018"=19,"2019"=20,)%>% 
  mutate(pollutant="so2") 
o3_all<-as.data.frame(o3_ls_mean) %>% 
  dplyr::rename("2000"=1,"2001"=2,"2002"=3,"2003"=4,"2004"=5,"2005"=6,"2006"=7,
         "2007"=8,"2008"=9,"2009"=10,"2010"=11,"2011"=12,"2012"=13,"2013"=14,
         "2014"=15,"2015"=16,"2016"=17,"2017"=18,"2018"=19,"2019"=20,)%>% 
  mutate(pollutant="o3") 
no2_all<-as.data.frame(no2_ls_mean) %>% 
  dplyr::rename("2000"=1,"2001"=2,"2002"=3,"2003"=4,"2004"=5,"2005"=6,"2006"=7,
         "2007"=8,"2008"=9,"2009"=10,"2010"=11,"2011"=12,"2012"=13,"2013"=14,
         "2014"=15,"2015"=16,"2016"=17,"2017"=18,"2018"=19,"2019"=20,)%>% 
  mutate(pollutant="no2") 
nox_all<-as.data.frame(nox_ls_mean) %>% 
  dplyr::rename("2000"=1,"2001"=2,"2002"=3,"2003"=4,"2004"=5,"2005"=6,"2006"=7,
         "2007"=8,"2008"=9,"2009"=10,"2010"=11,"2011"=12,"2012"=13,"2013"=14,
         "2014"=15,"2015"=16,"2016"=17,"2017"=18,"2018"=19,"2019"=20,)%>% 
  mutate(pollutant="nox") 
c6h6_all<-as.data.frame(c6h6_ls_mean) %>% 
  dplyr::rename("2001"=1,"2002"=2,"2003"=3,"2004"=4,"2005"=5,"2006"=6,
         "2007"=7,"2008"=8,"2009"=9,"2010"=10,"2011"=11,"2012"=12,"2013"=13,
         "2014"=14,"2015"=15,"2016"=16,"2017"=17,"2018"=18,"2019"=19,)%>% 
  mutate(pollutant="c6h6") 
co_all<-as.data.frame(co_ls_mean) %>% 
  dplyr::rename(.,"2003"=1,"2004"=2,"2005"=3,"2006"=4,
         "2007"=5,"2008"=6,"2009"=7,"2010"=8,"2011"=9,"2012"=10,"2013"=11,
         "2014"=12,"2015"=13,"2016"=14,"2017"=15,"2018"=16,"2019"=17)%>% 
  mutate(pollutant="co") 
pm25_all<-as.data.frame(pm25_ls_mean) %>% 
 dplyr::rename("2002"=1,"2003"=2,"2004"=3,"2005"=4,"2006"=5,
         "2007"=6,"2008"=7,"2009"=8,"2010"=9,"2011"=10,"2012"=11,"2013"=12,
         "2014"=13,"2015"=14,"2016"=15,"2017"=16,"2018"=17,"2019"=18,) %>% 
          mutate(pollutant="pm25") 

yearly_mean<- pm10_all
for (i in pollutants[2:8]) {
  filename<-eval(parse(text = sprintf("%s_all",i)))
  yearly_mean<-rbind.fill(yearly_mean,filename)
}

yearly_mean_tidy<-yearly_mean %>% 
  pivot_longer(!pollutant, 
               names_to="year", 
               values_to="measurements")

ggplot(yearly_mean_tidy, aes(year,measurements,colour=pollutant)) + 
  geom_point()+ geom_vline(aes(xintercept="2008"))+
  facet_wrap(~ pollutant, ncol=4)

## joining data frames 2000-2008
pollutants_mean_08 <- pm10_mean_08

pollutans_meansnames_08 <-list(pollutants_mean_08,pm25_mean_08,so2_mean_08,co_mean_08,o3_mean_08,no2_mean_08,nox_mean_08,c6h6_mean_08)

pollutants_mean_08 <- Reduce(function(x,y) left_join(x = x, y = y, by = "code"), 
                             pollutans_meansnames_08)

## joining data frames 2008-2019
pollutants_mean_19 <- pm10_mean_19

pollutans_meansnames_19 <-list(pollutants_mean_19,pm25_mean_19,
                               so2_mean_19,co_mean_19,o3_mean_19,no2_mean_19,
                               nox_mean_19,c6h6_mean_19)

pollutants_mean_19 <- Reduce(function(x,y) left_join(x = x, y = y, by = "code"), 
                             pollutans_meansnames_19)

# 
# con2aqi(pollutant="pm10",con=68)
# pollutants_mean_19_test <- pollutants_mean_19 %>% 
#   mutate(AQI con2aqi(pollutant="co",con=8.4))
