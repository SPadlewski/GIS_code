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
##Data loading

#spatial
powiaty<- st_read(here::here("qm_data", 
                             "geo", 'jednostki_administracyjne',
                             "Powiaty.shp"))# %>%st_transform(., 27700)

country<- st_read(here::here("qm_data", 
                             "geo", 'jednostki_administracyjne',
                             "Panstwo.shp"))# %>%st_transform(., 27700)

life_ex<- read_csv(here::here("qm_data", 
                              "csv", "life_ex.csv")) %>% 
  mutate(JPT_KOD_JE = substr(JPT_KOD_JE,1,nchar(JPT_KOD_JE)-3))

cities_code <- life_ex %>% 
  dplyr::select(JPT_KOD_JE)

cities <- powiaty %>% 
  inner_join(.,cities_code,by='JPT_KOD_JE')

plot(country$geometry)
plot(cities$geometry,add=TRUE)


#pollution

Stations <- read_csv(here::here("qm_data", 
                                "csv", 
                                "Stacje.csv"))

###Load all pollutants 

pollutants <- c("pm10","pm25","so2","o3","nox","no2","co","c6h6")

pollutants_csv <- list()

for(i in pollutants){
  #loading csv for whole country and slicing first row
  filepath <- file.path(here("qm_data","csv"),paste(toupper(i),".csv",sep=""))
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

plot(country$geometry)

poland_border <- country %>%
  st_transform(., 2180) %>% 
  as(., 'Spatial')

# SF -> SP  - All pollutants
for (i in pollutants_csv) {
  filename <- eval(parse(text = sprintf("%s_sf",i)))
  assign(sprintf("%s_sp",i), summarise(group_by(janitor::clean_names(filename),kod_stacji),srednia = mean(as.numeric(srednia)), n = n()) ) 
  filename <- eval(parse(text = sprintf("%s_sp",i)))
  assign(sprintf("%s_sp",i), as(st_transform(filename, 2180), 'Spatial'))
}

## creating a grid
emptygrd <- as.data.frame(spsample(poland_border, n=1000, type="regular", cellsize=2500))

names(emptygrd) <- c("X", "Y")

coordinates(emptygrd) <- c("X", "Y")

gridded(emptygrd) <- TRUE  # Create SpatialPixel object
fullgrid(emptygrd) <- TRUE  # Create SpatialGrid object
#proj4string(emptygrd) <- proj4string(pm10_poland_sp)

# creating raster - - All pollutants
for (i in pollutants_csv) {
  filename <- eval(parse(text = sprintf("%s_sp",i)))
  # Add the projection to the grid
  proj4string(emptygrd)<- proj4string(filename)
  # Interpolate the grid cells using a power value of 2 
  assign("interpolate", gstat::idw(srednia ~ 1, filename, newdata=emptygrd, idp=2.0))
  # Convert output to raster object 
  assign("ras", raster(interpolate))
  # Clip the raster to Lesserpoland outline
  assign(sprintf("%s_rs",i), mask(ras, poland_border))
}

# Plot the raster
plot(pm25_poland_rs)


#creating dataframe for pollutants 
for (i in pollutants) {
  columnname <- parse(text = sprintf("mean_%s",i))
  assign(sprintf("%s_mean",i), data.frame("code"=character(),mean=numeric()))
}
#extracting mean from a raster and adding it to dataframe 
for (i in cities$JPT_KOD_JE) {
  #PM10
  assign(sprintf("sf_%s",i), filter(cities, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_pm10",i),  lapply(raster::extract(pm10_poland_rs,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_pm10",i)))
  pm10_mean<-rbind(pm10_mean,c(i,filename[[1]]))
  pm10_mean<-rename(pm10_mean,code=1,pm10_mean=2)
  #PM25
  assign(sprintf("sf_%s",i), filter(cities, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_pm25",i),  lapply(raster::extract(pm25_poland_rs,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_pm25",i)))
  pm25_mean<-rbind(pm25_mean,c(i,filename[[1]]))
  pm25_mean<-rename(pm25_mean,code=1,pm25_mean=2)
  #SO2
  assign(sprintf("sf_%s",i), filter(cities, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_so2",i),  lapply(raster::extract(so2_poland_rs,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_so2",i)))
  so2_mean<-rbind(so2_mean,c(i,filename[[1]]))
  so2_mean<-rename(so2_mean,code=1,so2_mean=2)
  #O3
  assign(sprintf("sf_%s",i), filter(cities, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_o3",i),  lapply(raster::extract(o3_poland_rs,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_o3",i)))
  o3_mean<-rbind(o3_mean,c(i,filename[[1]]))
  o3_mean<-rename(o3_mean,code=1,o3_mean=2)
  #NOx
  assign(sprintf("sf_%s",i), filter(cities, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_nox",i),  lapply(raster::extract(nox_poland_rs,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_nox",i)))
  nox_mean<-rbind(nox_mean,c(i,filename[[1]]))
  nox_mean<-rename(nox_mean,code=1,nox_mean=2)
  #NO2
  assign(sprintf("sf_%s",i), filter(cities, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_no2",i),  lapply(raster::extract(no2_poland_rs,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_no2",i)))
  no2_mean<-rbind(no2_mean,c(i,filename[[1]]))
  no2_mean<-rename(no2_mean,code=1,no2_mean=2)
  #CO
  assign(sprintf("sf_%s",i), filter(cities, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_co",i),  lapply(raster::extract(co_poland_rs,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_co",i)))
  co_mean<-rbind(co_mean,c(i,filename[[1]]))
  co_mean<-rename(co_mean,code=1,co_mean=2)
  #C6H6
  assign(sprintf("sf_%s",i), filter(cities, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_c6h6",i),  lapply(raster::extract(c6h6_poland_rs,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_c6h6",i)))
  c6h6_mean<-rbind(c6h6_mean,c(i,filename[[1]]))
  c6h6_mean<-rename(c6h6_mean,code=1,c6h6_mean=2)
  
}

## joining data frames 
pollutants_mean <- pm10_mean

pollutans_meansnames <-list(pollutants_mean,pm25_mean,so2_mean,co_mean,o3_mean,no2_mean,nox_mean,c6h6_mean)

pollutants_mean <- Reduce(function(x,y) left_join(x = x, y = y, by = "code"), 
                             pollutans_meansnames)

#joing pollutants and lief_ex with cities

cities_final <- cities %>% 
  merge(x=.,y=pollutants_mean, by.x= "JPT_KOD_JE",by.y="code") %>% 
  merge(x=.,y=life_ex, by= "JPT_KOD_JE") 
  
#quick plot
# Basic scatter plot
ggplot(cities_final, aes(x=c6h6_mean, y=lx)) + geom_point()
# Change the point size, and shape
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)