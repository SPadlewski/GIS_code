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
  filter(JPT_NAZWA_=='małopolskie')

powiaty_LP <- powiaty %>% 
  filter(str_detect(JPT_KOD_JE,'^12'))

gminy_LP <- gminy %>% 
  filter(str_detect(JPT_KOD_JE,'^12'))

#pollution

Stations <- read_csv(here::here("ass_data", 
                                "csv", 
                                "Stacje.csv"))
  
# loading single pollutant "PM10"


# 
# pm10_poland <- read_csv(here::here("ass_data", 
#                                 "csv", 
#                                 "PM10.csv")) %>% 
#   slice(., -(1))
#   
# pm10_poland_sf <- pm10_poland %>%
#   left_join(.,Stations,
#             by = c("Kod stacji" = "Kod stacji")) %>% 
#   drop_na("WGS84 φ N")%>% 
#   st_as_sf(., 
#            coords = c("WGS84 λ E","WGS84 φ N"), 
#            crs = 4326) 
# 
# pm10_ls<-pm10_poland_sf %>% 
#   filter(Województwo.x=='małopolskie')


###Load all pollutants 

pollutants <- c("pm10","pm25","so2","o3","nox","no2","co","c6h6")

pollutants_csv <- list()

for(i in pollutants){
    #loading csv for whole country and slicing first row
    filepath <- file.path(here("ass_data","csv"),paste(toupper(i),".csv",sep=""))
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

# adding furnaces csv 

furnaces <- read_csv(here::here("ass_data", 
                                "csv", 
                                "furnaces.csv")) %>% 
              dplyr::select(area_code,area_name,podregion,powiat_name,
                     typ_gminy,'2013','2014','2015','2016','2017','2018') %>% 
              drop_na(area_code)


furnaces_final <- furnaces %>% 
  mutate(type_n=case_when(str_detect(typ_gminy, "City|Urban$") ~ "1",
                          str_detect(typ_gminy, "Urban-rural") ~ "3",
                          str_detect(typ_gminy, "Rural") ~ "2")) %>% 
  mutate(area_code=paste(area_code, type_n,sep=""))



gminy_LP_fur <- gminy_LP %>%
  left_join(.,furnaces_final,
            by = c("JPT_KOD_JE" = "area_code"))


### quick plots 

##furnaces
tmap_mode("view")

tm_shape(lesserpoland) +
  tm_polygons(col = NA, alpha = 1) +
  tm_shape(powiaty_LP) +
  tm_polygons(col = NA, alpha=0.5) +
  tm_shape(gminy_LP_fur) +
  tm_polygons(col = "2018", breaks=c(0,25,50,75,100,150,200,250,300,500,1000,2000,4000),alpha=0.5) #seq(100, 4300 ,by=10)

## stations  
tm_shape(pm10_ls) +
  tm_dots(col=NA)

#quick plot

# Basic scatter plot
ggplot(pm10_ls, aes(x=Rok, y=Średnia)) + geom_point()
# Change the point size, and shape
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)

#####  Interpolation

plot(lesserpoland$geometry)
plot(st_geometry(pm10_ls), add=TRUE)

ls_border <- lesserpoland %>%
  st_transform(., 2180) %>% 
  as(., 'Spatial')

# SF -> SP period <=2008 - All pollutants
for (i in pollutants_csv) {
  filename <- eval(parse(text = sprintf("%s_sf",i)))
  assign(sprintf("%s_sp_08",i), summarise(group_by(janitor::clean_names(filter(filename,Rok<=2008)),kod_stacji),srednia = mean(as.numeric(srednia)), n = n()) ) 
  filename <- eval(parse(text = sprintf("%s_sp_08",i)))
  assign(sprintf("%s_sp_08",i), as(st_transform(filename, 2180), 'Spatial'))
}

# SF -> SP period >2008 - All pollutants
for (i in pollutants_csv) {
  filename <- eval(parse(text = sprintf("%s_sf",i)))
  assign(sprintf("%s_sp_19",i), summarise(group_by(janitor::clean_names(filter(filename,Rok>2008)),kod_stacji),srednia = mean(as.numeric(srednia)), n = n()) ) 
  filename <- eval(parse(text = sprintf("%s_sp_19",i)))
  assign(sprintf("%s_sp_19",i), as(st_transform(filename, 2180), 'Spatial'))
}

# SF -> SP period <=2008 - one pollutants

# test_pm10 <- pm10_poland_sf %>%
#   filter(Rok<=2008) %>%
#   janitor::clean_names(.)%>% 
#   group_by(kod_stacji) %>% 
#   summarise(srednia = mean(as.numeric(srednia)), n = n()) 
# 
# test_pm10 <- test_pm10 %>%   
#   st_transform(., 2180)%>% 
#   as(., 'Spatial')

## creating a grind
emptygrd <- as.data.frame(spsample(ls_border, n=1000, type="regular", cellsize=1000))

names(emptygrd) <- c("X", "Y")

coordinates(emptygrd) <- c("X", "Y")

gridded(emptygrd) <- TRUE  # Create SpatialPixel object
fullgrid(emptygrd) <- TRUE  # Create SpatialGrid object
#proj4string(emptygrd) <- proj4string(pm10_poland_sp)

# creating raster - period <=2008 - All pollutants
for (i in pollutants_csv) {
  filename <- eval(parse(text = sprintf("%s_sp_08",i)))
  # Add the projection to the grid
  proj4string(emptygrd)<- proj4string(filename)
  # Interpolate the grid cells using a power value of 2 
  assign("interpolate", gstat::idw(srednia ~ 1, filename, newdata=emptygrd, idp=2.0))
  # Convert output to raster object 
  assign("ras", raster(interpolate))
  # Clip the raster to Lesserpoland outline
  assign(sprintf("%s_rs_08",i), mask(ras, ls_border))
}

# creating raster - period >2008 - All pollutants
for (i in pollutants_csv) {
  filename <- eval(parse(text = sprintf("%s_sp_19",i)))
  # Add the projection to the grid
  proj4string(emptygrd)<- proj4string(filename)
  # Interpolate the grid cells using a power value of 2 
  assign("interpolate", gstat::idw(srednia ~ 1, filename, newdata=emptygrd, idp=2.0))
  # Convert output to raster object 
  assign("ras", raster(interpolate))
  # Clip the raster to Lesserpoland outline
  assign(sprintf("%s_rs_19",i), mask(ras, ls_border))
}


# Plot the raster
plot(so2_poland_rs_08)


#creating dataframe for pollutants 2000-2008
for (i in pollutants) {
  columnname <- parse(text = sprintf("mean_%s",i))
  assign(sprintf("%s_mean_08",i), data.frame("code"=character(),mean=numeric()))
  }
#extracting mean from a raster and adding it to dataframe 2000-2008
for (i in gminy_LP$JPT_KOD_JE) {
  #PM10
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_pm10_08",i),  lapply(raster::extract(pm10_poland_rs_08,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_pm10_08",i)))
  pm10_mean_08<-rbind(pm10_mean_08,c(i,filename[[1]]))
  pm10_mean_08<-rename(pm10_mean_08,code=1,pm10_mean_08=2)
  #PM25
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_pm25_08",i),  lapply(raster::extract(pm25_poland_rs_08,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_pm25_08",i)))
  pm25_mean_08<-rbind(pm25_mean_08,c(i,filename[[1]]))
  pm25_mean_08<-rename(pm25_mean_08,code=1,pm25_mean_08=2)
  #SO2
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_so2_08",i),  lapply(raster::extract(so2_poland_rs_08,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_so2_08",i)))
  so2_mean_08<-rbind(so2_mean_08,c(i,filename[[1]]))
  so2_mean_08<-rename(so2_mean_08,code=1,so2_mean_08=2)
  #O3
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_o3_08",i),  lapply(raster::extract(o3_poland_rs_08,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_o3_08",i)))
  o3_mean_08<-rbind(o3_mean_08,c(i,filename[[1]]))
  o3_mean_08<-rename(o3_mean_08,code=1,o3_mean_08=2)
  #NOx
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_nox_08",i),  lapply(raster::extract(nox_poland_rs_08,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_nox_08",i)))
  nox_mean_08<-rbind(nox_mean_08,c(i,filename[[1]]))
  nox_mean_08<-rename(nox_mean_08,code=1,nox_mean_08=2)
  #NO2
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_no2_08",i),  lapply(raster::extract(no2_poland_rs_08,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_no2_08",i)))
  no2_mean_08<-rbind(no2_mean_08,c(i,filename[[1]]))
  no2_mean_08<-rename(no2_mean_08,code=1,no2_mean_08=2)
  #CO
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_co_08",i),  lapply(raster::extract(co_poland_rs_08,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_co_08",i)))
  co_mean_08<-rbind(co_mean_08,c(i,filename[[1]]))
  co_mean_08<-rename(co_mean_08,code=1,co_mean_08=2)
  #C6H6
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_c6h6_08",i),  lapply(raster::extract(c6h6_poland_rs_08,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_c6h6_08",i)))
  c6h6_mean_08<-rbind(c6h6_mean_08,c(i,filename[[1]]))
  c6h6_mean_08<-rename(c6h6_mean_08,code=1,c6h6_mean_08=2)
  
  }

#creating dataframe for pollutants 2008-2019
for (i in pollutants) {
  columnname <- parse(text = sprintf("mean_%s",i))
  assign(sprintf("%s_mean_19",i), data.frame("code"=character(),mean=numeric()))
}
#extracting mean from a raster and adding it to dataframe 2008-2019
for (i in gminy_LP$JPT_KOD_JE) {
  #PM10
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_pm10_19",i),  lapply(raster::extract(pm10_poland_rs_19,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_pm10_08",i)))
  pm10_mean_19<-rbind(pm10_mean_19,c(i,filename[[1]]))
  pm10_mean_19<-rename(pm10_mean_19,code=1,pm10_mean_19=2)
  #PM25
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_pm25_19",i),  lapply(raster::extract(pm25_poland_rs_19,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_pm25_19",i)))
  pm25_mean_19<-rbind(pm25_mean_19,c(i,filename[[1]]))
  pm25_mean_19<-rename(pm25_mean_19,code=1,pm25_mean_19=2)
  #SO2
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_so2_19",i),  lapply(raster::extract(so2_poland_rs_19,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_so2_19",i)))
  so2_mean_19<-rbind(so2_mean_19,c(i,filename[[1]]))
  so2_mean_19<-rename(so2_mean_19,code=1,so2_mean_19=2)
  #O3
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_o3_19",i),  lapply(raster::extract(o3_poland_rs_19,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_o3_19",i)))
  o3_mean_19<-rbind(o3_mean_19,c(i,filename[[1]]))
  o3_mean_19<-rename(o3_mean_19,code=1,o3_mean_19=2)
  #NOx
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_nox_19",i),  lapply(raster::extract(nox_poland_rs_19,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_nox_19",i)))
  nox_mean_19<-rbind(nox_mean_19,c(i,filename[[1]]))
  nox_mean_19<-rename(nox_mean_19,code=1,nox_mean_19=2)
  #NO2
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_no2_19",i),  lapply(raster::extract(no2_poland_rs_19,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_no2_19",i)))
  no2_mean_19<-rbind(no2_mean_19,c(i,filename[[1]]))
  no2_mean_19<-rename(no2_mean_19,code=1,no2_mean_19=2)
  #CO
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_co_19",i),  lapply(raster::extract(co_poland_rs_19,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_co_19",i)))
  co_mean_19<-rbind(co_mean_19,c(i,filename[[1]]))
  co_mean_19<-rename(co_mean_19,code=1,co_mean_19=2)
  #C6H6
  assign(sprintf("sf_%s",i), filter(gminy_LP, JPT_KOD_JE==i))
  filename <- eval(parse(text = sprintf("sf_%s",i)))
  assign(sprintf("sf_%s_c6h6_19",i),  lapply(raster::extract(c6h6_poland_rs_19,filename,small=TRUE), mean))
  filename <- eval(parse(text = sprintf("sf_%s_c6h6_19",i)))
  c6h6_mean_19<-rbind(c6h6_mean_19,c(i,filename[[1]]))
  c6h6_mean_19<-rename(c6h6_mean_19,code=1,c6h6_mean_19=2)
  
}

## joining data frames 2000-2008