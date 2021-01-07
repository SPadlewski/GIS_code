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

#Pollution Stations

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
     }

# putting all pollutants SFs to a list
pollutants_sf<- list(pm10_poland_sf,pm25_poland_sf,co_poland_sf,no2_poland_sf,
                     nox_poland_sf,so2_poland_sf,c6h6_poland_sf,o3_poland_sf)

#####  Interpolation

ls_border <- lesserpoland %>%
  st_transform(., 2180) %>% 
  as(., 'Spatial')


# SF -> SP period  yearly - All pollutants
pollutants_yearly <- lapply(pollutants_sf,function(x) group_split(x,Rok) ) 

pollutants_yearly_sp <- lapply(pollutants_yearly,
                                 function(x) lapply(x,
                                                    function(y) as(st_transform(y, 2180), 'Spatial')))

## creating a grind
emptygrd <- as.data.frame(spsample(ls_border, n=1000, type="regular", cellsize=1000))

names(emptygrd) <- c("X", "Y")

coordinates(emptygrd) <- c("X", "Y")

gridded(emptygrd) <- TRUE  # Create SpatialPixel object
fullgrid(emptygrd) <- TRUE  # Create SpatialGrid object

pollutants_yearly_ras<- list()

# creating raster - yearly- All pollutants
for (p in pollutants_yearly_sp) {
    # Add the projection to the grid
    proj4string(emptygrd)<- proj4string(p[[1]])
    # Interpolate the grid cells using a power value of 2 
    assign("interpolate_list", lapply(p,function(i) gstat::idw(Średnia ~ 1, i, newdata=emptygrd, idp=2.0)))
    # Convert output to raster object 
    assign("ras", lapply(interpolate_list,function(i) raster(i)))
    # Clip the raster to Lesserpoland outline
    assign("ras_masked" ,lapply(ras,function(i) raster::mask(i, ls_border)))
    # add to the pollutants_yearly_ras
    pollutants_yearly_ras<-append(pollutants_yearly_ras,list(ras_masked))
  }  
 
# Plot the raster - test
plot(pollutants_yearly_ras[[8]][[20]])


#extracting values FOR EVERY REGION from a raster and adding it to dataframe 
pollutants_yearly_mean<- list()

for (p in pollutants_yearly_ras) {
  assign("pollutants_yearly_gminy",  lapply(p, function(y) 
                                              lapply(gminy_LP_grouped, function(g)       
                                                raster::extract(y,g,small=TRUE))))
  pollutants_yearly_mean<-append(pollutants_yearly_mean,list(pollutants_yearly_gminy))
} 

#calculation mean for every region
pollutants_yearly_gminy_mean<-  pollutants_yearly_mean %>% 
                              lapply(., function(p)
                                lapply(p,function(y) 
                                  lapply(y,function(g) 
                                       mean(as.numeric(as.character(unlist(g))))
    )))

#transfering to data frame

gminy_codes <- gminy_LP %>% 
  dplyr::select(.,"JPT_KOD_JE","JPT_NAZWA_") %>% 
  st_drop_geometry() %>% 
  mutate(JPT_KOD_JE = as.numeric(JPT_KOD_JE)) %>% 
  arrange(.,JPT_KOD_JE)
  
gminy_codes_v<- as.vector(gminy_codes$JPT_KOD_JE)

# naming list elements + convert to data frame
df <- pollutants_yearly_gminy_mean %>% 
                    lapply(., function(p) 
                        lapply(p,function(y) 
                            as.data.frame(y))) %>% 
                                lapply(., function(p) 
                                  lapply(p, function(y) y %>% 
           rename_at(vars(colnames(y)), function(x) gminy_codes_v) %>% 
           pivot_longer(everything())))

names(df) <- c("pm10","pm25","co","no2","nox","so2","c6h6","o3")

for (i in c(1,4,5,6,8)) {
  names(df[[i]]) <- as.character(unlist(seq(2000,2019,by=1)))
  names(df[[2]]) <- as.character(unlist(seq(2002,2019,by=1)))
  names(df[[7]]) <- as.character(unlist(seq(2001,2019,by=1)))
  names(df[[3]]) <- as.character(unlist(seq(2003,2019,by=1)))
    }


df_pollutants <- df %>% 
  lapply(., function(x) x %>% reduce(left_join, by = "name")) 

for (i in c(1,4,5,6,8)) {
  names(df_pollutants[[i]]) <- c("area_code",as.character(unlist(seq(2000,2019,by=1))))
  names(df_pollutants[[2]]) <- c("area_code",as.character(unlist(seq(2002,2019,by=1))))
  names(df_pollutants[[7]]) <- c("area_code",as.character(unlist(seq(2001,2019,by=1))))
  names(df_pollutants[[3]]) <- c("area_code",as.character(unlist(seq(2003,2019,by=1))))
}

df_pollutants <- df_pollutants %>% 
  reduce(left_join, by = "area_code",all.x = TRUE,all.y = TRUE) 


df_pollutants <- df_pollutants%>% 
  rename_at(.vars = vars(seq(136,155,1)),
            .funs = funs(sub("[.x]*$|[.y]*$", "_o3", .))) %>% 
  rename_at(.vars = vars(seq(117,135,1)),
            .funs = funs(sub("[.x]*$|[.y]*$", "_c6h6", .))) %>% 
  rename_at(.vars = vars(seq(97,116,1)),
            .funs = funs(sub("[.x]*$|[.y]*$", "_so2", .)))%>% 
  rename_at(.vars = vars(seq(77,96,1)),
            .funs = funs(sub("[.x]*$|[.y]*$", "_nox", .))) %>% 
  rename_at(.vars = vars(seq(57,76,1)),
            .funs = funs(sub("[.x]*$|[.y]*$", "_no2", .)))%>% 
  rename_at(.vars = vars(seq(40,56,1)),
            .funs = funs(sub("[.x]*$|[.y]*$", "_co", .))) %>% 
  rename_at(.vars = vars(seq(22,39,1)),
            .funs = funs(sub("[.x]*$|[.y]*$", "_pm25", .)))%>% 
  rename_at(.vars = vars(seq(2,21,1)),
            .funs = funs(sub("[.x]*$|[.y]*$", "_pm10", .))) 


# write.csv(df_pollutants,here::here("ass_data",
#                              "csv",
#                              "pollution_yearly.csv"), row.names = FALSE)


