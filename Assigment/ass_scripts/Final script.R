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
library(spdep)
library(car)
library(caret)
library(broom)
library(corrr)

## Loading data

### Spatial Data
gminy <- st_read(here::here("ass_data", 
                            "geo", 'jednostki_administracyjne',
                            "Gminy.shp"))

wojewodztwa<- st_read(here::here("ass_data", 
                                 "geo", 'jednostki_administracyjne',
                                 "Wojewodztwa.shp"))


powiaty<- st_read(here::here("ass_data", 
                             "geo", 'jednostki_administracyjne',
                             "Powiaty.shp"))

country<- st_read(here::here("ass_data", 
                             "geo", 'jednostki_administracyjne',
                             "Panstwo.shp"))


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



### Pollution

Stations <- read_csv(here::here("ass_data", 
                                "csv", 
                                "Stacje.csv"))

# Following piece of code is commented out because running it might take up to 1h 
# The output of this section of the code is the data frame called "pollution_yearly"
# which consist annual reading of the eight pollutants ("pm10","pm25","so2","o3","nox","no2","co","c6h6")
# from the period of 2000-2019 for all the 182 subregions in malopolska

# #### Loading all pollutants 2000-2019 data
# 
# pollutants <- c("pm10","pm25","so2","o3","nox","no2","co","c6h6")
# 
# pollutants_csv <- list()
# 
# for(i in pollutants){
#   #loading csv for whole country and slicing first row
#   filepath <- file.path(here::here("ass_data","csv"),paste(toupper(i),".csv",sep=""))
#   assign(sprintf("%s_poland",i), slice(read_csv(filepath),-1)) 
#   pollutants_csv[[match(i,pollutants)]] <- sprintf("%s_poland",i)
# }
# 
# ##### Joining pollution readings with the station data and converting it to SF
# 
# for (i in pollutants_csv) {
#   # joining with stations dataframe by station code
#   filename <- eval(parse(text = i))
#   assign(sprintf("%s_sf",i), left_join(filename,Stations,
#                                        by = c("Kod stacji" = "Kod stacji"))) 
#   # droping na from "WGS84 φ N" column
#   filename_2 <- eval(parse(text = sprintf("%s_sf",i)))
#   assign(sprintf("%s_sf",i), drop_na(filename_2,"WGS84 φ N") ) 
#   # converting to SF
#   filename_2 <- eval(parse(text = sprintf("%s_sf",i)))
#   assign(sprintf("%s_sf",i), st_as_sf(filename_2, coords = c("WGS84 λ E","WGS84 φ N"), crs = 4326)) 
# }
# 
# #####  putting all pollutants SFs to a list
# pollutants_sf<- list(pm10_poland_sf,pm25_poland_sf,co_poland_sf,no2_poland_sf,
#                      nox_poland_sf,so2_poland_sf,c6h6_poland_sf,o3_poland_sf)
# #### Interpolation
# 
# ##### SP outline od Malopolska
# ls_border <- lesserpoland %>%
#   st_transform(., 2180) %>% 
#   as(., 'Spatial')
# 
# ##### SF -> SP yearly measurments  - All pollutants
# pollutants_yearly <- lapply(pollutants_sf,function(x) group_split(x,Rok) ) 
# 
# pollutants_yearly_sp <- lapply(pollutants_yearly,
#                                function(x) lapply(x,
#                                                   function(y) as(st_transform(y, 2180), 'Spatial')))
# 
# ##### creating a grind
# emptygrd <- as.data.frame(spsample(ls_border, n=1000, type="regular", cellsize=1000))
# 
# names(emptygrd) <- c("X", "Y")
# 
# coordinates(emptygrd) <- c("X", "Y")
# 
# gridded(emptygrd) <- TRUE  # Create SpatialPixel object
# fullgrid(emptygrd) <- TRUE  # Create SpatialGrid object
# 
# ##### creating raster with yearly measurments from 2000-2019 for all pollutants
# 
# pollutants_yearly_ras<- list()
# 
# for (p in pollutants_yearly_sp) {
#   # Add the projection to the grid
#   proj4string(emptygrd)<- proj4string(p[[1]])
#   # Interpolate the grid cells using a power value of 2 
#   assign("interpolate_list", lapply(p,function(i) gstat::idw(Średnia ~ 1, i, newdata=emptygrd, idp=2.0)))
#   # Convert output to raster object 
#   assign("ras", lapply(interpolate_list,function(i) raster(i)))
#   # Clip the raster to Lesserpoland outline
#   assign("ras_masked" ,lapply(ras,function(i) raster::mask(i, ls_border)))
#   # add to the pollutants_yearly_ras
#   pollutants_yearly_ras<-append(pollutants_yearly_ras,list(ras_masked))
# } 
# 
# ##### Plot the raster - test
# # plot(pollutants_yearly_ras[[8]][[20]])
# 
# #####  extracting values FOR EVERY REGION from a raster and adding it to dataframe 
# pollutants_yearly_mean<- list()
# 
# for (p in pollutants_yearly_ras) {
#   assign("pollutants_yearly_gminy",  lapply(p, function(y) 
#     lapply(gminy_LP_grouped, function(g)       
#       raster::extract(y,g,small=TRUE))))
#   pollutants_yearly_mean<-append(pollutants_yearly_mean,list(pollutants_yearly_gminy))
# } 
# 
# ##### calculation annual mean for every region
# pollutants_yearly_gminy_mean<-  pollutants_yearly_mean %>% 
#   lapply(., function(p)
#     lapply(p,function(y) 
#       lapply(y,function(g) 
#         mean(as.numeric(as.character(unlist(g))))
#       )))
# 
# #####transfering to data frame
# 
# gminy_codes <- gminy_LP %>% 
#   dplyr::select(.,"JPT_KOD_JE","JPT_NAZWA_") %>% 
#   st_drop_geometry() %>% 
#   mutate(JPT_KOD_JE = as.numeric(JPT_KOD_JE)) %>% 
#   arrange(.,JPT_KOD_JE)
# 
# gminy_codes_v<- as.vector(gminy_codes$JPT_KOD_JE)
# 
# ##### naming list elements + convert to the final data frame
# df <- pollutants_yearly_gminy_mean %>% 
#   lapply(., function(p) 
#     lapply(p,function(y) 
#       as.data.frame(y))) %>% 
#   lapply(., function(p) 
#     lapply(p, function(y) y %>% 
#              rename_at(vars(colnames(y)), function(x) gminy_codes_v) %>% 
#              pivot_longer(everything())))
# 
# names(df) <- c("pm10","pm25","co","no2","nox","so2","c6h6","o3")
# 
# for (i in c(1,4,5,6,8)) {
#   names(df[[i]]) <- as.character(unlist(seq(2000,2019,by=1)))
#   names(df[[2]]) <- as.character(unlist(seq(2002,2019,by=1)))
#   names(df[[7]]) <- as.character(unlist(seq(2001,2019,by=1)))
#   names(df[[3]]) <- as.character(unlist(seq(2003,2019,by=1)))
# }
# 
# 
# df_pollutants <- df %>% 
#   lapply(., function(x) x %>% reduce(left_join, by = "name")) 
# 
# for (i in c(1,4,5,6,8)) {
#   names(df_pollutants[[i]]) <- c("area_code",as.character(unlist(seq(2000,2019,by=1))))
#   names(df_pollutants[[2]]) <- c("area_code",as.character(unlist(seq(2002,2019,by=1))))
#   names(df_pollutants[[7]]) <- c("area_code",as.character(unlist(seq(2001,2019,by=1))))
#   names(df_pollutants[[3]]) <- c("area_code",as.character(unlist(seq(2003,2019,by=1))))
# }
# 
# df_pollutants <- df_pollutants %>% 
#   reduce(left_join, by = "area_code",all.x = TRUE,all.y = TRUE) 
# 
# 
# df_pollutants <- df_pollutants%>% 
#   rename_at(.vars = vars(seq(136,155,1)),
#             .funs = funs(sub("[.x]*$|[.y]*$", "_o3", .))) %>% 
#   rename_at(.vars = vars(seq(117,135,1)),
#             .funs = funs(sub("[.x]*$|[.y]*$", "_c6h6", .))) %>% 
#   rename_at(.vars = vars(seq(97,116,1)),
#             .funs = funs(sub("[.x]*$|[.y]*$", "_so2", .)))%>% 
#   rename_at(.vars = vars(seq(77,96,1)),
#             .funs = funs(sub("[.x]*$|[.y]*$", "_nox", .))) %>% 
#   rename_at(.vars = vars(seq(57,76,1)),
#             .funs = funs(sub("[.x]*$|[.y]*$", "_no2", .)))%>% 
#   rename_at(.vars = vars(seq(40,56,1)),
#             .funs = funs(sub("[.x]*$|[.y]*$", "_co", .))) %>% 
#   rename_at(.vars = vars(seq(22,39,1)),
#             .funs = funs(sub("[.x]*$|[.y]*$", "_pm25", .)))%>% 
#   rename_at(.vars = vars(seq(2,21,1)),
#             .funs = funs(sub("[.x]*$|[.y]*$", "_pm10", .))) 
# ##### saving to csv 
# write.csv(df_pollutants,here::here("ass_data",
#                              "csv",
#                              "pollution_yearly.csv"), row.names = FALSE)

#### Loading generated csv file with pollutans 
pollutants <- read_csv(here::here("ass_data", 
                                  "csv", 
                                  "pollution_yearly.csv")) 

#### calculating mean reading of the pollutants for the whole region of Malopolska 
pollutants_ls_mean<- pollutants%>% 
  summarise_all( mean) %>% 
  pivot_longer(-area_code,
               names_to = "year_pollutant",
               values_to = "value",
  ) %>% 
  select(-area_code) %>% 
  mutate(year= as.factor(str_extract(year_pollutant, "^\\d+")),
         pollutant= as.character(str_extract(year_pollutant, "(?<=_)(.*)")))

#### calculating difference of the annual PM25 reading before 2008 and after  
pm25_dif <- pollutants%>% 
  dplyr::select(c(1,22:39))%>%
  mutate(across(.cols = 2:19, as.numeric)) %>% 
  mutate(pm25_08_19= round(rowMeans(.[c(9:18)], na.rm = TRUE),1)) %>%
  mutate(pm25_02_08= round(rowMeans(.[c(2:8)], na.rm = TRUE),1)) %>% 
  mutate(pm25= .[[18]]-pm25_02_08) %>% 
  select(area_code,pm25)

### Density of population

density <- read_csv(here::here("ass_data", 
                               "csv", 
                               "density.csv")) %>% 
  slice(.,-2) %>% 
  row_to_names(row_number = 1) %>% 
  dplyr::rename(.,area_code=1,area_name=2)  %>% 
  right_join(.,gminy_codes, by=c("area_code"="JPT_KOD_JE"))


#### calculating difference between 02-08 and 08-19
density_dif <- density %>% 
  dplyr::select(c(1,3:20))%>%
  mutate(across(.cols = 2:19, as.numeric)) %>% 
  mutate(density_km2_02_08= round(rowMeans(.[c(2:8)], na.rm = TRUE),0)) %>%
  mutate(density_km2_08_19= round(rowMeans(.[c(9:19)], na.rm = TRUE),0)) %>%
  mutate(density_km2= density_km2_08_19-density_km2_02_08) %>% 
  select(area_code,density_km2)%>% 
  mutate(density_km2 = replace_na(density_km2, 0))

### population
pop <- read_csv(here::here("ass_data", 
                           "csv", 
                           "ludnosc.csv")) %>% 
  slice(.,c(-1,-2,-3,-5)) %>% 
  row_to_names(row_number = 1) %>% 
  dplyr::rename(.,area_code=1,area_name=2) %>% 
  mutate(across(.cols = 3:22, as.numeric))%>% 
  mutate(population_00_08 = round(rowMeans(subset(., select = as.numeric(c(3:11))), na.rm = TRUE),0)) %>% 
  mutate(population_08_19 = round(rowMeans(subset(., select = as.numeric(c(12:22))), na.rm = TRUE),0)) %>% 
  mutate(population_dif = population_08_19-population_00_08)

pop_dif <- pop %>% 
  dplyr::select(area_code,population_dif) %>% 
  mutate(population_dif = replace_na(population_dif, 0))

pop_08_19 <- pop %>% 
  dplyr::select(area_code,population_08_19)


### Furnaces removed in malopolska during 2008-2019

furnaces <- read_csv(here::here("ass_data", 
                                "csv", 
                                "furnaces_final.csv")) %>% 
  dplyr::select(area_code,area_name,podregion,powiat_name,
                typ_gminy,'2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018') %>% 
  drop_na(area_code)


furnaces_final <- furnaces %>% 
  mutate(type_n=case_when(str_detect(typ_gminy, "City|Urban$") ~ "1",
                          str_detect(typ_gminy, "Urban-rural") ~ "3",
                          str_detect(typ_gminy, "Rural") ~ "2")) %>% 
  mutate(area_code=paste(area_code, type_n,sep="")) %>% 
  mutate(all_removed= rowSums(.[sapply(.,is.numeric)], na.rm = TRUE)) %>%
  mutate(area_code=as.numeric(area_code)) %>% 
  left_join(.,pop_08_19,by="area_code") %>% 
  mutate(furnaces_per_1000= round(all_removed/(population_08_19/1000),0))

total_fur <- sum(furnaces_final$all_removed)

furnaces_08_19 <- furnaces_final %>% 
  dplyr::select(area_code,furnaces_per_1000)

gminy_LP_fur <- gminy_LP %>%
  mutate(JPT_KOD_JE=as.numeric(JPT_KOD_JE)) %>%
  left_join(.,furnaces_final,
            by = c("JPT_KOD_JE"="area_code"))




### roads length in subregions

# following piece of code is commented out because running it might take up to 20min
# The output of this section of the code is the data frame called "roads_length"
# which consist of total road length in metres for all the 182 subregions in malopolska
# calculated using OSM data

# roads <- st_read(here::here("ass_data", 
#                             "geo", 'malopolskie-latest-free',
#                             "gis_osm_roads_free_1.shp"))%>% 
#   st_transform(., 2180)
# 
# roads_length <- lapply(gminy_LP_grouped,function(x) sum(st_length(sf::st_intersection(roads,x))))
# 
# names<- gminy_codes%>% 
#   arrange(., JPT_KOD_JE) %>%
#   dplyr::select(JPT_KOD_JE) %>% 
#   pull()
# 
# 
# roads_df<-as.data.frame(roads_length) 
# colnames(roads_df)<- names
# 
# roads_df<- roads_df %>% 
#   pivot_longer(everything(), 
#                names_to="area_code", 
#                values_to="roads_length_m") %>% 
#   mutate(roads_length_m = as.numeric(gsub("[m]$", "", roads_length_m))) %>%
#   mutate(area_code=as.numeric(area_code))%>% 
#   left_join(.,gminy_codes,by=c("area_code"="JPT_KOD_JE"))
# 
# write.csv(roads_df,here::here("ass_data", 
#                               "csv", 
#                               "roads_length.csv"), row.names = FALSE)


roads <- read_csv(here::here("ass_data", 
                             "csv",
                             "roads_length.csv")) %>% 
  mutate(roads_km=round(roads_lenght_m/1000,3))


### land use data 

# following piece of code is commented out because running it might take up to 20min
# The output of this section of the code is the data frame called "urban_area"
# which consist of percentages of urban development in the subregion for all the 182 subregions in malopolska
# calculated using OSM data

# land <- st_read(here::here("ass_data", 
#                            "geo", 'malopolskie-latest-free',
#                            "gis_osm_landuse_a_free_1.shp"))%>% 
#   st_transform(., 2180)
# 
# urban<-land %>% 
#   dplyr::filter(fclass=="industrial"|fclass=="retail"|fclass=="residential"|fclass=="commercial"|fclass=="heath")
# 
# urban_area <- lapply(gminy_LP_grouped,function(x) sum(st_area(sf::st_intersection(urban,x))))
# total_area <- lapply(gminy_LP_grouped,function(x) st_area(x))
# 
# 
# urban_df<-as.data.frame(urban_area) 
# total_df<- as.data.frame(total_area) 
# 
# colnames(urban_df)<- names
# colnames(total_df)<- names
# 
# urban_df<- urban_df %>% 
#   pivot_longer(everything(), 
#                names_to="area_code", 
#                values_to="urban_area_m2") %>% left_join(.,
#                                                         pivot_longer(total_df,everything(), 
#                                                                      names_to="area_code", 
#                                                                      values_to="total_area_m2"),
#                                                         by="area_code") %>% 
#   mutate(total_area_m2=as.numeric(total_area_m2),
#          urban_area_m2=as.numeric(urban_area_m2),
#          area_code=as.numeric(area_code))%>% 
#   left_join(.,
#             gminy_codes,
#             by=c("area_code"="JPT_KOD_JE")) %>% 
#   mutate(urban_landuse = urban_area_m2/total_area_m2*100) %>% 
#   mutate_at(vars(-area_code,-JPT_NAZWA_), funs(round(., 1)))
# 
# write.csv(urban_df,here::here("ass_data", 
#                               "csv", 
#                               "urban_area.csv"), row.names = FALSE)

urban<-read_csv(here::here("ass_data", 
                           "csv", 
                           "urban_area.csv")) %>% 
  dplyr::select(area_code,urban_landuse,total_area_m2)

### altitude data

# following piece of code is commented out because running it might take up to 20min
# The output of this section of the code is the data frame called "elevation."
# which consist of average altitude for all the 182 subregions in malopolska
# calculated using elevatr package

# ls_border <- lesserpoland %>%
#   st_transform(., 2180) %>% 
#   as(., 'Spatial')
# 
# #### creating the dataframe with SP bbox
# loc_df <- data.frame(x = runif(6,min=sp::bbox(buffer(ls_border,10000))[1,1], 
#                                max=sp::bbox(buffer(ls_border,10000))[1,2]),
#                      y = runif(6,min=sp::bbox(buffer(ls_border,10000))[2,1], 
#                                max=sp::bbox(buffer(ls_border,10000))[2,2]))
# 
# #### getting the altitude data using the elevatr package
# x <- get_elev_raster(locations = loc_df, prj = sp::proj4string(ls_border), z=10)
# 
# elev_mask<-mask(x,ls_border)
# 
# writeRaster(elev_mask, filename=here::here("ass_data", 
#                                            "csv", 
#                                            "ls_elevation.tif"), format="GTiff", overwrite=TRUE)
# 
# #### extracting mean altitude from a raster and adding it to dataframe
# 
# elev_gminy <- lapply(gminy_LP_grouped,function(x) raster::extract(elev_mask,x,small=TRUE)) 
# elev_gminy_mean <- elev_gminy %>% 
#   lapply(.,function(x) as.numeric(as.character(unlist(x)))) %>% 
#   lapply(.,function(x) mean(x,na.rm = TRUE))
# 
# plot(mask(elev_mask,gminy_LP_grouped[[180]]))
# 
# elev_df<-as.data.frame(elev_gminy_mean) 
# 
# colnames(elev_df)<- names
# 
# elev_df<- elev_df %>% 
#   pivot_longer(everything(), 
#                names_to="area_code", 
#                values_to="elevation_m") %>%
#   mutate(area_code=as.numeric(area_code))%>% 
#   left_join(.,
#             gminy_codes,
#             by=c("area_code"="JPT_KOD_JE")) %>% 
#   
#   write.csv(elev_df,here::here("ass_data", 
#                                "csv", 
#                                "elevation.csv"), row.names = FALSE)

#### loading the altitude data
altitude<-read_csv(here::here("ass_data", 
                              "csv", 
                              "elevation.csv")) %>% 
  rename(altitude_m_masl=elevation_m)

### forrest area 

forrest <- read_csv(here::here("ass_data", 
                               "csv",
                               "forrest_area_prc.csv")) %>% 
  slice(.,c(-2))%>% 
  row_to_names(row_number = 1) %>% 
  dplyr::rename(.,area_code=1,area_name=2) %>% 
  mutate(across(.cols = 3:22, as.numeric)) %>% 
  left_join(gminy_codes,.,by=c("JPT_KOD_JE"="area_code")) %>% 
  mutate(forrest_08_19 = round(rowMeans(subset(., select = as.numeric(c(13:23))), na.rm = TRUE),0)) %>% 
  mutate(forrest = forrest_08_19) %>% 
  dplyr::select(JPT_KOD_JE,forrest)


#### gminy budget
budget<-read_csv(here::here("ass_data", 
                            "csv", 
                            "gminy_budget.csv")) %>% 
  slice(.,c(-1,-3))%>% 
  row_to_names(row_number = 1) %>% 
  dplyr::rename(.,area_code=1,area_name=2) %>% 
  mutate(across(.cols = 3:21, as.numeric)) %>% 
  left_join(gminy_codes,.,by=c("JPT_KOD_JE"="area_code")) %>% 
  mutate(budget_08_19 = round(rowMeans(subset(., select = as.numeric(c(11:22))), na.rm = TRUE),2)) %>% 
  mutate(budget_00_08 = round(rowMeans(subset(., select = as.numeric(c(5:10))), na.rm = TRUE),2)) %>% 
  mutate(budget = budget_08_19-budget_00_08) %>% 
  mutate(budget = replace_na(budget, 0)) %>% 
  dplyr::select(JPT_KOD_JE,budget)

#### gminy flats
flats<-read_csv(here::here("ass_data", 
                           "csv", 
                           "flats.csv")) %>% 
  slice(.,c(-2))%>% 
  row_to_names(row_number = 1) %>% 
  dplyr::rename(.,area_code=1,area_name=2) %>% 
  mutate(across(.cols = 3:20, as.numeric)) %>% 
  left_join(gminy_codes,.,by=c("JPT_KOD_JE"="area_code")) %>% 
  mutate(flats_08_19 = round(rowMeans(subset(., select = as.numeric(c(10:21))), na.rm = TRUE),1)) %>% 
  mutate(flats_00_08 = round(rowMeans(subset(., select = as.numeric(c(5:9))), na.rm = TRUE),1)) %>% 
  mutate(flats = flats_08_19-flats_00_08) %>% 
  mutate(flats = replace_na(budget, 0)) %>% 
  dplyr::select(JPT_KOD_JE,flats)


############################## Air quality improvment 


## Descriptive statistics

### assembling the final data frame used for the analysis 

df <- gminy_LP %>% 
  select(JPT_KOD_JE,JPT_NAZWA_) %>% 
  rename(area_code=JPT_KOD_JE,
         area_name=JPT_NAZWA_) %>% 
  mutate(area_code= as.numeric(area_code)) %>% 
  left_join(.,density_dif, by="area_code") %>% 
  left_join(.,furnaces_08_19,by="area_code") %>% 
  left_join(.,pm25_dif, by="area_code") %>% 
  left_join(.,pop_dif, by="area_code") %>% 
  left_join(.,roads[c("area_code","roads_km")], by="area_code") %>% 
  left_join(.,urban, by="area_code") %>% 
  left_join(.,altitude,by="area_code") %>% 
  mutate(roads_density_km2= roads_km/(total_area_m2/1000000)) 

df_tidy<- df %>% 
  st_drop_geometry() %>% 
  dplyr::select(is.numeric,-area_code)%>%
  pivot_longer(everything(),
               names_to="All_variables", 
               values_to="val")%>%
  mutate(All_variables = tolower(All_variables))

### plotting annual polluntion trends in the region of lesserpoland 
ggplot(pollutants_ls_mean, aes(x=year,y=value,colour=pollutant)) + 
  geom_point()+ geom_vline(aes(xintercept="2008"))+
  geom_smooth(aes(group=pollutant), method="lm", se=FALSE)+
  facet_wrap(~ pollutant, ncol=4,scales = "free")

### histogram dependent variable PM25

pm25_hist <- df_tidy%>%
  filter(All_variables=="pm25")%>%
  ggplot(., aes(x=val)) + 
  geom_histogram(aes(x = val, y = ..density..),color="black", fill="white")+
  geom_density(colour="red", size=1, adjust=1)
pm25_hist

### Dependent variable descriptive statistics
summary(df$pm25)

### histogram independent variable 
independent_hist <- df_tidy%>%
  filter(All_variables!="pm25")%>%
  ggplot(., aes(x=val)) + 
  geom_histogram(aes(x = val, y = ..density..),color="black", fill="white")+
  geom_density(colour="red", size=1, adjust=1)+
  facet_wrap(~All_variables, scales = 'free')
independent_hist
### transforming data log1p independent

independent_log1phist <- df_tidy%>%
  filter(All_variables!="pm25" & All_variables!="density_km2" &
           All_variables!="population_dif")%>%
  ggplot(., aes(x=log1p(val))) + 
  geom_histogram(aes(x = log1p(val), y = ..density..),color="black", fill="white")+
  geom_density(colour="red", size=1, adjust=1)+
  facet_wrap(~All_variables, scales = 'free')
independent_log1phist

### boxplots
pm25_box <- df_tidy%>%
  filter(All_variables=="pm25")%>%
  ggplot(., aes(y=val)) + 
  geom_boxplot()
pm25_box

log_furnaces_box <- df_tidy%>%
  filter(All_variables=="furnaces_per_1000")%>%
  ggplot(., aes(y=log1p(val))) + 
  geom_boxplot()
log_furnaces_box

### mapping pm25 and furnaces
tmap_mode("plot")
ls_bb<- st_bbox(lesserpoland,
                crs = st_crs(lesserpoland)) %>% 
  st_as_sfc()

pm25_map <- tm_shape(df, bbbox = ls_bb) + 
  tm_polygons(col='pm25',style="fisher",
              palette="YlOrRd",alpha=1)
pm25_map

furnaces_map <- tm_shape(df, bbbox = ls_bb) + 
  tm_polygons(col='furnaces_per_1000',style="fisher",
              palette="YlOrRd",alpha=1)
furnaces_map

### scatter plot PM25 ~ LOG_ Furnaces

qplot(x = log1p(furnaces_per_1000) , 
      y = pm25, 
      data=df)

##Spatial dependance 

###calculating centroids for regions
coordsR <- df%>%
  st_centroid()%>%
  st_geometry()

###adujusting centroids to stay with in the polygons
coordsR_ad <- df%>%
  st_point_on_surface(.)%>%
  st_geometry()

par(mfrow=c(1,1))

plot(coordsR,axes=TRUE,col="blue")
plot(coordsR_ad,axes=TRUE,col="red",add=TRUE)
plot(gminy_LP$geometry,alpha=0.5, add=TRUE)

## Contiguity

#create a neighbours list

gminy_nb <- df%>%
  poly2nb(., queen=T)

knn_gminy <-coordsR_ad %>%
  knearneigh(., k=4)

gminy_knn <- knn_gminy %>%
  knn2nb()

#plot them
plot(gminy_nb, st_geometry(coordsR_ad), col="red")
plot(gminy_LP$geometry, add=T)

plot(gminy_knn, st_geometry(coordsR_ad), col="blue")
plot(gminy_LP$geometry, add=T)

#create a spatial weights object from these weights
Gminy.queens_weight<- gminy_nb %>%
  nb2listw(., style="C")

Gminy.knn_4_weight<- gminy_knn %>%
  nb2listw(., style="C")


##Testing
#Moran’s I test
I_LsGminy_Global_PM25 <- df%>%
  pull(pm25) %>%
  as.vector()%>%
  moran.test(., Gminy.queens_weight)

I_LsGminy_Global_PM25

#Geary’s C

C_LsGminy_Global_PM25 <- df%>%
  pull(pm25) %>%
  as.vector()%>%
  geary.test(., Gminy.queens_weight)

C_LsGminy_Global_PM25


###Localised statistics
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
##Local Moran’s I

I_LsGminy_Global_PM25 <- df %>%
  pull(pm25) %>%
  as.vector()%>%
  localmoran(., Gminy.queens_weight)%>%
  as_tibble()


df<- df%>%
  mutate(pm25_pvalue =as.numeric(I_LsGminy_Global_PM25$`Pr(z > 0)`))%>%
  mutate(pm25_I =as.numeric(I_LsGminy_Global_PM25$Ii))%>%
  mutate(pm25_Iz =as.numeric(I_LsGminy_Global_PM25$Z.Ii))

#mapping Local Moran’s I
tm_shape(df) +
  tm_polygons(col="pm25_Iz",
              style="fixed",
              breaks=breaks1,
              palette="RdGy",
              midpoint=0,
              title="Local Moran's Iz, PM25 in London")

## Gi* statistic

G_LsGminy_Global_PM25 <- df%>%
  dplyr::arrange(area_code)%>%
  dplyr::pull(pm25) %>%
  as.vector()%>%
  localG(.,Gminy.queens_weight)

df<- df%>%
  dplyr::arrange(area_code)%>%
  dplyr::mutate(pm25_LocGiz = as.numeric(G_LsGminy_Global_PM25))

tm_shape(df) +
  tm_polygons("pm25_LocGiz",
              style="fixed",
              breaks=breaks1,
              palette="RdBu",
              midpoint=NA,
              title="Gi*, PM25")

## OLS regressions

### model1
model1 <- lm(pm25 ~ log1p(furnaces_per_1000) ,
             data = df)
tidy(model1)
summary(model1)



### model2
model2 <- lm(pm25 ~ log1p(furnaces_per_1000)+density_km2+log1p(roads_density_km2)+
               log1p(altitude_m_masl)+log1p(urban_landuse)+population_dif +log(total_area_m2) ,
             data = df)
tidy(model2)
summary(model2)

### model3
model3 <- lm(pm25 ~ log1p(furnaces_per_1000)+density_km2+log1p(roads_density_km2)+
               log1p(altitude_m_masl) + log(total_area_m2),
             data = df)
tidy(model3)
summary(model3)


### OLS Assumptions 

#### residuals distribution analysis analysis 

#save the residuals into your dataframe

model_data <- model3 %>%
  augment(., df)

#plot residuals
model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 

# adding residuals shapelayer
df <- df %>%
  mutate(model3resids = residuals(model3))

#### No Multicolinearity in the independent variables  

Correlation <- df %>%
  st_drop_geometry()%>%
  dplyr::select(furnaces_per_1000,
                density_km2,
                roads_density_km2,
                altitude_m_masl,
                total_area_m2) %>%
  correlate()



#visualise the correlation matrix
rplot(Correlation)

#VIF
vif(model3)

#### Homoscedasticity

#print some model diagnositcs. 
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model3)

#### Independence of Errors

#now plot the residuals
tmap_mode("view")

tm_shape(df) +
  tm_polygons("model3resids",
              palette = "RdYlBu") 

Queen <- df %>%
  st_drop_geometry()%>%
  dplyr::select(model3resids)%>%
  pull()%>%
  moran.test(., Gminy.queens_weight)%>%
  tidy()

Queen

Nearest_neighbour <- df %>%
  st_drop_geometry()%>%
  dplyr::select(model3resids)%>%
  pull()%>%
  moran.test(., Gminy.knn_4_weight)%>%
  tidy()

Nearest_neighbour

## spatial regression

library(spatialreg)

pm25_model3_queen <- lagsarlm(pm25 ~ log1p(furnaces_per_1000)+ density_km2+log1p(roads_density_km2)+
                                log1p(altitude_m_masl)+log1p(total_area_m2), 
                              data = df, 
                              nb2listw(gminy_nb, style="C"), 
                              method = "eigen")

#what do the outputs show?
tidy(pm25_model3_queen)
glance(pm25_model3_queen)
summary(pm25_model3_queen)
#run a spatially-lagged regression model
pm25_model3_knn4 <- lagsarlm(pm25 ~ log1p(furnaces_per_1000)+ density_km2+log1p(roads_density_km2)+
                               log1p(altitude_m_masl)+log1p(total_area_m2), 
                             data = df, 
                             nb2listw(gminy_knn, style="C"), 
                             method = "eigen")

#what do the outputs show?
tidy(pm25_model3_knn4)
glance(pm25_model3_knn4)
summary(pm25_model3_knn4)

## GWR 

dfSP <- df %>%
  as(., "Spatial")

coordsR_adSP <- coordsR_ad %>%
  as(., "Spatial")

library(spgwr)

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(pm25 ~ log1p(furnaces_per_1000)+ density_km2+log1p(roads_density_km2)+
                          log1p(altitude_m_masl), 
                        data = dfSP, 
                        coords=coordsR_adSP,
                        adapt=T)




#run the gwr model
gwr.model = gwr(pm25 ~ log1p(furnaces_per_1000)+ density_km2+log1p(roads_density_km2)+
                  log1p(altitude_m_masl), 
                data = dfSP, 
                coords=coordsR_adSP , 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

#print the results of the model
gwr.model

results <- as.data.frame(gwr.model$SDF)
names(results)


#attach coefficients to original SF


df <- df%>%
  mutate(coefFurnaces = results$log1p.furnaces_per_1000.)

tm_shape(df ) +
  tm_polygons(col = "coefFurnaces", 
              palette = "RdBu", 
              alpha = 1)
