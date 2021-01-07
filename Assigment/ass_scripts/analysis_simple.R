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

# weather data 2008-2019
weather <- read_csv(here::here("ass_data", 
                                  "csv", 
                                  "weather.csv")) 
#Load all pollutants 
pollutants <- read_csv(here::here("ass_data", 
                           "csv", 
                           "pollution_yearly.csv")) %>% 
  mutate_at(vars(c(40:56)),funs(.*1000))

pollutants_ls_mean<- pollutants%>% 
  summarise_all( mean) %>% 
  pivot_longer(-area_code,
               names_to = "year_pollutant",
               values_to = "value",
               ) %>% 
  select(-area_code) %>% 
  mutate(year= as.factor(str_extract(year_pollutant, "^\\d+")),
         pollutant= as.character(str_extract(year_pollutant, "(?<=_)(.*)")))
  

pm10_08_19<- pollutants%>% 
  dplyr::select(c(1:21))%>%
  mutate(across(.cols = 2:21, as.numeric)) %>% 
  mutate(pm10= round(rowMeans(.[c(11:21)], na.rm = TRUE),1)) %>%
  select(area_code,pm10)

### population
pop <- read_csv(here::here("ass_data", 
                                "csv", 
                                "ludnosc.csv")) %>% 
  slice(.,c(-1,-2,-3,-5)) %>% 
  row_to_names(row_number = 1) %>% 
  dplyr::rename(.,area_code=1,area_name=2) %>% 
  mutate(across(.cols = 3:22, as.numeric))%>% 
  mutate(population_08_19 = round(rowMeans(subset(., select = as.numeric(c(3:22))), na.rm = TRUE),0))

pop_08_19 <- pop %>% 
  dplyr::select(area_code,population_08_19)

pop_tidy <-pop %>% 
  mutate(across(.cols = 3:22, as.numeric))%>% 
  dplyr::select(-area_name,-mean_08_19)%>% 
  pivot_longer(-area_code,
               names_to = "year",
               values_to = "population",)

### Density
density <- read_csv(here::here("ass_data", 
                           "csv", 
                           "density.csv")) %>% 
  slice(.,-2) %>% 
  row_to_names(row_number = 1) %>% 
  dplyr::rename(.,area_code=1,area_name=2)  %>% 
  right_join(.,gminy_codes, by=c("area_code"="JPT_KOD_JE"))

#panel data
density_tidy <-density %>% 
  mutate(across(.cols = 3:20, as.numeric))%>% 
  dplyr::select(-area_name)%>% 
  pivot_longer(c(-area_code,-JPT_NAZWA_),
               names_to = "year",
               values_to = "density_km2",)
#period 08-19
density_08_19 <- density %>% 
  dplyr::select(c(1,9:20))%>%
  mutate(across(.cols = 2:13, as.numeric)) %>% 
  mutate(density_km2= round(rowMeans(.[c(2:13)], na.rm = TRUE),0)) %>%
  select(area_code,density_km2)

colnames(density_08_19)
### tourists
tourists <- read_csv(here::here("ass_data", 
                               "csv", 
                               "tourists.csv")) %>% 
  slice(.,c(-1,-3)) %>% 
  row_to_names(row_number = 1) %>% 
  dplyr::rename(.,area_code=1,area_name=2) %>% 
  right_join(.,gminy_codes, by=c("area_code"="JPT_KOD_JE")) 

tourists_tidy <-tourists %>% 
  mutate(across(.cols = 3:22, as.numeric))%>% 
  dplyr::select(-area_name)%>% 
  pivot_longer(c(-area_code,-JPT_NAZWA_),
               names_to = "year",
               values_to = "tourists",) %>% 
  left_join(.,pop_tidy,by=c("area_code","year")) %>% 
  mutate(tourists_per_1000=round(tourists/(population/1000),1),
         tourists_per_1000=replace_na(tourists_per_1000,0)) %>% 
  dplyr::select(area_code,year,tourists_per_1000)

tourists_08_19 <- tourists %>% 
  mutate(across(.cols = 11:22, as.numeric)) %>% 
  mutate(all_tourists= rowSums(.[c(11:22)], na.rm = TRUE)) %>%
  dplyr::select(c(area_code,all_tourists))

# furnaces 

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
  
furnaces_08_19 <- furnaces_final %>% 
  dplyr::select(area_code,furnaces_per_1000)

gminy_LP_fur <- gminy_LP %>%
  mutate(JPT_KOD_JE=as.numeric(JPT_KOD_JE)) %>%
  left_join(.,furnaces_final,
            by = c("JPT_KOD_JE"="area_code"))



#### roads


roads <- read_csv(here::here("ass_data", 
                            "csv",
                            "roads_lenght.csv")) %>% 
  mutate(roads_km=round(roads_lenght_m/1000,3))

colnames(roads)

#### land use



urban<-read_csv(here::here("ass_data", 
                              "csv", 
                              "urban_area.csv")) %>% 
  dplyr::select(area_code,urban_landuse,total_area_m2)

colnames()


##evelation


altitude<-read_csv(here::here("ass_data", 
                              "csv", 
                              "elevation.csv")) %>% 
  rename(altitude_m_masl=elevation_m)



##furnaces
tmap_mode("view")

tm_shape(lesserpoland) +
  tm_polygons(col = NA, alpha = 1) +
  tm_shape(powiaty_LP) +
  tm_polygons(col = NA, alpha=0.5) +
  tm_shape(gminy_LP_fur) +
  tm_polygons(col = "all_removed_per_1000", breaks=c(0,5,10,15,20,25,30,35,40,45,50,100),alpha=0.5) #seq(100, 4300 ,by=10)

## stations  
tm_shape(pm10_ls) +
  tm_dots(col=NA)

#quick plot

# Basic scatter plot
ggplot(pm10_ls, aes(x=Rok, y=Średnia)) + geom_point()
# Change the point size, and shape
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)

#####  Analysis

#plotting annual polluntion trends in the region of lesserpoland 
ggplot(pollutants_ls_mean, aes(x=year,y=value,colour=pollutant)) + 
  geom_point()+ geom_vline(aes(xintercept="2008"))+
  geom_smooth(aes(group=pollutant), method="lm", se=FALSE)+
  facet_wrap(~ pollutant, ncol=4,scales = "free")

# H0 - intruduced policy is not influancing the air quality

#regression dataset

df <- pollutants %>% 
  pivot_longer(-area_code,names_to = "year_pollutant",
               values_to = "value") %>% 
  mutate(year= as.factor(str_extract(year_pollutant, "^\\d+")),
         pollutant= as.character(str_extract(year_pollutant, "(?<=_)(.*)"))) %>% 
  pivot_wider(names_from = pollutant,values_from = value,id_cols = c(area_code,year)) %>% 
  mutate(policy = case_when(as.numeric(as.character(year))>=2008~1,TRUE~0)) %>% 
  left_join(.,density_tidy,by=c("area_code","year")) %>% 
  left_join(.,tourists_tidy,by=c("area_code","year"))

#simple regression
reg <- lm(pm10~density_km2 + tourists_per_1000 + factor(policy) + factor(year)+factor(area_code),data=df)
summary(reg)

sim_reg <- lm(pm10~factor(policy) ,data=df)
summary(reg)

ggplot(df, aes(y=pm10,x=factor(year))) + 
  geom_point()

pollutants_ls_mean<- pollutants%>% 
  summarise_all( mean) %>% 
  pivot_longer(-area_code,
               names_to = "year_pollutant",
               values_to = "value",) %>% 
  select(-area_code) %>% 
  mutate(year= as.factor(str_extract(year_pollutant, "^\\d+")),
         pollutant= as.character(str_extract(year_pollutant, "(?<=_)(.*)")))


# main analysis dataset 

df_2 <- gminy_LP %>% 
  select(JPT_KOD_JE,JPT_NAZWA_) %>% 
  rename(area_code=JPT_KOD_JE,
         area_name=JPT_NAZWA_) %>% 
  mutate(area_code= as.numeric(area_code)) %>% 
  left_join(.,density_08_19, by="area_code") %>% 
  left_join(.,pm10_dif, by="area_code") %>% 
  left_join(.,pop_08_19, by="area_code") %>% 
  left_join(.,roads[c("area_code","roads_km")], by="area_code") %>% 
  left_join(.,tourists_08_19, by="area_code") %>% 
  left_join(.,urban, by="area_code") %>% 
  left_join(.,altitude,by="area_code") %>% 
  left_join(.,weather,by="area_code") %>% 
  mutate(roads_density_km2= roads_km/(total_area_m2/1000000)) %>% 
  mutate(total_tourists_per_1000= all_tourists/(population_08_19/1000)) 

df_2_log <- df_2 %>% 
  st_drop_geometry() %>% 
  dplyr::select(!c(area_name,area_code, JPT_NAZWA_)) %>% 
  log1p(.)
  
  

##descriptive statistics

df_2_tidy<- df_2 %>% 
  st_drop_geometry() %>% 
  dplyr::select(is.numeric,-area_code)%>%
  pivot_longer(everything(),
               names_to="All_variables", 
               values_to="val")%>%
  mutate(All_variables = tolower(All_variables))
#histogram dependent variable 
pm10_hist <- df_2_tidy%>%
  filter(All_variables=="pm10")%>%
  ggplot(., aes(x=val)) + 
  geom_histogram(aes(x = val, y = ..density..),color="black", fill="white")+
  geom_density(colour="red", size=1, adjust=1)
pm10_hist

#histogram independent variable 
independent_hist <- df_2_tidy%>%
  filter(All_variables!="pm10")%>%
  ggplot(., aes(x=val)) + 
  geom_histogram(aes(x = val, y = ..density..),color="black", fill="white")+
  geom_density(colour="red", size=1, adjust=1)+
  facet_wrap(~All_variables, scales = 'free')
independent_hist
##transforming data
#log1p independent
independent_log1phist <- df_2_tidy%>%
  filter(All_variables!="pm10")%>%
  ggplot(., aes(x=log1p(val))) + 
  geom_histogram(aes(x = log1p(val), y = ..density..),color="black", fill="white")+
  geom_density(colour="red", size=1, adjust=1)+
  facet_wrap(~All_variables, scales = 'free')
independent_log1phist


symbox(~urban_landuse, 
       df_2, 
       na.rm=T,
       powers=seq(-10,10,by=1))

#boxplot
pm10_box <- df_2_tidy%>%
  filter(All_variables=="pm10")%>%
  ggplot(., aes(y=val)) + 
  geom_boxplot()
pm10_box
#mapping
tmap_mode("plot")
ls_bb<- st_bbox(lesserpoland,
                crs = st_crs(lesserpoland)) %>% 
  st_as_sfc()

pm10_map <- tm_shape(df_2, bbbox = ls_bb) + 
   tm_polygons(col='pm10',style="fisher",
                palette="YlOrRd",alpha=0.5)
pm10_map

###Spatial dependance 

#calculating centroids for regions
coordsR <- df_2%>%
  st_centroid()%>%
  st_geometry()

#adujusting  centroids
coordsR_ad <- df_2%>%
  st_point_on_surface(.)%>%
  st_geometry()

plot(coordsR,axes=TRUE)
plot(coordsR_ad,axes=TRUE,col="red",add=TRUE)
plot(gminy_LP$geometry,alpha=0.5, add=TRUE)

##Contiguity

#create a neighbours list

gminy_nb <- df_2 %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_gminy <-coordsR_ad %>%
  knearneigh(., k=4)

gminy_knn <- knn_gminy %>%
  knn2nb()

#plot them
plot(gminy_nb, st_geometry(coordsR_ad), col="red")

plot(gminy_knn, st_geometry(coordsR_ad), col="blue")
#add a map underneath
plot(gminy_LP$geometry, add=T)

#create a spatial weights object from these weights
LsGminy.lw <- gminy_nb %>%
  nb2listw(., style="C")

##nearest neighbours ?
knn_gminy <-coordsR_ad %>%
  knearneigh(., k=4)

##Testing
#Moran’s I test
I_LsGminy_Global_PM10 <- df_2 %>%
  pull(pm10) %>%
  as.vector()%>%
  moran.test(., LsGminy.lw)

I_LsGminy_Global_PM10
#Geary’s C

C_LsGminy_Global_PM10 <- df_2 %>%
  pull(pm10) %>%
  as.vector()%>%
  geary.test(., LsGminy.lw)

C_LsGminy_Global_PM10
#Getis-Ord global G statistic

G_LsGminy_Global_PM10 <- df_2 %>%
  pull(pm10) %>%
  as.vector()%>%
  globalG.test(., LsGminy.lw)

G_LsGminy_Global_PM10

###Localised statistics
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
##Local Moran’s I

I_LsGminy_Global_PM10 <- df_2 %>%
  pull(pm10) %>%
  as.vector()%>%
  localmoran(., LsGminy.lw)%>%
  as_tibble()

I_LsGminy_Global_furnaces <- df_2 %>%
  pull(furnaces_per_1000) %>%
  as.vector()%>%
  localmoran(., LsGminy.lw)%>%
  as_tibble()

head(I_LsGminy_Global_furnaces)
df_2<- df_2 %>%
  mutate(pm10_I = as.numeric(I_LsGminy_Global_PM10$Ii))%>%
  mutate(pm10_Iz =as.numeric(I_LsGminy_Global_PM10$Z.Ii))%>%
  mutate(pm10_pvalue =as.numeric(I_LsGminy_Global_PM10$`Pr(z > 0)`))%>%
  mutate(furnaces_pvalue =as.numeric(I_LsGminy_Global_furnaces$`Pr(z > 0)`))%>%
  mutate(furnaces_I =as.numeric(I_LsGminy_Global_furnaces$Ii))%>%
  mutate(furnaces_Iz =as.numeric(I_LsGminy_Global_furnaces$Z.Ii))

#mapping Local Moran’s I
tm_shape(df_2) +
  tm_polygons(col="pm10_Iz",
              style="fixed",
              breaks=breaks1,
              palette="RdGy",
              midpoint=NA,
              title="Local Moran's I, Pm10 in London")

## Gi* statistic
G_LsGminy_Global_PM10 <- df_2 %>%
  dplyr::arrange(area_code)%>%
  dplyr::pull(pm10) %>%
  as.vector()%>%
  localG(., LsGminy.lw)

df_2<- df_2 %>%
  dplyr::arrange(area_code)%>%
  dplyr::mutate(pm10_LocGiz = as.numeric(G_LsGminy_Global_PM10))

tm_shape(df_2) +
  tm_polygons("pm10_LocGiz",
              style="fixed",
              breaks=breaks1,
              palette="RdBu",
              midpoint=NA,
              title="Gi*, PM10")

##GWSS


## OLS regressions

#model1
model1 <- lm(pm10 ~ log1p(furnaces_per_1000) ,
             data = df_2)
tidy(model1)
summary(model1)

#model2
colnames(df_2)
model2 <- lm(pm10 ~ log1p(density_km2)+log1p(roads_density_km2)+
               log1p(altitude_m_masl)+log1p(urban_landuse)+log1p(total_tourists_per_1000)+log(total_area_m2)  ,
             data = df_2)
tidy(model2)
summary(model2)



#model3
colnames(df_2)
model3 <- lm(pm10 ~ log1p(density_km2)+log1p(roads_density_km2)+
               log1p(altitude_m_masl)+log1p(urban_landuse)   ,
             data = df_2)
tidy(model3)
summary(model3)

df_2 <-df_2 %>% 
  mutate(model3resids = residuals(model3))

# quick plot


model2 <- lm(pm10 ~ log1p(furnaces_per_1000) ,
             data = df_2)

#now plot the residuals
tmap_mode("view")
#qtm(LonWardProfiles, fill = "model1_resids")

tm_shape(df_2) +
  tm_polygons("model3resids",
              palette = "RdYlBu") 

#spatial regression

library(spatialreg)

pm10_model3_queen <- lagsarlm(pm10 ~ log1p(density_km2)+log1p(roads_density_km2)+
                                log1p(altitude_m_masl)+log1p(urban_landuse) , 
                                 data = df_2, 
                                 nb2listw(gminy_nb, style="C"), 
                                 method = "eigen")

#what do the outputs show?
tidy(pm10_model3_queen)
glance(pm10_model3_queen)
#run a spatially-lagged regression model
pm10_model3_knn4 <- lagsarlm(pm10 ~ log1p(density_km2)+log1p(roads_density_km2)+
                                log1p(altitude_m_masl)+log1p(urban_landuse) , 
                              data = df_2, 
                              nb2listw(gminy_knn, style="C"), 
                              method = "eigen")

#what do the outputs show?
tidy(pm10_model3_knn4)
glance(pm10_model3_knn4)
summary(pm10_model3_knn4)