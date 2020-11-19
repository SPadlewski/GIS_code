#first library a few packages that we will use during the practical
#note you may need to install them first...
#Installing
install.packages("spatstat")
install.packages("GISTools")

#libraries
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(stringr)
library(tidyverse)
library(raster)
library(fpc)
library(dbscan)
library(ggplot2)
library(tidyverse)
library(purrr)
library(OpenStreetMap)

##First, get the London Borough Boundaries
LondonBoroughs <- st_read(here::here("..","wk5/Prac5_data", "statistical-gis-boundaries-london", "ESRI", "London_Borough_Excluding_MHW.shp"))

BoroughMap <- LondonBoroughs %>% 
  st_transform(.,27700)

qtm(BoroughMap)

summary(BoroughMap)

##Now get the location of all Blue Plaques in the City
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")

geojson_write(BluePlaques,file = here("prac6_data", "BluePlaques.geojson"))

BluePlaques <- st_read(here("prac6_data","BluePlaques.geojson")) %>% 
  st_transform(.,27700)

summary(BluePlaques)

#plot the blue plaques in the city
tmap_mode("view")

tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

#remove duplicates
BluePlaques <- distinct(BluePlaques)

#remove BluePlaques outside of london
BluePlaques <- BluePlaques[BoroughMap,]
BluePlaquesSub <- BluePlaques[BoroughMap,]

tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

#extract the borough

Camden <- BoroughMap %>%
  filter(., NAME=="Camden")

#Check to see that the correct borough has been pulled out
tm_shape(Camden) +
  tm_polygons(col = NA, alpha = 0.5)

#clipping data 
BluePlaquesSub <- BluePlaques[Camden,]

tm_shape(Camden) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")


#now set a window as the borough boundary
window <- as.owin(Camden)
plot(window)

#create a ppp object
BluePlaquesSub<- BluePlaquesSub %>%
  as(., 'Spatial')

BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1],
                          y=BluePlaquesSub@coords[,2],
                          window=window)

BluePlaquesSub.ppp %>%
  plot(.,pch=20,cex=0.5, 
       main="Blue Plaques Camden")

BluePlaquesSub.ppp %>%
  density(., sigma=300) %>%
  plot()

#First plot the points
plot(BluePlaquesSub.ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Camden")

#now count the points in that fall in a 6 x 6
#grid overlaid across the windowBluePlaquesSub.ppp2<-BluePlaquesSub.ppp %>%
BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 10, ny = 10)%>%
  plot(., add=T, col="red")

#run the quadrat count
Qcount <- BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 10, ny = 10) %>%
  as.data.frame() %>%
  dplyr::count(Var1=Freq)%>%
  dplyr::rename(Freqquadratcount=n)

Qcount %>% 
  summarise_all(class)

sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(-Var1) 

lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)

QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))


#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,20),c(0,20), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)


teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 10, ny = 10)

plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Camden")
plot(teststats, add=T, col = "red")


#Ripley’s K

K <- BluePlaquesSub.ppp %>%
  Kest(., correction="border") %>% 
  plot(xaxt="none") 
  axis(1, seq(0,1750,25))


#DBSCAN

#first check the coordinate reference system of the Harrow spatial polygon:
st_geometry(BoroughMap)


#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- BluePlaquesSub %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 350, MinPts = 10)

#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add=T)

BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=10)

db$cluster

#adding cluster membership info back into our dataframe
BluePlaquesSubPoints<- BluePlaquesSubPoints %>%
  mutate(dbcluster=db$cluster)

chulls <- BluePlaquesSubPoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)

chulls <- chulls %>%
  filter(dbcluster >=1)

#better way
chulls_1<- BluePlaquesSubPoints %>%
  group_by(dbcluster) %>%
  nest() %>% 
  mutate(hull= map(data,~with(.x,chull(coords.x1,coords.x2))),
         out = map2(data, hull, ~ .x[.y,,drop=FALSE]))%>%
  select(-data) %>%
  unnest()

chulls_1 <- chulls_1 %>%
  filter(dbcluster >=1)

#chulls2 <- ddply(BluePlaquesSubPoints, .(dbcluster), 
#  function(df) df[chull(df$coords.x1, df$coords.x2), ])




dbplot <- ggplot(data=BluePlaquesSubPoints, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls_1, 
                                aes(coords.x1,coords.x2,group=dbcluster), 
                                alpha = 0.5) 

#dbplot <- dbplot + geom_point(data=chulls_2[c(116,185,144,93,135,132,184,63,113,156,157),],size=10 )
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()

###add a basemap
##First get the bbox in lat long for Harrow
CamdenWGSbb <- Camden %>%
  st_transform(., 4326)%>%
  st_bbox()

CamdenWGSbb

basemap <- OpenStreetMap::openmap(c(51.5126521,-0.2135013),c(51.5729787,-0.1053499),
                                  zoom=NULL,
                                  "stamen-toner")
# convert the basemap to British National Grid
basemap_bng <- openproj(basemap, projection="+init=epsg:27700")

#plotting
autoplot.OpenStreetMap(basemap_bng) + 
  geom_point(data=BluePlaquesSubPoints, 
             aes(coords.x1,coords.x2, 
                 colour=dbcluster, 
                 fill=dbcluster)) + 
  geom_polygon(data = chulls_1, 
               aes(coords.x1,coords.x2, 
                   group=dbcluster,
                   fill=dbcluster), 
               alpha = 0.5)  

############## PART 2 ###################

#read the ward data in
LondonWards <- st_read(here::here("prac6_data/London-wards-2018/London-wards-2018_ESRI", "London_Ward.shp"))

#have a look to check that it's 
#in the right projection
st_crs(LondonWards)

tmap_mode("view")
tm_shape(LondonWards, alpha=.5) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

BluePlaquesSub <- BluePlaques[LondonWards,]
#counting blueplaques in each wards
points_sf_joined_2 <- LondonWards%>%
  st_join(BluePlaquesSub)%>%
  add_count(NAME)%>%
  dplyr::distinct(.)
  #calcualte area
  mutate(area=st_area(.))%>%
  summarise(NAME)
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, WardName, Wardcode, n, AvgGCSE201)
