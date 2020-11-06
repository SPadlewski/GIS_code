##Load all our data
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
library(dplyr)
library(tidyr)
library(raster)
library(plotly)
library(raster)
library(ggplot2)
library(gstat)
  
#loading data
Londonward <- st_read(here::here("Prac5_data",
                                    "statistical-gis-boundaries-london", 
                                    "ESRI", 
                                    "London_Ward.shp"))%>%
  st_transform(., 27700)

Camden <- Londonborough %>% 
  filter(.,NAME=="Camden")

#Cleaning data
Rightmove_2 <- Rightmve %>% 
  mutate(price_pcm=as.numeric(gsub('pcm|£|,','',price_pcm)))%>% 
  drop_na(price_pcm)

One_bed <- Rightmove_2 %>% 
  filter(title=="1 bedroom apartment" | title=="1 bedroom flat")

#trimming outliers
One_bed <- One_bed %>% 
  filter(price_pcm> quantile(price_pcm,0.25),
         price_pcm< quantile(price_pcm,0.75))

#Visualising data

plot(Rightmove_2$price_pcm)

median(One_bed$price_pcm)



#Histogram

ggplot(data=One_bed,mapping=aes(x=price_pcm))+
  geom_histogram(color="black",
                 fill="blue",
                 binwidth = 50)+
  geom_vline(aes(xintercept=median(price_pcm)),
             color="red",
             linetype="dashed",
             size=1
             )+
  geom_vline(aes(xintercept=mean(price_pcm)),
             color="green",
             linetype="dashed",
             size=1
  )


#geom_bar(mapping=aes(x=price_pcm))




#Adding Geodata
Rightmove_geo <- Rightmove_2 %>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)

One_bed_geo <- One_bed %>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)


plot(Rightmove_geo$geometry)

#Interpolation

plot(Camden$geometry)
plot(st_geometry(One_bed_geo),add=TRUE)


CamdenSP <- Camden %>% 
  as(., 'Spatial')

One_bed_geo_SP <- One_bed_geo %>% 
  as(., 'Spatial') 

emptygrd <- as.data.frame(spsample(CamdenSP, n=100000, type="regular", cellsize=10))

names(emptygrd) <- c("X", "Y")

coordinates(emptygrd) <- c("X", "Y")

gridded(emptygrd) <- TRUE  # Create SpatialPixel object
fullgrid(emptygrd) <- TRUE  # Create SpatialGrid object

# Add the projection to the grid
proj4string(emptygrd) <- proj4string(One_bed_geo_SP)


# Interpolate the grid cells using a power value of 2 
interpolate <- gstat::idw(price_pcm~1,One_bed_geo_SP, newdata=emptygrd, idp=2.0)

# Convert output to raster object 
ras <- raster(interpolate)
# Clip the raster to Australia outline
rasmask <- mask(ras, Camden)
# Plot the raster
plot(rasmask,add=TRUE)

# Adding basemap OSM
tmapcamden <- Camden %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

tmap_mode("plot")

tm_shape(tmapcamden)+
  tm_rgb()+
  tm_shape(rasmask)+
  tm_raster(palette = c("red","yellow"),alpha = 0.70)
