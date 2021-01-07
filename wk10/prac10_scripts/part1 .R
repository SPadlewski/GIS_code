
# make a temp file to store the .zip in

download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", 
              destfile="prac10_data/statistical-gis-boundaries-london.zip")

library(tidyverse)
library(fs)
listfiles<-dir_info(here::here("prac10_data")) %>%
  dplyr::filter(str_detect(path, "statistical-gis-boundaries-london.zip")) %>%
  dplyr::select(path)%>%
  pull()%>%
  #print out the .gz file
  print()%>%
  as.character()%>%
  utils::unzip(exdir=here::here("prac10_data"))

#install.packages("pins")
library(pins)
pinexample<-pin("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip")


# Take the downloaded data and filter it based on the filename 
# that contains: Borough OR Ward_ AND .shp using grepl()

library(here)
boroughsandwards<-dir_info(here::here("prac10_data", 
                                      "statistical-gis-boundaries-london", 
                                      "ESRI"))%>%
  #$ means exact match
  dplyr::filter(str_detect(path, 
                           "London_Ward_CityMerged.shp$|London_Borough_Excluding_MHW.shp$"))%>%
  dplyr::select(path)%>%
  pull()

boroughsandwards

# # Now read in both of the files using map() that applies a function 
# (here st_read() from the sf package) to a list. map() comes from the 
# purrr package which extends R’s functional programming ability.

library(sf)
boroughsandwardssf<-map(boroughsandwards, st_read)
#To map or access each individual shapefile it’s just…
library(tmap)
tmap_mode("plot")
qtm(boroughsandwardssf[[1]])

#Get the data for Airbnb
Airbnb <- read_csv("http://data.insideairbnb.com/united-kingdom/england/london/2019-07-10/visualisations/listings.csv")

#(API)
library(memisc)
download.file("http://download.geofabrik.de/europe/great-britain/england/greater-london-latest-free.shp.zip",
              mode='wb', 
              destfile="prac10_data/geofabrik.zip")

listfiles<-dir_info(here::here("prac10_data")) %>%
  dplyr::filter(str_detect(path, "geofabrik.zip")) %>%
  dplyr::select(path)%>%
  pull()%>%
  #print out the .gz file
  print()%>%
  as.character()%>%
  utils::unzip(exdir=here::here("prac10_data", "geofabrik"))

##(API)
library(osmar)
src <- osmsource_api(url = "https://api.openstreetmap.org/api/0.6/")
# 1000 refers to distance from centre point
bb <- center_bbox(-0.112510,51.507627, 1000, 1000)
LDN <- get_osm(bb, source = src)
plot(LDN)
# extract just highwways
ways <- find(LDN, way(tags(k == "highway")))
hway <- find_down(LDN, way(ways))
hway <- subset(LDN, ids = hway)

# transforming crs
st_transform(boroughsandwardssf[[1]], 27700)
st_transform(boroughsandwardssf[[2]], 27700)

#or use map() again…
boroughsandwardssf<- map(boroughsandwardssf, crs=27700, st_transform)

# change the airbnb data to spatial 
Airbnb <- Airbnb %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326)%>%
  # project it too - remember that 27700 is
  # British National Grid
  st_transform(., 27700)

##Functions
Joinfun <- function(data1, data2){
  
  output<- data1%>%
    st_join(data2,.)%>%
    add_count(GSS_CODE, name="airbnbs") 
  return(output)
}

# without a loop
Airbnbborough <- Joinfun(Airbnb, boroughsandwardssf[[1]])
Airbnbward <- Joinfun(Airbnb, boroughsandwardssf[[2]])


# set up a variable  
basicloop <- 1
# so while our variable is less than 
#6 run the following code within the {}
while (basicloop < 6) {
  #print the varaible (starts at 1)
  print(basicloop)
  # then add 1 to the variable
  basicloop = basicloop+1
  # go back to the start of the loop
  #and if it is still <6 run again
}
 

# here is my empty list
emptylist <- list()

basicloop <- 1

while (basicloop < 6) {
  print(basicloop)
  emptylist[[basicloop]] <- basicloop
  basicloop <- basicloop+1
} 


# get the length - add 1 as we are using less than
boroughlen<-length(boroughsandwardssf)+1
# empty list
hold<-list()
# here is our starting point variable
i<-1


# while i is less than boroughlength 
# max of boroughlength is 3
while (i < boroughlen){
  # put the output in our varible
  # use the function for boroughs and then wards
  hold[[i]] <- Joinfun(Airbnb, boroughsandwardssf[[i]])
  # add one to the index
  i<-i+1
}


# make a quick thematic map to have a look
justairbnbborough<-hold[[1]]%>%
  dplyr::select(GSS_CODE, airbnbs)%>%
  st_drop_geometry()%>%
  distinct(GSS_CODE, airbnbs)%>%
  left_join(boroughsandwardssf[[1]], .,
            by = c("GSS_CODE" = "GSS_CODE"))

justairbnward<-hold[[2]]%>%
  dplyr::select(GSS_CODE, airbnbs)%>%
  st_drop_geometry()%>%
  distinct(GSS_CODE, airbnbs)%>%
  left_join(boroughsandwardssf[[2]], .,
            by = c("GSS_CODE" = "GSS_CODE"))

qtm(justairbnbborough, fill = "airbnbs")
qtm(justairbnward, fill = "airbnbs")


##Advanced mapping

library(classInt)
library(leaflet)
library(leafpop)

# extract only westminster from boroughs

Borough <- hold[[1]] %>%
  filter(NAME=="Westminster")%>%
  # we need to set the projection to WGS84
  # to use with leaflet
  st_transform(., crs=4326)%>%
  #at the moment each airbnb is a row for the borough
  #we just one one row that has number of airbnbs
  group_by(., GSS_CODE, NAME)%>%
  summarise(airbnbs = unique(airbnbs))

wardsatborough <- hold[[2]] %>%
  filter(BOROUGH=="Westminster")%>%
  # we need to set the projection to WGS84
  # to use with leaflet
  st_transform(., crs=4326)%>%
  #at the moment each airbnb is a row for the wards
  #we just one one row that has number of airbnbs
  group_by(., GSS_CODE, NAME)%>%
  summarise(airbnbs = unique(airbnbs))

# set our breaks for the map
breaks1<-classIntervals(hold[[1]]$airbnbs, 
                        n=5, 
                        style = "quantile")
breaks2<-classIntervals(wardsatborough$airbnbs, 
                        n=5, 
                        style = "quantile")

# use the breaks to set our colour palettes
pal <- colorBin(palette = "YlOrRd", 
                domain=hold[[1]]$airbnbs, 
                bins=breaks1$brks)
pal2 <- colorBin(palette = "YlOrRd", 
                 domain=wardsatborough$airbnbs, 
                 bins=breaks2$brks)

# we want a popup of information too
# here we make a new varaible with no 
#spatial info (remove geometry)

wardinfo <-wardsatborough %>%
  st_drop_geometry()%>%
  dplyr::rename(Ward = NAME,
                Airbnbs = airbnbs)%>%
  dplyr::select(Ward, Airbnbs)%>%
  popupTable()

boroughinfo <-wardsatborough %>%
  st_drop_geometry()%>%
  dplyr::rename(Borough = NAME,
                Airbnbs = airbnbs)%>%
  dplyr::select(Borough, Airbnbs)%>%
  popupTable()


wardandboroughs<- leaflet() %>%
  # add basemap options
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addTiles(group = "OSM") %>%
  
  #add our Borough polygons, linking to the tables we just made
  addPolygons(data=Borough,
              color="white", 
              weight = 2,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 0.7,
              popup = boroughinfo,
              fillColor = ~pal(Borough$airbnbs),
              group = "Borough")%>%
  
  #add our ward polygons, linking to the tables we just made
  addPolygons(data=wardsatborough,
              color="white", 
              weight = 2,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 0.7,
              popup = wardinfo,
              fillColor = ~pal2(wardsatborough$airbnbs),
              group = "Wards")%>%
  
  # add a legend for wards
  addLegend(pal = pal2, 
            values = wardsatborough$airbnbs,
            group=c("Wards"), 
            position ="bottomleft",
            title ="Accom count")%>%
  
  # add a legend for boroughs
  addLegend(pal = pal, 
            values = Borough,
            group=c("Borough"), 
            title ="Accom count",
            position ="bottomleft")%>%
  
  # specify layers control
  addLayersControl(
    baseGroups = c("Toner Lite", "OSM"),
    overlayGroups = c("Borough", "Wards"),
    options = layersControlOptions(collapsed = FALSE))%>%
  hideGroup(c("Borough"))

# show us the map
wardandboroughs


# range of Airbnbs per ward in Westminster

Range<-wardsatborough %>%
  summarise(Max = max(airbnbs),
            Min = min (airbnbs))