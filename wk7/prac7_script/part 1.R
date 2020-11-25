#libraries
install.packages("googledrive")
library(googledrive)
library(tidyverse)
library(fs)
library(stringr)
library(utils)
library(here)
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(rasterVis)
library(ggplot2)
library(sf)

#getting data
o<-drive_download("https://drive.google.com/open?id=1MV7ym_LW3Pz3MxHrk-qErN1c_nR0NWXy",
                  path="prac7_data/exampleGoogleDrivedata/LC08_L1TP_203023_20190513_20190521_01_T1.tar.gz", 
                  overwrite=T)


#unzipping
listfiles<-dir_info(here::here("prac7_data", "exampleGoogleDrivedata")) %>%
  dplyr::filter(str_detect(path, ".gz")) %>%
  dplyr::select(path)%>%
  dplyr::pull()%>%
  #print out the .gz file
  print()%>%
  as.character()%>%
  utils::untar(exdir=here::here("prac7_data", "exampleGoogleDrivedata"))


# List your raster files excluding band 8 using the patter argument
listlandsat<-dir_info(here::here("prac7_data", "Lsatdata"))%>%
  dplyr::filter(str_detect(path, "[B123456790].TIF")) %>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()%>%
  # Load our raster layers into a stack
  stack()

# Load the manchester boundary
manchester_boundary <- st_read(here::here("prac7_data", 
                                          "manchester_boundary_download",
                                          "manchester_boundary.shp"))

crs(manchester_boundary)
crs(listlandsat)


# get band 8
b8list<-dir_info(here::here("prac7_data", "Lsatdata"))%>%
  dplyr::filter(str_detect(path, "[B8].TIF")) %>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()%>%
  raster()

listlandsat <- listlandsat %>%
  addLayer(., b8backin)