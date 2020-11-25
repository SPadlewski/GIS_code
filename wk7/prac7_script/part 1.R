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
library(GGally)
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
                                          "Manchester_boundary",
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

## ngb is a nearest neighbour sampling method
b8correct <- b8list%>%
  resample(., listlandsat$LC08_L1TP_203023_20190513_20190521_01_T1_B1,
           method = "ngb") %>%
  # Write out the raster
  writeRaster(.,str_c(here::here("prac7_data", 
                                 "Lsatdata"), 
                      names(b8list), 
                      sep="/"),
              format='GTiff', 
              overwrite=TRUE)


b8backin<-dir_info(here::here("prac7_data", "Lsatdata"))%>%
  dplyr::filter(str_detect(path, "[B8].tif")) %>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()%>%
  raster()

listlandsat <- listlandsat %>%
  addLayer(., b8backin)

raster::compareRaster(listlandsat$LC08_L1TP_203023_20190513_20190521_01_T1_B1,
                      listlandsat$LC08_L1TP_203023_20190513_20190521_01_T1_B8)

#clipping
lsatmask <- listlandsat %>%
  # now crop our temp data to the extent
  crop(.,manchester_boundary)%>%
  mask(.,  manchester_boundary)

# add mask to the filenames within the raster stack

names(lsatmask) <- names(lsatmask)%>%
  str_c(., 
        "mask", 
        sep="_")

# I need to write mine out in another location
outputfilenames <-
  str_c("prac7_data/Lsatdata/", "mask/", names(lsatmask) ,sep="")

#writing masked files 
lsatmask %>%
  writeRaster(., outputfilenames, 
              bylayer=TRUE, 
              format='GTiff', 
              overwrite=TRUE)


# either read them back in from the saved file:

manc_files<-dir_info(here::here("prac7_data", "Lsatdata", "mask")) %>%
  dplyr::filter(str_detect(path, "[B1234567]_mask.tif")) %>%
  dplyr::filter(str_detect(path, "B11", negate=TRUE))%>%
  dplyr::select(path)%>%
  pull()%>%
  stack()

# or extract them from the original stack
manc<-stack(lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B1_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B2_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B3_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B4_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B5_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B6_mask,
            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B7_mask)

# Name the Bands based on where they sample the electromagentic spectrum
names(manc) <- c('ultra-blue', 'blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2') 

crs(manc) # projection
extent(manc) # extent
ncell(manc) # number of cells
dim(manc) # number of rows, columns, layers
nlayers(manc) # number of layers
res(manc) # xres, yres

# true colour composite
manc_rgb <- stack(manc$red, manc$green, manc$blue)
# false colour composite
manc_false <- stack(manc$NIR, manc$red, manc$green)

manc_rgb %>%
  plotRGB(.,axes=TRUE, stretch="lin")

manc_false %>%
  plotRGB(.,axes=TRUE, stretch="lin")

# Looking at single bands
plot(manc$SWIR2)

## How are these bands different?
#set the plot window size (2 by 2)
par(mfrow = c(2,2))
#plot the bands
plot(manc$blue, main = "Blue")
plot(manc$green, main = "Green")
plot(manc$red, main = "Red")
plot(manc$NIR, main = "NIR")

## Look at the stats of these bands
pairs(manc[[1:7]])

manc %>%
  as.data.frame(., na.rm=TRUE)%>%
  sample_n(., 100)%>%
  ggpairs(.,axisLabels="none")

# NDVI function
myfunction <- function(arg1, arg2, ... ){
  statements
  return(object)
}

NDVIfun <- function(NIR, Red) {
  NDVI <- (NIR - Red) / (NIR + Red)
  return(NDVI)
}

ndvi <- NDVIfun(manc$NIR, manc$red)

dev.off()

ndvi %>%
  plot(.,col = rev(terrain.colors(10)), main = "Landsat-NDVI")


# Let's look at the histogram for this dataset
ndvi %>%
  hist(., breaks = 40, main = "NDVI Histogram", xlim = c(-.3,.8))

veg <- ndvi %>%
  reclassify(., cbind(-Inf, 0.3, NA))

veg %>%
  plot(.,main = 'Possible Veg cover')


manc_rgb %>%
  plotRGB(.,axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")

veg %>%
  plot(., add=TRUE, legend=FALSE)

#Calucating tempearture from Landsat data

library(RStoolbox)

MTL<-dir_info(here::here("prac7_data", "Lsatdata")) %>%
  dplyr::filter(str_detect(path, "MTL.txt")) %>%
  dplyr::select(path)%>%
  pull()%>%
  readMeta()

#To see all the attributes
head(MTL)