#install.packages("highcharter")
library(highcharter)
library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(raster)
library(downloader)
library(rgdal)
library(tmap)
library(tidymodels)
#install.packages("tidymodels")

LondonWards <- st_read(here::here("prac8_data", 
                                  "NewLondonWard",
                                  "NewLondonWard.shp"))

extradata <- read_csv(here::here("prac8_data", "LondonAdditionalDataFixed.csv"))


#joining datsets
LondonWardsleftjoin <- LondonWards %>%
  left_join(.,extradata,
            by = c("WD11CD" = "Wardcode"))

#LondonWardsSF <- merge(LondonWards, extradata, by.x = "WD11CD", by.y = "Wardcode")



#check which variables are numeric first

Datatypelist <- LondonWardsleftjoin %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

#make groups based on types of variables
Groups <- LondonWardsleftjoin %>% 
  st_drop_geometry()%>%
  dplyr::select(is.numeric)%>%
  pivot_longer(everything(),
               names_to="All_variables", 
               values_to="val")%>%
  mutate(All_variables = tolower(All_variables))%>%
  mutate(group = case_when(str_detect(All_variables, "age") ~ "Age",
                           str_detect(All_variables, "employ|income|job|jsa") ~ "Employment",
                           str_detect(All_variables,
                                      "house|rent|detatched|flat|terrace|owned|social|private|share|tax|mortgage") ~ "Housing", TRUE~"Other"))

Agehist <- Groups%>%
  filter(group=="Employment")%>%
  ggplot(., aes(x=log10(val))) + 
  geom_histogram(aes(x = log10(val), y = ..density..))+
  geom_density(colour="red", size=1, adjust=1)+
  facet_wrap(~All_variables, scales = 'free')



Agehist


Londonpoint <- ggplot(LondonWardsleftjoin, aes(x=x.y,y=y.y))+geom_point()+coord_equal()
Londonpoint

Londonpoint<-ggplot(LondonWardsleftjoin, aes(x=x.y,y=y.y))+stat_bin2d(bins=15)+coord_equal()
Londonpoint


Londonpoint<-ggplot(LondonWardsleftjoin, aes(x=x.y,y=y.y))+geom_point()+coord_equal()
Londonpoint

Londonpoint+stat_density2d(aes(fill = ..level..), geom="polygon")

#Function to recode data
newvar<-0
recode<-function(variable,high,medium,low){
  newvar[variable<=high]<-"High"
  newvar[variable<=medium]<-"Medium"
  newvar[variable<=low]<-"Low"
  return(newvar)
}


attach(LondonWards)
#Check the name of your column, there could be a slight error and it might be called 'AvgGCSED201'
summary(AvgGCSE201) 

LondonWards <- LondonWards %>% 
  mutate(GCSE_recode=recode(AvgGCSE201,409.1,358.3,332.3))

summary(UnauthAbse) 
LondonWards <- LondonWards %>% 
  mutate(unauth_recode=recode(UnauthAbse,2.4675,1.4105,0.8215))



#Location Quotient function 1
LQ1<-function(pctVariable){
  pctVariable /mean(pctVariable)
}
#Location Quotient function 2
LQ2<-function(variable,rowtotal){
  localprop<-variable/rowtotal
  globalprop<-sum(variable)/sum(rowtotal)
  return(localprop/globalprop)
}


head(LondonWards[,1:7])


#Calculate Quotients for (Owner Occupied, Private Rent, 
#Social Rent, Shared Ownership, Rent Free)

head(LondonWards[,39:43])

LondonWards <- LondonWards %>% 
  mutate(newLQOwned = LQ1(PctOwned20))

LondonWards <- LondonWards %>% 
  mutate(newLQSocial = LQ1(PctSocialR))

LondonWards <- LondonWards %>% 
  mutate(newLQPrivate = LQ1(PctPrivate))

LondonWards <- LondonWards %>% 
  mutate(newLQSharedO = LQ1(PctSharedO))

LondonWards <- LondonWards %>% 
  mutate(newLQRentFre = LQ1(PctRentFre))

#plotting result on the map 
tmap_mode("plot")

# set the breaks
# for our mapped data
breaks = c(0, 5, 12, 26, 57, 286) 

# plot each map
tm1 <- tm_shape(LondonWards) + 
  tm_polygons("PctRentFre",palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)

tm2 <- tm_shape(LondonWards) + 
  tm_polygons("PctSharedO",palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(b)", position=c(0,0.85), size=1.5)

tm3 <- tm_shape(LondonWards) + 
  tm_polygons("PctPrivate",palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(c)", position=c(0,0.85), size=1.5)

tm4 <- tm_shape(LondonWards) + 
  tm_polygons("PctSocialR",palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(d)", position=c(0,0.85), size=1.5)

tm5 <- tm_shape(LondonWards) + 
  tm_polygons("PctOwned20",palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(e)", position=c(0,0.85), size=1.5)

t=tmap_arrange(tm1, tm2, tm3, tm4, tm5, ncol=2)

t

###Creating a Basic Geodemographic Classification

LondonWardsData <- LondonWards %>%
  #drop geometry
  st_drop_geometry()%>%
  #display list of variables
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

slice_head(LondonWardsData, n=5)

# Create a new data frame just containing the two variables we are interested in
mydata <- LondonWards %>%
  st_drop_geometry()%>%
  dplyr::select(c(PctOwned20, PctNoEngli))

#– check variable distributions first
histplot <- ggplot(data=mydata, aes(x=PctOwned20))
histplot +geom_histogram(binwidth=3)

histplot <- ggplot(data=mydata, aes(x= PctNoEngli))
histplot +geom_histogram(bins = 30)

#make our k-means
fit <- mydata %>%
  kmeans(., 3, nstart=25)

# get cluster means

centroid <- tidy(fit)%>%
  #print the results of the cluster groupings
  print()%>%
  dplyr::select(PctOwned20, PctNoEngli)

# as we only have variable two dimensions we can plot the clusters on a graph
p <- ggplot(mydata,aes(PctOwned20, PctNoEngli))+
  geom_point(aes(colour=factor(fit$cluster)))+
  geom_point(data=centroid,aes(PctOwned20, PctNoEngli), size=7, shape=18)+ theme(legend.position="none")
p

LondonWards <- fit %>% 
  # 
  augment(., LondonWards)%>%
  dplyr::select(WD11CD, .cluster)%>%
  #make sure the .cluster column is numeric
  mutate(across(.cluster, as.numeric))%>%
  # join the .cluster to our sf layer
  left_join(LondonWards, 
            .,
            by = c("WD11CD" = "WD11CD"))


#now map our geodeomographic classification
map <- ggplot(LondonWards) + 
  geom_sf(mapping = aes(fill=.cluster))+
  scale_fill_continuous(breaks=c(1,2,3))
map


