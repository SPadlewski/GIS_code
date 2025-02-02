#library a bunch of packages we may (or may not) use - install them first if not installed already. 
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)


#download a zip file containing some boundaries we want to use

download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", 
              destfile="prac9_data/statistical-gis-boundaries-london.zip")

listfiles<-dir_info(here::here("prac9_data")) %>%
  dplyr::filter(str_detect(path, ".zip")) %>%
  dplyr::select(path)%>%
  pull()%>%
  #print out the .gz file
  print()%>%
  as.character()%>%
  utils::unzip(exdir=here::here("prac9_data"))


#look what is inside the zip

Londonwards<-dir_info(here::here("prac9_data", 
                                 "statistical-gis-boundaries-london", 
                                 "ESRI"))%>%
  #$ means exact match
  dplyr::filter(str_detect(path, 
                           "London_Ward_CityMerged.shp$"))%>%
  dplyr::select(path)%>%
  pull()%>%
  #read in the file in
  st_read()

qtm(Londonwards)


#read in some attribute data
LondonWardProfiles <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                               col_names = TRUE, 
                               locale = locale(encoding = 'Latin1'))

#check all of the columns have been read in correctly
Datatypelist <- LondonWardProfiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist


#We can use readr to deal with the issues in this dataset - which are to do with text values being stored in columns containing numeric values

#read in some data - couple of things here. Read in specifying a load of likely 'n/a' values, also specify Latin1 as encoding as there is a pound sign (£) in one of the column headers - just to make things fun!

LondonWardProfiles <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                               na = c("", "NA", "n/a"), 
                               locale = locale(encoding = 'Latin1'),    
                               col_names = TRUE)
                               

#check all of the columns have been read in correctly
Datatypelist <- LondonWardProfiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist
                                                      

#merge boundaries and data
LonWardProfiles <- left_join(Londonwards,
                             LondonWardProfiles, 
                             by = c("GSS_CODE" = "New code"))

#let's map our dependent variable to see if the join has worked:
tmap_mode("view")
qtm(LonWardProfiles, 
    fill = "Average GCSE capped point scores - 2014", 
    borders = NULL,  
    fill.palette = "Blues")      



#might be a good idea to see where the secondary schools are in London too
london_schools <- read_csv("https://data.london.gov.uk/download/london-schools-atlas/57046151-39a0-45d9-8dc0-27ea7fd02de8/all_schools_xy_2016.csv")

#from the coordinate values stored in the x and y columns, which look like they are latitude and longitude values, create a new points dataset
lon_schools_sf <- st_as_sf(london_schools, 
                           coords = c("x","y"), 
                           crs = 4326)

lond_sec_schools_sf <- lon_schools_sf %>%
  filter(PHASE=="Secondary")

tmap_mode("view")
qtm(lond_sec_schools_sf)

####Regression Basics

q <- LonWardProfiles %>% 
  qplot(x = `Unauthorised Absence in All Schools (%) - 2013`, 
           y = `Average GCSE capped point scores - 2014`, 
           data=.)

#plot with a regression line - note, I've added some jitter here as the x-scale is rounded
q + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()


#run the linear regression model and store its outputs in an object called model1
Regressiondata<- LonWardProfiles%>%
  clean_names()%>%
  dplyr::select(average_gcse_capped_point_scores_2014, 
                unauthorised_absence_in_all_schools_percent_2013)

#now model
model1 <- lm(average_gcse_capped_point_scores_2014 ~
               unauthorised_absence_in_all_schools_percent_2013, 
             data = Regressiondata)
                               
#show the summary of those outputs
summary(model1)


library(broom)
tidy(model1)
glance(model1)
#install.packages('tidypredict')
library(tidypredict)
Regressiondata %>%
  tidypredict_to_column(model1)


##Bootstrap resampling
Bootstrapdata<- LonWardProfiles%>%
  clean_names()%>%
  dplyr::select(average_gcse_capped_point_scores_2014, 
                unauthorised_absence_in_all_schools_percent_2013)

library(rsample)
set.seed(42)

GCSE_boot <-st_drop_geometry(Bootstrapdata) %>%
  bootstraps(times = 200, apparent = TRUE)


slice_tail(GCSE_boot, n=5)

GCSE_models <- GCSE_boot %>%
  #make new column
  mutate(
    #column name is model that contains...
    model = map(splits, ~ lm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013, 
                             data = .)))

# let's look at the first model results
GCSE_models$model[[1]]


GCSE_models_tidy <- GCSE_models %>%
  mutate(
    coef_info = map(model, tidy))


GCSE_coef <- GCSE_models_tidy %>%
  unnest(coef_info)
GCSE_coef

coef <- GCSE_coef %>% 
  filter(term == "unauthorised_absence_in_all_schools_percent_2013")
coef


coef %>%
  ggplot(aes(x=estimate)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="lightblue2", col="lightblue3")+
  geom_vline(aes(xintercept=mean(estimate)),
             color="blue",
             linetype="dashed")+
  labs(title="Bootstrap resample estimates",
       x="Coefficient estimates",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


library(rsample)

int_pctl(GCSE_models_tidy, coef_info, alpha = 0.05)


GCSE_aug <- GCSE_models_tidy %>%
  #sample_n(5) %>%
  mutate(augmented = map(model, augment))%>%
  unnest(augmented)


length(LonWardProfiles$`Average GCSE capped point scores - 2014`)


firstboot<-filter(GCSE_aug,id=="Bootstrap0001")

firstbootlength <- firstboot %>%
  dplyr::select(average_gcse_capped_point_scores_2014)%>%
  pull()%>%
  length()

#nrow(firstboot)

firstboot$coef_info

uniquecoefficent <- firstboot %>%
  #select(average_gcse_capped_point_scores_2014) %>%
  dplyr::select(coef_info)%>%
  unnest(coef_info)%>%
  distinct()

uniquecoefficent


#plotting
ggplot(GCSE_aug, aes(unauthorised_absence_in_all_schools_percent_2013,
                     average_gcse_capped_point_scores_2014))+
  # we want our lines to be from the fitted column grouped by our bootstrap id
  geom_line(aes(y = .fitted, group = id), alpha = .2, col = "cyan3") +  
  # remember out apparent data is the original within the bootstrap
  geom_point(data=filter(GCSE_aug,id=="Apparent"))+
  #add some labels to x and y
  labs(x="unauthorised absence in all schools 2013 (%)",
       y="Average GCSE capped point scores 2014")


#save the file if you need to
#ggsave("insert_filepath and name")




######Assumptions Underpinning Linear Regression


### Assumption 1 
### There is a linear relationship between 
### the dependent and independent variables

# use Janitor to clean up the names.

LonWardProfiles <- LonWardProfiles %>%
  clean_names()

#let's check the distribution of these variables first

ggplot(LonWardProfiles, aes(x=average_gcse_capped_point_scores_2014)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)


ggplot(LonWardProfiles, aes(x=unauthorised_absence_in_all_schools_percent_2013)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) + 
  geom_density(colour="red",
               size=1, 
               adjust=1)


ggplot(LonWardProfiles, aes(x=median_house_price_2014)) + 
  geom_histogram()


qplot(x = median_house_price_2014, 
      y = average_gcse_capped_point_scores_2014, 
      data=LonWardProfiles)


ggplot(LonWardProfiles, aes(x=log(median_house_price_2014))) + 
  geom_histogram()


symbox(~median_house_price_2014, 
       LonWardProfiles, 
       na.rm=T,
       powers=seq(-3,3,by=.5))


ggplot(LonWardProfiles, aes(x=(median_house_price_2014)^-1)) + 
  geom_histogram()


qplot(x = (median_house_price_2014)^-1, 
      y = average_gcse_capped_point_scores_2014,
      data=LonWardProfiles)

qplot(x = log(median_house_price_2014), 
      y = average_gcse_capped_point_scores_2014, 
      data=LonWardProfiles)


## Assumption 2 
## The residuals in your model should be normally distributed


#save the residuals into your dataframe

model_data <- model1 %>%
  augment(., Regressiondata)

#plot residuals
model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram()


## Assumption 3 - No Multicolinearity in the independent variables

Regressiondata2<- LonWardProfiles%>%
  clean_names()%>%
  dplyr::select(average_gcse_capped_point_scores_2014,
                unauthorised_absence_in_all_schools_percent_2013,
                median_house_price_2014)

model2 <- lm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
               log(median_house_price_2014), data = Regressiondata2)

#show the summary of those outputs
tidy(model2)

glance(model2)


#and for future use, write the residuals out
model_data2 <- model2 %>%
  augment(., Regressiondata2)

# also add them to the shapelayer
LonWardProfiles <- LonWardProfiles %>%
  mutate(model2resids = residuals(model2))

install.packages('corrr')
library(corrr)

Correlation <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(average_gcse_capped_point_scores_2014,
                unauthorised_absence_in_all_schools_percent_2013,
                median_house_price_2014) %>%
  mutate(median_house_price_2014 =log(median_house_price_2014))%>%
  correlate() %>%
  # just focus on GCSE and house prices
  focus(-average_gcse_capped_point_scores_2014, mirror = TRUE) 


#visualise the correlation matrix
rplot(Correlation)

vif(model2)

position <- c(10:74)

Correlation_all<- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(position)%>%
  sapply(.,as.numeric)%>% 
  correlate(method ="pearson")

Correlation_all_class<-Correlation_all %>% summarise_all(class) %>% 
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class") %>% 
  dplyr::filter(Variable_class=='character') %>% 

rplot(Correlation_all)



##Assumption 4 - Homoscedasticity


#print some model diagnositcs. 
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model2)

##Assumption 5 - Independence of Errors

#run durbin-watson test
DW <- durbinWatsonTest(model2)
tidy(DW)


#now plot the residuals
tmap_mode("view")
#qtm(LonWardProfiles, fill = "model1_resids")

tm_shape(LonWardProfiles) +
  tm_polygons("model2resids",
              palette = "RdYlBu") +
  tm_shape(lond_sec_schools_sf) + tm_dots(col = "TYPE")


#calculate the centroids of all Wards in London
coordsW <- LonWardProfiles%>%
  st_centroid()%>%
  st_geometry()

par(mfrow=c(1,1))
plot(coordsW)


# Now we need to generate a spatial weights matrix (remember from the lecture a couple of weeks ago). 
# We'll start with a simple binary matrix of queen's case neighbours

LWard_nb <- LonWardProfiles %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")

plot(LWard_knn, st_geometry(coordsW), col="blue")

#add a map underneath
plot(LonWardProfiles)

#create a spatial weights matrix object from these weights

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="C")

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")

#Now run a moran’s I test on the residuals, first using queens neighbours
Queen <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()



#Then nearest k-nearest neighbours
Nearest_neighbour <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

Queen

Nearest_neighbour

### Spatial Regression Models

#
#Original Model
model2 <- lm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
               log(median_house_price_2014), data = LonWardProfiles)

tidy(model2)

#Now run a spatially-lagged regression model with a queen’s case weights matrix
library(spatialreg)

slag_dv_model2_queen <- lagsarlm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
                                   log(median_house_price_2014), 
                                 data = LonWardProfiles, 
                                 nb2listw(LWard_nb, style="C"), 
                                 method = "eigen")

#what do the outputs show?
tidy(slag_dv_model2_queen)

#glance() gives model stats but this need something produced from a linear model
#here we have used lagsarlm()
glance(slag_dv_model2_queen)
glance(model2)

t<-summary(slag_dv_model2_queen)


#run a spatially-lagged regression model
slag_dv_model2_knn4 <- lagsarlm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
                                  log(median_house_price_2014), 
                                data = LonWardProfiles, 
                                nb2listw(LWard_knn, 
                                         style="C"), 
                                method = "eigen")

#what do the outputs show?
tidy(slag_dv_model2_knn4)


#write out the residuals

LonWardProfiles <- LonWardProfiles %>%
  mutate(slag_dv_model2_knn_resids = residuals(slag_dv_model2_knn4))

KNN4Moran <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(slag_dv_model2_knn_resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

KNN4Moran

## The Spatial Error Model

#We can run a spatial error model on the same data below:
  
sem_model1 <- errorsarlm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
                             log(median_house_price_2014), 
                           data = LonWardProfiles,
                           nb2listw(LWard_knn, style="C"), 
                           method = "eigen")

tidy(sem_model1)


###Spatial regression model in R

extradata <- read_csv("https://www.dropbox.com/s/qay9q1jwpffxcqj/LondonAdditionalDataFixed.csv?raw=1")

#add the extra data too
LonWardProfiles <- LonWardProfiles%>%
  left_join(., 
            extradata, 
            by = c("gss_code" = "Wardcode"))%>%
  clean_names()

#print some of the column names
LonWardProfiles%>%
  names()%>%
  tail(., n=10)

#plotting 
p <- ggplot(LonWardProfiles, 
            aes(x=unauth_absence_schools11, 
                y=average_gcse_capped_point_scores_2014))
p + geom_point(aes(colour = inner_outer))


#first, let's make sure R is reading our InnerOuter variable as a factor
#see what it is at the moment...
isitfactor <- LonWardProfiles %>%
  dplyr::select(inner_outer)%>%
  summarise_all(class)

isitfactor


# change to factor

LonWardProfiles<- LonWardProfiles %>%
  mutate(inner_outer=as.factor(inner_outer))

#now run the model
model3 <- lm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
               log(median_house_price_2014) + 
               inner_outer, 
             data = LonWardProfiles)

tidy(model3)

contrasts(LonWardProfiles$inner_outer)

# If we want to change the reference group, there are various ways of doing this. 
# We can use the contrasts() function, or we can use the relevel() function. Let’s try it:

LonWardProfiles <- LonWardProfiles %>%
  mutate(inner_outer = relevel(inner_outer, 
                               ref="Outer"))

model3 <- lm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
               log(median_house_price_2014) + 
               inner_outer, 
             data = LonWardProfiles)

tidy(model3)

####  Task 3 - Spatial Non-stationarity and Geographically Weighted Regression Models (GWR)

#select some variables from the data file
myvars <- LonWardProfiles %>%
  dplyr::select(average_gcse_capped_point_scores_2014,
                unauthorised_absence_in_all_schools_percent_2013,
                median_house_price_2014,
                rate_of_job_seekers_allowance_jsa_claimants_2015,
                percent_with_level_4_qualifications_and_above_2011,
                inner_outer)

#check their correlations are OK
Correlation_myvars <- myvars %>%
  st_drop_geometry()%>%
  dplyr::select(-inner_outer)%>%
  correlate()

#run a final OLS model
model_final <- lm(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
                    log(median_house_price_2014) + 
                    inner_outer + 
                    rate_of_job_seekers_allowance_jsa_claimants_2015 +
                    percent_with_level_4_qualifications_and_above_2011, 
                  data = myvars)

tidy(model_final)


LonWardProfiles <- LonWardProfiles %>%
  mutate(model_final_res = residuals(model_final))

par(mfrow=c(2,2))
plot(model_final)

qtm(LonWardProfiles, fill = "model_final_res")


final_model_Moran <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model_final_res)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

final_model_Moran


library(spgwr)

st_crs(LonWardProfiles) = 27700

LonWardProfilesSP <- LonWardProfiles %>%
  as(., "Spatial")

st_crs(coordsW) = 27700

coordsWSP <- coordsW %>%
  as(., "Spatial")

coordsWSP


#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
                          log(median_house_price_2014) + 
                          inner_outer + 
                          rate_of_job_seekers_allowance_jsa_claimants_2015 +
                          percent_with_level_4_qualifications_and_above_2011, 
                        data = LonWardProfilesSP, 
                        coords=coordsWSP,
                        adapt=T)


#run the gwr model
gwr.model = gwr(average_gcse_capped_point_scores_2014 ~ unauthorised_absence_in_all_schools_percent_2013 + 
                  log(median_house_price_2014) + 
                  inner_outer + 
                  rate_of_job_seekers_allowance_jsa_claimants_2015 +
                  percent_with_level_4_qualifications_and_above_2011, 
                data = LonWardProfilesSP, 
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)


#print the results of the model
gwr.model


results <- as.data.frame(gwr.model$SDF)
names(results)


#attach coefficients to original SF


LonWardProfiles2 <- LonWardProfiles %>%
  mutate(coefUnauthAbs = results$unauthorised_absence_in_all_schools_percent_2013,
         coefHousePrice = results$log.median_house_price_2014.,
         coefJSA = rate_of_job_seekers_allowance_jsa_claimants_2015,
         coefLev4Qual = percent_with_level_4_qualifications_and_above_2011)

tm_shape(LonWardProfiles2) +
  tm_polygons(col = "coefHousePrice", 
              palette = "RdBu", 
              alpha = 0.5)

#run the significance test
sigTest = abs(gwr.model$SDF$"log.median_house_price_2014.")-2 * gwr.model$SDF$"log.median_house_price_2014._se"


#store significance results
LonWardProfiles2 <- LonWardProfiles2 %>%
  mutate(GWRUnauthSig = sigTest)

tm_shape(LonWardProfiles2) +
  tm_polygons(col = "GWRUnauthSig", 
              palette = "RdYlBu")