library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)

# read in data on informal settlements in Cape Town
# source: https://open.africa/dataset/city-of-cape-town-gis-data
ct_inf <- st_read("spatial-data/data/informal.shp") 

# read in data with quarter degree grid cells
ct_qdgc <- st_read("spatial-data/data/pent_sa_mini.shp")

# extract center of each informal settlement polygon
ct_inf_center <- st_centroid(ct_inf)

# convert to lat-long coordinates 
# (the st_zm() is not usually needed, here fixes a weird error)
ct_inf <- ct_inf %>% st_transform(crs = 4326) %>% st_zm()
ct_inf_center <- ct_inf_center %>% st_transform(crs = 4326) 

# plot a "base map" only
leaflet() %>% 
  addProviderTiles("Esri.WorldStreetMap") %>%
  setView(lng = 18.4241, lat = -33.9249, zoom = 10) 

# plot the informal settlement polygons
leaflet() %>% 
  addProviderTiles("Esri.WorldStreetMap") %>%
  setView(lng = 18.4241, lat = -33.9249, zoom = 10) %>%
  addPolygons(data = ct_inf) 

# plot the informal settlement polygons shaded by Traveltime
pal <- colorNumeric("Blues", domain = ct_inf$Traveltime)
leaflet() %>% 
  addProviderTiles("Esri.WorldStreetMap") %>%
  setView(lng = 18.4241, lat = -33.9249, zoom = 10) %>%
  addPolygons(data = ct_inf, color = ~pal(Traveltime), fillOpacity = 1) %>%
  addLegend(data = ct_inf, pal = pal, values = ~Traveltime) 

# EXERCISE: shade the polygons by the number of households (HHs), or the predominant age (Predom_Age)
# plot the informal settlement polygons shaded by Age 
pal <- colorNumeric("Spectral", domain = ct_inf$HHs) #Why ?? sppectral is for Qualitative: for qualitative unordered data
leaflet() %>% 
  addProviderTiles("Esri.WorldStreetMap") %>%
  setView(lng = 18.4241, lat = -33.9249, zoom = 10) %>%
  addPolygons(data = ct_inf, color = ~pal(HHs), fillOpacity = 1) %>%
  addLegend(data = ct_inf, pal = pal, values = ~HHs) 
#QUESTION
# add markers for each settlement - Does this not work because of NAs or because its not continous 
pal <- colorNumeric("Spectral", domain = ct_inf$Predom_Age)
leaflet() %>% 
  addProviderTiles("Esri.WorldStreetMap") %>%
  setView(lng = 18.4241, lat = -33.9249, zoom = 10) %>%
  addPolygons(data = ct_inf, color = ~pal(Predom_Age), fillOpacity = 1) %>%
  addLegend(data = ct_inf, pal = pal, values = ~Predom_Age)


# use the "popup" and "label" arguments to addMarkers so that the settlement name
# is displayed on a mouseclick (InformalSe) and the travel time is shown on mouse over (Traveltime)

pal <- colorNumeric("Blues", domain = ct_inf$Traveltime)
leaflet() %>% 
  addProviderTiles("Esri.WorldStreetMap") %>%
  setView(lng = 18.4241, lat = -33.9249, zoom = 10) %>%
  addPolygons(data = ct_inf, color = ~pal(Traveltime), fillOpacity = 1) %>%
  addLegend(data = ct_inf, pal = pal, values = ~Traveltime) %>%
  addMarkers(data = ct_inf_center, popup = ~InformalSe, label = ~as.character(Traveltime))

ct_qdgc <- ct_qdgc %>% st_transform(crs = 4326) %>% st_zm()
#Question: It's saying its not finding the polygon data
# EXERCISE: overlay the quarter-degree grid cells in the ct_qdgc dataset
pal <- colorNumeric("Blues", domain = ct_inf$Traveltime)
leaflet() %>% 
  addProviderTiles("Esri.WorldStreetMap") %>%
  setView(lng = 18.4241, lat = -33.9249, zoom = 10) %>%
  addPolygons(data = ct_inf, color = ~pal(Traveltime), fillOpacity = 1) %>%
  addLegend(data = ct_inf, pal = pal, values = ~Traveltime) %>%
  addMarkers(data = ct_inf_center, popup = ~InformalSe, label = ~as.character(Traveltime)) +addPolygons(ct_qdgc$geometry)

# EXERCISE: count how many informal settlements are in each grid cell, and 
# shade each grid cell by the number of informal settlements it contains

# EXERCISE (unrelated to the above): work through the application in Ch 13 of 
# Geocomputation with R (https://geocompr.robinlovelace.net/location.html)

library(sf)
library(dplyr)
library(purrr)
library(raster)
library(osmdata)
library(spDataLarge)
data("census_de", package = "spDataLarge")

# pop = population, hh_size = household size
#converting from german to english
input = dplyr::select(census_de, x = x_mp_1km, y = y_mp_1km, pop = Einwohner,
                      women = Frauen_A, mean_age = Alter_D,
                      hh_size = HHGroesse_D)
# set -1 and -9 to NA
input_tidy = mutate_all(input, list(~ifelse(. %in% c(-1, -9), NA, .)))
input_ras = rasterFromXYZ(input_tidy, crs = st_crs(3035)$proj4string)
#After the preprocessing, the data can be converted into a raster stack or brick
#It requires an input data frame where the first two columns represent coordinates on a regular grid. 
#All the remaining columns (here: pop, women, mean_age, hh_size) will serve as input for the raster brick layers.

#Note that we are using an equal-area projection (EPSG:3035; Lambert Equal Area Europe),
# i.e., a projected CRS where each grid cell has the same area, here 1000 x 1000 square meters.
# Since we are using mainly densities such as the number of inhabitants or the portion of women
# per grid cell, it is of utmost importance that the area of each grid cell is the same to avoid
# ‘comparing apples and oranges’. 

#Next step is to convert the data, for pop data we have to convert the classes into a numeric data type using class means.
#This measn that raster cells that have a pop between 3-250 have a mean of 176 etc. This was found in the meta data for the csv
#Right now its a catagorical varibale with level 1 corresponding to a cell having betweem 3-250 people etc. 

#Why are there 3 coloumns 
rcl_pop = matrix(c(1, 1, 127, 2, 2, 375, 3, 3, 1250, 
                   4, 4, 3000, 5, 5, 6000, 6, 6, 8000), 
                 ncol = 3, byrow = TRUE)
rcl_women = matrix(c(1, 1, 3, 2, 2, 2, 3, 3, 1, 4, 5, 0), 
                   ncol = 3, byrow = TRUE)
rcl_age = matrix(c(1, 1, 3, 2, 2, 0, 3, 5, 0),
                 ncol = 3, byrow = TRUE) #These are weighted by importance 3 being favorable-0 unfavorable 
rcl_hh = rcl_women
rcl = list(rcl_pop, rcl_women, rcl_age, rcl_hh)

reclass = input_ras
for (i in seq_len(nlayers(reclass))) {
  reclass[[i]] = reclassify(x = reclass[[i]], rcl = rcl[[i]], right = NA)
}
names(reclass) = names(input_ras)
#We define metropolitan areas as pixels of 20 km2 inhabited by more than 500,000 people.
#Pixels at this coarse resolution can rapidly be created using aggregate()
#The command below uses the argument fact = 20 to reduce the resolution of the result twenty-fold (recall the original raster resolution was 1 km2):
# pop_agg = aggregate(reclass$pop, fact = 20, fun = sum)
# summary(pop_agg)
# pop_agg = pop_agg[pop_agg > 500000, drop = FALSE]

#Exersises________________________________________________________________________________________
#1 We have used raster::rasterFromXYZ() to convert a input_tidy into a raster brick. 
#Try to achieve the same with the help of the sp::gridded() function.
#Download the csv file containing inhabitant information for a 100-m cell resolution
data<-read_csv(file="data/townsGerman") #did not work 

