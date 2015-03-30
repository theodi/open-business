#Set up address file from master first

setwd("~/git/open-business")
theme_set(theme_minimal(base_family = "Helvetica Neue", base_size = 18))
source('~/git/ODI colour Scheme.R')
library(ggplot2)
library(ggmap)

options(stringsAsFactors = FALSE)


#==========================================================
#Get the map
#==========================================================

#Google Maps version - currently working
#Google specific - bounding box for UK map 
UKbox.g <- c(-8.6, 49, 1.7, 60) #google does not interpret bboxes very well so tweaked version

UKmap.g <- get_map(UKbox.g,
                 zoom = 6,
                 maptype = c("terrain"),
                 messaging = FALSE, 
                 crop = FALSE,
                 color = c("bw"),
                 source = c("google"))

#Plot the map to variable UKMap
UKMap.g <- ggmap(UKmap.g, extent = 'device')
print(UKMap.g)


# Get a map of London
Lonmap.g <- get_map("London, UK",
                   zoom = 12,
                   maptype = c("terrain"),
                   messaging = FALSE, 
                   crop = FALSE,
                   color = c("bw"),
                   source = c("google"))

#Plot the map to variable UKMap
LonMap.g <- ggmap(Lonmap.g, extent = 'device')
print(LonMap.g)

#---------------------------------------------
#bounding box for other maps
# To generate coordinates on a map use osm export function and copy values
UKbox <- c(-8.613, 49.866, 1.934, 58.995)

#Stamen 
# Get a UK map
UKmap.s <- get_map(UKbox,
                 zoom = 7,
                 maptype = c("toner"),
                 crop = FALSE,
                 color = c("color"),
                 source = c("stamen"),
                )

#Plot the map to variable UKMap
UKMap.s <- ggmap(UKmap.s, extent = 'device', legend = 'topleft')
print(UKMap.s)

# Get a London Map

Lonmap.s <- get_map("London, UK",
                   zoom = 12,
                   maptype = c("toner"),
                   crop = FALSE,
                   color = c("color"),
                   source = c("stamen"),
                   )

LonMap.s <- ggmap(Lonmap.s, extent = 'device', legend = 'topleft')
print(LonMap.s)


#---------------------------------------------

#OSM - NOT WORKING
#UK Map - not working
UKmap.o <- get_map(UKbox,
                 scale = "auto",
                 crop = FALSE,
                 color = c("color"),
                 source = c("osm"),
                 )
   
#Plot the map to variable UKMap
UKMap.o <- ggmap(UKmap.o, extent = 'device', legend = 'topleft')
print(UKMap.o)

# Get a London Map - Not working

Lonmap.o <- get_map("London, UK",
                    zoom = 12,
                    maptype = c("toner"),
                    crop = FALSE,
                    color = c("color"),
                    source = c("osm"),
                    )

LonMap.o <- ggmap(Lonmap.o, extent = 'device', legend = 'topleft')
print(LonMap.o)

#==========================================================
# Setup data points to map
#==========================================================

# Read the exported csv
postcodes.full <- read.csv("master-postcodes.csv")

# Clean several addresses - remove "herts" from one postcode, correct 2 others - *correct this in master when time*
postcodes.full$x <- gsub("Herts ", "", postcodes.full$x)
postcodes.full$x <- gsub("EC24A 4JE", "EC2A 4JE", postcodes.full$x)

# Remove NAs - as NA plots at lon=18.4904100  lat=-22.95764 (assume North America or state but doesnt plot on our map anyway)
postcodes <- postcodes.full$x[!is.na(postcodes.full$x)]

#Convert postcodes to lon/lat of places
place <- geocode(postcodes)

#write csv to avoid having to rerun Google API calls!
write.csv(place, "lon-lats.csv", , row.names = FALSE)


#TESTS - done with one value then while offline
#aplace <- geocode("EC2A 4JE")
#print(aplace)
#print(as.numeric(aplace))
#Test done with small matrix while offline
#test.lon <- c(-0.0836629, -0.5, 0, 0.1, -1)
#test.lat <- c(51.52239, 52, 50.5, 51, 53)
#aplace.test <- data.frame(test.lon, test.lat)

#---------------------------------------------
#If using the 260 postcodes already identified

place <- read.csv("lon-lats.csv")


#==========================================================
# Add points to map
#==========================================================

#Plot of points to UKmap
# On gmap
UKMap.g+ 
  geom_point(aes(x = lon, y = lat), data = place,
             alpha= 1, color = odi_orange)
ggsave("graphics/UKMap-google.png", height = 6, width = 12)

# On stamen
UKMap.s+ 
  geom_point(aes(x = lon, y = lat), data = place,
             alpha= 1, color = odi_orange)
ggsave("graphics/UKMap-Stamen-toner.png", height = 6, width = 12)
#---------------------------------------------
#Plots of points on London map

# On gmap
LonMap.g+ 
  geom_point(aes(x = lon, y = lat), data = place,
             alpha= 1, color = odi_orange)
ggsave("graphics/LonMap-google.png", height = 6, width = 12)

# On stamen
LonMap.s+ 
  geom_point(aes(x = lon, y = lat), data = place,
             alpha= 1, color = odi_orange)
ggsave("graphics/LonMap-Stamen-toner.png", height = 6, width = 12)
#---------------------------------------------
# PLot of density - heatmap - this is not working - YET!!




