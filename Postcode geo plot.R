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
#bounding box for stamen
# To generate coordinates on a map use osm export function and copy values
UKbox <- c(-8.9, 50.7, 1.9, 58.6) #(left, bottom, right, top)

#c(-8.613, 49.866, 1.934, 58.995)

#Stamen 
# Get a UK map
UKmap.s <- get_map(UKbox,
                   zoom = 8,
                   maptype = c("toner"),
                   crop = FALSE,
                   color = c("color"),
                   source = c("stamen"),
)

#Plot the map to variable UKMap
UKMap.s <- ggmap(UKmap.s, extent = 'device', legend = 'topleft')
print(UKMap.s)


# Get a London Map
#bounding box based
#box
Lonbox <- c(-0.247244,51.458139,-0.00264,51.556176) #(left, bottom, right, top)


#c(-0.35, 51.35, 0.10, 51.64)

Lonmapb.s <- get_map(Lonbox,
                     zoom = 12,
                     maptype = c("toner"),
                     crop = FALSE,
                     color = c("color"),
                     source = c("stamen"),
)

LonMapb.s <- ggmap(Lonmapb.s, extent = 'device', legend = 'topleft')
print(LonMapb.s)

#search based
#Lonmaps.s <- get_map("London, UK",
#                   zoom = 13,
#                   maptype = c("toner"),
#                   crop = FALSE,
#                   color = c("color"),
#                   source = c("stamen"),
#                   )

#LonMaps.s <- ggmap(Lonmaps.s, extent = 'device', legend = 'topleft')
#print(LonMaps.s)


#---------------------------------------------
#OTHER MAPS - IGNORE FOR NOW
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
postcodes <- read.csv("data/master-postcodes.csv")

#Convert postcodes to lon/lat of places
place <- geocode(postcodes$x)

place$postcode <- unlist(postcodes)

#write csv to avoid having to rerun Google API calls!
write.csv(place, "data/lon-lats.csv", , row.names = FALSE)



#Read extra postcodes
postcodes.ex <- read.csv("data/extra-postcodes.csv")

#Convert these to lon/lat
ex.place <- geocode(postcodes.ex$x)

ex.place$postcode <- unlist(postcodes.ex)

#Bind these onto the original lon/lats
place.full <- rbind(place, ex.place)

#write csv to avoid having to rerun Google API calls!
write.csv(place.full, "data/lon-lats-all.csv", , row.names = FALSE)



#TESTS - done with one value then while offline
#aplace <- geocode("EC2A 4JE")
#print(aplace)
#print(as.numeric(aplace))
#Test done with small matrix while offline
#test.lon <- c(-0.0836629, -0.5, 0, 0.1, -1)
#test.lat <- c(51.52239, 52, 50.5, 51, 53)
#aplace.test <- data.frame(test.lon, test.lat)

#---------------------------------------------
#If using the postcodes already identified

place <- read.csv("data/lon-lats.csv")
place.full <- read.csv("data/lon-lats-all.csv")


#==========================================================
# Add points to map
#==========================================================
#Just trading address

#Plot of points to UKmap
# On stamen
UKMap.s+ 
  geom_point(aes(x = lon, y = lat), data = place,
             alpha= 1, color = odi_orange, size = 3)

#set image = 
#PNG
ggsave(file = "graphics/Master/UKMap-Stamen-toner.png", plot=image, height = 12, width = 10)
#EPS
ggsave(file = "graphics/Master/UKMap-Stamen-toner.eps", plot=image, height = 12, width = 10)
#SVG
ggsave(file="graphics/Master/UKMap-Stamen-toner.svg", plot=image, width=12, height=10)

#---------------------------------------------
#Plots of points on London map
# On stamen
LonMapb.s+ 
  geom_point(aes(x = lon, y = lat), data = place,
             alpha= 1, color = odi_orange, size = 4)

#LonMaps.s+ 
#  geom_point(aes(x = lon, y = lat), data = place,
#             alpha= 1, color = odi_orange)

#set image =  
#PNG
ggsave(file = "graphics/Master/LonMap-Stamen-toner.png", plot=image, height = 8, width = 12)
#EPS
ggsave(file = "graphics/Master/LonMap-Stamen-toner.eps", plot=image, height = 8, width = 12)
#SVG
ggsave(file="graphics/Master/LonMap-Stamen-toner.svg", plot=image, height = 8, width = 12)

#---------------------------------------------
#GMAP
#UK
# On gmap
#UKMap.g+ 
#  geom_point(aes(x = lon, y = lat), data = place,
#             alpha= 1, color = odi_orange)
#ggsave("graphics/UKMap-google.png", height = 6, width = 12)

#London
# On gmap
#LonMap.g+ 
#  geom_point(aes(x = lon, y = lat), data = place,
#             alpha= 1, color = odi_orange)
# ggsave("graphics/LonMap-google.png", height = 6, width = 12)

#---------------------------------------------
#---------------------------------------------
#All postcodes - with registered as well
#Plot of points to UKmap
# On stamen
UKMap.s+ 
  geom_point(aes(x = lon, y = lat), data = place.full,
             alpha= 1, color = odi_orange, size = 3)

#set image = 
#PNG
ggsave(file = "graphics/Master/UKMap-all-post.png", plot=image, height = 12, width = 10)
#EPS
ggsave(file = "graphics/Master/UKMap-all-post.eps", plot=image, height = 12, width = 10)
#SVG
ggsave(file="graphics/Master/UKMap-all-post.svg", plot=image, width=12, height=10)

# On gmap
#UKMap.g+ 
#  geom_point(aes(x = lon, y = lat), data = place,
#             alpha= 1, color = odi_orange)
#ggsave("graphics/UKMap-google.png", height = 6, width = 12)
#---------------------------------------------
#Plots of points on London map
# On stamen
image = LonMapb.s+ 
  geom_point(aes(x = lon, y = lat), data = place.full,
             alpha= 1, color = odi_orange, size = 4)

#LonMaps.s+ 
#  geom_point(aes(x = lon, y = lat), data = place,
#             alpha= 1, color = odi_orange)

#set image =  
#PNG
ggsave(file = "graphics/Master/LonMap-all-post.png", plot=image, height = 8, width = 12)
#EPS
ggsave(file = "graphics/Master/LonMap-all-post.eps", plot=image, height = 8, width = 12)
#SVG
ggsave(file="graphics/Master/LonMap-all-post.svg", plot=image, height = 8, width = 12)


# On gmap
#LonMap.g+ 
#  geom_point(aes(x = lon, y = lat), data = place,
#             alpha= 1, color = odi_orange)
# ggsave("graphics/LonMap-google.png", height = 6, width = 12)









#---------------------------------------------
#---------------------------------------------
#---------------------------------------------
# PLot of density - heatmap - this is not working - YET!!

#---------------------------------------------
#No longer necessary
# Clean several addresses - remove "herts" from one postcode, correct 2 others - *correct this in master when time*
#postcodes.full$x <- gsub("Herts ", "", postcodes.full$x)
#postcodes.full$x <- gsub("EC24A 4JE", "EC2A 4JE", postcodes.full$x)


# Remove NAs - as NA plots at lon=18.4904100  lat=-22.95764 (assume North America or state but doesnt plot on our map anyway)
#postcodes <- postcodes.full$x[!is.na(postcodes.full$x)]


