
#Plot addresses from Master - 
#write.csv(table(list[, "Postcode"]), "master-postcodes.csv", , row.names = FALSE)
write.csv(list[, "Postcode"], "master-postcodes.csv", , row.names = FALSE)


setwd("~/git/open-business")
theme_set(theme_minimal(base_family = "Helvetica Neue", base_size = 18))
source('/Users/Jamie/Documents/R STUDIO/Open Business/ODI colour Scheme.R')
library(ggplot2)
library(ggmap)

options(stringsAsFactors = FALSE)


#---------------------------------------------
#Get map

#bounding box but google does not interpret this very well - others might do better
# Might be best: (-8.459, 49.909, 2.175, 59.623)
# To generate coordinates on a map use osm export function and copy values
UKbox.g <- c(-8.6, 49, 1.7, 60)
UKbox <- c(-8.459, 49.909, 2.175, 59.623)

#This uses google maps but is alright for now - 
#explore other options - play with other maps and zooms and bounding box coords
UKmap <- get_map(UKbox,
                 zoom = 6,
                 maptype = c("terrain"),
                 messaging = FALSE, 
                 crop = FALSE,
                 color = c("color"),
                 source = c("google"))

#Plot the map to variable UKMap
UKMap <- ggmap(UKmap, extent = 'device', legend = 'topleft')

print(UKMap)
#OSM - currently not working properly! maybe doesn't want to export such a large map - can't work out correct scale
UKmap <- get_map(UKbox,
                 zoom = 6,
                 maptype = c("toner"),
                 filename = "ggmapTemp", crop = FALSE,
                 color = c("bw"),
                 source = c("stamen"),
                )


       

#---------------------------------------------
# Setup data points to map

# Read the exported csv
postcodes <- read.csv("master-postcodes.csv")

# Clean several addresses - remove "herts" from one postcode, correct 2 others - *correct this in master when time*
postcodes$x <- gsub("Herts ", "", postcodes$x)
postcodes$x <- gsub("EC24A 4JE", "EC2A 4JE", postcodes$x)

#Convert postcodes to lon/lat of places
#NA plots at lon=18.4904100  lat=-22.95764 (assume North America or state but doesnt plot on our map anyway)
place <- geocode(postcodes$x)


#failed method
#post.frame <- read.csv("master-postcodes.csv")
#name column 'postcode'
#names(post.frame)[names(post.frame)=="x"] <- "postcode"
#post.frame$postcode <- gsub("Herts ", "", post.frame$postcode)
#post.frame$postcode <- gsub("EC24A 4JE", "EC2A 4JE", post.frame$postcode)


#TESTS - done with one value then while offline
#aplace <- geocode("EC2A 4JE")
#print(aplace)
#print(as.numeric(aplace))
#test.lon <- c(-0.0836629, -0.5, 0, 0.1, -1)
#test.lat <- c(51.52239, 52, 50.5, 51, 53)
#aplace.test <- data.frame(test.lon, test.lat)


#---------------------------------------------
# Add points to map

#Plot of points 
 UKMap+ 
  geom_point(aes( x = lon, y = lat), data = place,
            alpha= 1, color = "orange", na.rm = TRUE)


# PLot of density - heatmap - this is not working - YET!!

UKMap + stat_density2d(aes(x = lon, y = lat, 
                     fill = ..level.. , alpha = ..level..),size = 2, bins = 4, 
                 data = place, geom = 'polygon', color = 'orange') 
      + scale_fill_gradient('aplace.test') 
      + scale_alpha(range = c(.4, .75), guide = FALSE) 
      + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))




