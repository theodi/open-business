
#Plot addresses from Master - 

write.csv(table(list[, "Postcode"]), "master-postcodes.csv", , row.names = FALSE)


theme_set(theme_minimal(base_family = "Helvetica Neue", base_size = 18))
source('/Users/Jamie/Documents/R STUDIO/Open Business/ODI colour Scheme.R')

options(stringsAsFactors = FALSE)

postcodes <- read.csv("master-postcodes.csv")


# UK long:-3.435973,  Lat:55.37805 or -4, 54
UK <- "United Kingdom"

#OSM doesn't want to export such a large map - can't work out correct scale
#get_openstreetmap(bbox = c(left = -8.459, bottom = 49.909, right = 2.175, top = 59.623),
                 # scale = ,
                 # format = c("png"),
                 # filename = "UKmap", color = c("color"))


#closest google map to what I can find
get_map(location = c(lon = -3.5, lat = 54.5),
        zoom = 6,
        maptype = c("roadmap"),
        messaging = FALSE,
        filename = "UKMap",
        color = c("color"),
        source = c("google"))


qmap(UK, zoom=6)
