library(ggmap)

london <- get_map(location = "London", source = "osm")
ggmap(london, extent = 'device')
