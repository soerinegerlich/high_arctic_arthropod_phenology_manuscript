#Global map
library(remotes)
#install_github("r-spatial/sf")
#install.packages("sf")
library(sf)
library(ggplot2)
#install.packages("rnaturalearthdata")
#install.packages("rnaturalearth")
library(rnaturalearthdata)
library(rnaturalearth)
library(dplyr)

#crs <- "+proj=laea + x_0=4315000 + y_0=3210000 + lon_0=46 + lat_0=-18 + ellps=WGS84 + units=m +no_defs"
crs_new <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#crs <- "+proj=laea +lat_0=-60 +lon_0=180 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0"

ctrys50m <- ne_countries(scale = 50, type = "countries", returnclass = "sf") 

sphere <- st_graticule(ndiscr = 10000, margin = 10e-6) %>%
  st_transform(crs = crs_new) %>%
  #st_transform(crs = crs_new, aoi = c(-180,55,180,90)) %>%
  st_convex_hull() %>%
  summarise(geometry = st_union(geometry))

site_name <- "Zackenberg"
long <- -20.56
lat <- 74.47

site_coordinates <- data.frame(site_name)
site_coordinates$long <- long
site_coordinates$lat <- lat

#sites <- SpatialPoints(coords = site_coordinates[,2:3], proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

site_coordinates <- st_as_sf(site_coordinates,coords = 2:3)

site_coordinates <- site_coordinates %>% 
  st_set_crs(4326) %>% st_transform(crs=32736)

#europe_cropped <- st_crop(sphere, 
#                          xmin = -20, xmax = 45,
#                          ymin = 30, ymax = 73)

ggplot()  +
  geom_sf(data = sphere, fill = "white", alpha = 0.7)+
  #ylim=c(55, 90), xlim=c(-180, 180)) +
  #geom_sf(data = europe_cropped) +
  geom_sf(data = ctrys50m, fill="white") +
  
  geom_sf(data = site_coordinates, color = "#00abff", size = 6)+
  
  #annotate("point", x = 60, y = -60, colour = "red", size = 4) +
  coord_sf(default_crs = sf::st_crs(4326), xlim=c(-90, 180), ylim=c(55, 45), expand = TRUE) +
  #coord_sf(default_crs = sf::st_crs(4326), xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE) +
  #scale_fill_viridis_c(option = "magma",begin = 0.1)+
  theme_bw()

###################KERGUELEN########################################

crs_new <- "+proj=laea +lat_0=-90 +lon_0=-0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#crs <- "+proj=laea +lat_0=-60 +lon_0=180 +datum=WGS84 +ellps=WGS84 +no_defs +towgs84=0,0,0"

ctrys50m <- ne_countries(scale = 50, type = "countries", returnclass = "sf") 

sphere <- st_graticule(ndiscr = 10000, margin = 10e-6) %>%
  st_transform(crs = crs_new) %>%
  #st_transform(crs = crs_new, aoi = c(-180,55,180,90)) %>%
  st_convex_hull() %>%
  summarise(geometry = st_union(geometry))

site_name <- "Kerguelen"
#long <- -20.56
#lat <- 74.47
long <- 69.25
lat <- -49.33

site_coordinates <- data.frame(site_name)
site_coordinates$long <- long
site_coordinates$lat <- lat

#sites <- SpatialPoints(coords = site_coordinates[,2:3], proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

site_coordinates <- st_as_sf(site_coordinates,coords = 2:3)

site_coordinates <- site_coordinates %>% 
  st_set_crs(4326) %>% st_transform(crs=32736)

#europe_cropped <- st_crop(sphere, 
#                          xmin = -20, xmax = 45,
#                          ymin = 30, ymax = 73)

ggplot()  +
  geom_sf(data = sphere, fill = "white", alpha = 0.7)+
  #ylim=c(55, 90), xlim=c(-180, 180)) +
  #geom_sf(data = europe_cropped) +
  geom_sf(data = ctrys50m, fill="white") +
  
  geom_sf(data = site_coordinates, color = "#00abff", size = 6)+
  
  #annotate("point", x = 60, y = -60, colour = "red", size = 4) +
  coord_sf(default_crs = sf::st_crs(4326), xlim=c(-90, 180), ylim=c(-55, -45), expand = TRUE) +
  #coord_sf(default_crs = sf::st_crs(4326), xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE) +
  #scale_fill_viridis_c(option = "magma",begin = 0.1)+
  theme_bw()



