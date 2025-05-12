##==============================================================================
## Project: QuEST
## Script to plot stuff on NM
##==============================================================================

##############
## Packages ##
##############
library(mapview)
library(dplyr)
# remotes::install_github('r-tmap/tmap')
library(tmap)
library(sf)
# if using spatial points,
library(sp)
# for color palettes
library(viridis)
library(paletteer)
# for plotting with ggplot
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
#library(vapoRwave)
library(tidyverse)

###############
#### Areas ####
###############
# load areas individually
# create an empty list to store the shape files
areas_list <- list()

# loop through folders 1 to 29
for (i in 1:29) {
  folder_path <- paste0("areas_NM/USF", i, "/area.shp")
  
  # check if the shape file exists in the folder before loading
  if (file.exists(folder_path)) {
    areas_list[[paste0("area", i)]] <- st_read(folder_path)
  }
}

# create a vector of folder names (site numbers)
folder_names <- sprintf("%02d", 1:29)  # Generates "01" to "29"

# add an Area_ID column to each shapefile in the list
for (i in seq_along(areas_list)) {
  areas_list[[i]]$Area_ID <- as.factor(folder_names[i])  # assigning the folder name as Area_ID
}

# combine all areas into a single data frame
all_areas <- do.call(rbind, areas_list)

# remove duplicate areas
#all_areas <- all_areas %>%
# distinct(Area_ID, .keep_all = TRUE)

#### plot the combined areas ####
# extract centroids
all_areas <- all_areas %>%
  mutate(Centroid = st_centroid(geometry)) %>%
  mutate(Latitude = st_coordinates(Centroid)[, 2],
         Longitude = st_coordinates(Centroid)[, 1])

# calculate representative points
all_areas <- all_areas %>%
  mutate(Latitude = st_coordinates(Centroid)[, 2],
         Longitude = st_coordinates(Centroid)[, 1])

# calculate area sizes and add a new column
all_areas$Size <- st_area(all_areas)

# Order the areas by size, from smallest to largest
all_areas <- all_areas[order(all_areas$Size, decreasing = TRUE), ]

### plot the combined areas with smaller areas first ###
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # Use alpha for transparency
  labs(title = "Santa Fe Watershed", fill = "Area ID") +
  geom_text(data = all_areas, aes(x = Longitude, y = Latitude, 
                                  label = Area_ID), color = "black", size = 3) +
  theme_minimal()


#### load streams ####
streams <- st_read("areas_NM/USF12/area_stream_network.shp")

# plot the areas with the streams
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # set transparency for visibility
  labs(title = "Santa Fe Watershed", fill = "Area ID") +
  geom_sf(data = streams, color = "blue") +
  theme_minimal() +
  theme(legend.position = "right")  # adjust legend position as needed

######################
#### PT locations ####
######################
# load leverage data
pt <- read.csv("data/NM_PT.csv")

# choose colors for maps. PLAY WITH THIS!
pal <- paletteer_c("ggthemes::Green-Blue-White Diverging", 30)
pal <- paletteer_c("ggthemes::Green", 43)

# transform lat lon to geometries
PT <- st_as_sf(pt, coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

# plot the areas with PT sites
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3) +
  labs(title = "Santa Fe Watershed - PT locations", fill = "Site") +
  scale_fill_manual(values = pal) +
  geom_sf(data = streams, color = "#5586B3") +
  geom_sf(data = PT, size = 4, shape = 21, fill = "black") +
  theme_minimal()

# can't get to not plot the areas in the legend if you want them with color... sorry
ggplot() +
  # plot all areas but not in the legend
  geom_sf(data = all_areas, alpha = 0.3, show.legend = FALSE) +
  
  # plot PT sites and set fill to Site to generate the legend entry
  geom_sf(data = PT, aes(fill = Site), size = 2.5, shape = 21, show.legend = TRUE) +
  
  # add streams to the map
  geom_sf(data = streams, color = "#5586B3") +
  
  # customize the legend and title
  labs(title = "Santa Fe Watershed - PT Locations", fill = "PT Sites") +
  
  # set color palette for PT site names
  scale_fill_manual(values = pal) +
  
  # minimal theme
  theme_minimal()

########################
#### Scan locations ####
########################
# only scan catchments
scan_areas <- all_areas[c("area12", "area20", "area21"),]
# load scan sites
pt <- read.csv("data/NM_scans.csv")

# choose colors for maps. PLAY WITH THIS!
pal <- paletteer_c("ggthemes::Green-Blue-White Diverging", 30)
pal <- paletteer_c("ggthemes::Green", 3)

#transform lat lon to geometries
PT <- st_as_sf(pt, coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

# plot the areas with scan sites
ggplot() +
  geom_sf(data = scan_areas, aes(fill = Area_ID), alpha = 0.4) +
  labs(title = "Santa Fe Watershed - scan locations", fill = "Site") +
  #scale_fill_manual(values = pal) +
  geom_sf(data = streams, color = "#5586B3") +
  geom_sf(data = PT, size = 4, shape = 21, fill = "red") +
  theme_minimal()

# plot with colored areas
ggplot() +
  geom_sf(data = scan_areas, aes(fill = Area_ID), alpha = 0.6) +
  labs(title = "Santa Fe Watershed - scan locations") + 
  scale_fill_manual(name = "Site Area", values = c("12" = "#FDE725", "20" = "#2B833E", "21" = "#440154"), 
                    labels = c("12" = "Lower", "20" = "Middle", "21" = "Upper")) +
  geom_sf(data = streams, color = "#5586B3") +
  geom_sf(data = PT, size = 4, shape = 21, fill = "black") +
  theme_minimal()

# plot with colored dots
ggplot() +
  geom_sf(data = scan_areas) +
  labs(title = "Santa Fe Watershed - scan locations") +
  geom_sf(data = streams, color = "#5586B3") +
  geom_sf(data = PT, aes(fill = Site), size = 4, shape = 21) +
  scale_fill_manual(name = "Site", values = c("USF12" = "#FDE725", "USF20" = "#2B833E", "USF21" = "#440154"), 
                                              labels = c("USF12" = "Lower", "USF20" = "Middle", "USF21" = "Upper")) +
  theme_minimal()

# can I get both colored?
ggplot() +
  geom_sf(data = scan_areas, aes(color = Area_ID), alpha = 0.6) +
  labs(title = "Santa Fe Watershed - scan locations") + 
  scale_color_manual(name = "Site Area", values = c("12" = "#FDE725", "20" = "#2B833E", "21" = "#440154"), 
                    labels = c("12" = "Lower", "20" = "Middle", "21" = "Upper")) +
  geom_sf(data = streams, color = "#5586B3") +
  geom_sf(data = PT, aes(fill = Site), size = 4, shape = 21) +
  scale_fill_manual(name = "Site", values = c("USF12" = "#FDE725", "USF20" = "#2B833E", "USF21" = "#440154"), 
                    labels = c("USF12" = "Lower", "USF20" = "Middle", "USF21" = "Upper")) +
  theme_minimal()
