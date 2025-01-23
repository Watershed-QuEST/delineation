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

#load areas individually
# Create an empty list to store the shape files
areas_list <- list()

# Loop through folders 1 to 29
for (i in 1:29) {
  folder_path <- paste0("areas_NM/USF", i, "/area.shp")
  
  # Check if the shape file exists in the folder before loading
  if (file.exists(folder_path)) {
    areas_list[[paste0("area", i)]] <- st_read(folder_path)
  }
}

# Create a vector of folder names (site numbers)
folder_names <- sprintf("%02d", 1:29)  # Generates "01" to "29"

# Add an Area_ID column to each shapefile in the list
for (i in seq_along(areas_list)) {
  areas_list[[i]]$Area_ID <- as.factor(folder_names[i])  # Assigning the folder name as Area_ID
}

# Combine all areas into a single data frame
all_areas <- do.call(rbind, areas_list)

# Remove duplicate areas
#all_areas <- all_areas %>%
# distinct(Area_ID, .keep_all = TRUE)

#### Plot the combined areas ####
# Extract centroids
all_areas <- all_areas %>%
  mutate(Centroid = st_centroid(geometry)) %>%
  mutate(Latitude = st_coordinates(Centroid)[, 2],
         Longitude = st_coordinates(Centroid)[, 1])

# Calculate representative points
all_areas <- all_areas %>%
  mutate(Latitude = st_coordinates(Centroid)[, 2],
         Longitude = st_coordinates(Centroid)[, 1])

# Calculate area sizes and add a new column
all_areas$Size <- st_area(all_areas)

# Order the areas by size, from smallest to largest
all_areas <- all_areas[order(all_areas$Size, decreasing = TRUE), ]

### Plot the combined areas with smaller areas first ###
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # Use alpha for transparency
  labs(title = "Santa Fe Watershed", fill = "Area ID") +
  geom_text(data = all_areas, aes(x = Longitude, y = Latitude, 
                                  label = Area_ID), color = "black", size = 3) +
  theme_minimal()


#### Load streams ####
streams <- st_read("areas_NM/USF12/area_stream_network.shp")

# Plot the areas with the streams
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # Set transparency for visibility
  labs(title = "Santa Fe Watershed", fill = "Area ID") +
  geom_sf(data = streams, color = "blue") +
  theme_minimal() +
  theme(legend.position = "right")  # Adjust legend position as needed

######################
#### PT locations ####
######################

# Load leverage data
pt <- read.csv("data/NM_PT.csv")

# Choose colors for maps. PLAY WITH THIS!
pal <- paletteer_c("ggthemes::Green-Blue-White Diverging", 30)
pal <- paletteer_c("ggthemes::Green", 43)

#transform lat lon to geometries
PT <- st_as_sf(pt, coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

# Plot the areas with PT sites
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3) +
  labs(title = "Santa Fe Watershed - PT locations", fill = "Site") +
  scale_fill_manual(values = pal) +
  geom_sf(data = PT, size = 3, shape = 21, fill = "black") +
  geom_sf(data = streams, color = "#5586B3") +
  theme_minimal()

# Can't get to not plot the areas in the legend if you want them with color... sorry
ggplot() +
  # Plot all areas but not in the legend
  geom_sf(data = all_areas, alpha = 0.3, show.legend = FALSE) +
  
  # Plot PT sites and set fill to Site to generate the legend entry
  geom_sf(data = PT, aes(fill = Site), size = 2.5, shape = 21, show.legend = TRUE) +
  
  # Add streams to the map
  geom_sf(data = streams, color = "#5586B3") +
  
  # Customize the legend and title
  labs(title = "Santa Fe Watershed - PT Locations", fill = "PT Sites") +
  
  # Set color palette for PT site names
  scale_fill_manual(values = pal) +
  
  # Minimal theme
  theme_minimal()
