##==============================================================================
## Project: QuEST
## Script to compare leverage with and without Q in NM
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

##################
#### Leverage ####
##################

# Load leverage data
leverage <- read.csv("data/leverage_nm.csv")

# Average values across days so there is only one value per site
levrage_avg <- leverage %>%
  group_by(variable, Site, Lat, Lon) %>%
  dplyr::summarize(Mean = mean(leverage, na.rm=TRUE))

# Choose colors for maps
pal <- paletteer_c("ggthemes::Temperature Diverging", 30)
dots <- paletteer_c("ggthemes::Temperature Diverging", 3)

#filter out by what you will use
### DOC ###
DOC <- levrage_avg %>%
  dplyr::filter(variable == "NPOC") %>%
  dplyr::select(Site, Lat, Lon, Mean)
#transform lat lon to geometries
DOC <- st_as_sf(DOC, coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

levpoints <- DOC %>%
  dplyr::select(Site, geometry)

# Plot the areas DOC
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3) +
  labs(title = "Santa Fe Watershed - DOC", fill = "Area ID") +
  scale_fill_manual(values = pal) +
  geom_sf(data = streams, color = "#2D3184") +
  geom_sf(data = DOC, aes(size = Mean), alpha = 0.5, shape = 21, fill = "black", color = "white") +
  scale_size(range = c(3, 10)) + # Adjust size range for visibility
  theme_minimal()

# Plot areas - DOC
ggplot() +
  # Plot all areas 
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3, fill = "gray") +
  labs(title = "Santa Fe Watershed - DOC", fill = "Area ID") +
  
  # Add rivers
  geom_sf(data = streams, color = "#2D3184") +
  
  # Plot DOC with color based on leverage values
  geom_sf(data = DOC, 
          aes(size = Mean, fill = ifelse(Mean > 0, "Positive", "Negative")),  # Use ifelse for fill based on leverage
          alpha = 0.5, 
          shape = 21, 
          color = "white") +
  
  # Use scale_fill_manual to define colors for positive and negative leverage
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue"), 
                    name = "Leverage") +
  
  # Adjust size range for visibility
  scale_size(range = c(3, 10), name = "Leverage Value") +
  
  # Customize guides
  guides(size = guide_legend(override.aes = list(fill = "black", color = "black")),
         fill = guide_legend(order = 1),  # Set order for the fill legend
         size = guide_legend(order = 2)  # Adjust size legend
  ) +
  
  theme_minimal()

### NO3 ###
NO3 <- levrage_avg %>%
  dplyr::filter(variable == "NO3") %>%
  dplyr::select(Site, Lat, Lon, Mean)
#transform lat lon to geometries
NO3 <- st_as_sf(NO3, coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

levpoints <- NO3 %>%
  dplyr::select(Site, geometry)

# Plot areas - NO3
ggplot() +
  # Plot all areas 
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3, fill = "gray") +
  labs(title = "Santa Fe Watershed - NO3", fill = "Area ID") +
  
  # Add rivers
  geom_sf(data = streams, color = "#2D3184") +
  
  # Plot NO3 with color based on leverage values
  geom_sf(data = NO3, 
          aes(size = Mean, fill = ifelse(Mean > 0, "Positive", "Negative")),  # Use ifelse for fill based on leverage
          alpha = 0.5, 
          shape = 21, 
          color = "white") +
  
  # Use scale_fill_manual to define colors for positive and negative leverage
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue"), 
                    name = "Leverage") +
  
  # Adjust size range for visibility
  scale_size(range = c(3, 10), name = "Leverage Value") +
  
  # Customize guides
  guides(size = guide_legend(override.aes = list(fill = "black", color = "black")),
         fill = guide_legend(order = 1),  # Set order for the fill legend
         size = guide_legend(order = 2)  # Adjust size legend
  ) +
  
  theme_minimal()

### Ca ###
Ca <- levrage_avg %>%
  dplyr::filter(variable == "Ca") %>%
  dplyr::select(Site, Lat, Lon, Mean)
#transform lat lon to geometries
Ca <- st_as_sf(Ca, coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

levpoints <- Ca %>%
  dplyr::select(Site, geometry)

# Plot the areas Ca
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3) +
  labs(title = "Santa Fe Watershed - Ca", fill = "Area ID") +
  scale_fill_manual(values = pal) +
  geom_sf(data = streams, color = "#2D3184") +
  geom_sf(data = Ca, aes(size = Mean, fill = Mean > 0), alpha = 0.5, shape = 21, color = "white") +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"), name = "Leverage") +
  scale_size(range = c(3, 10)) + # Adjust size range for visibility
  theme_minimal()

# Plot areas - Ca
ggplot() +
  # Plot all areas 
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3, fill = "gray") +
  labs(title = "Santa Fe Watershed - Ca", fill = "Area ID") +
  
  # Add rivers
  geom_sf(data = streams, color = "#2D3184") +
  
  # Plot Ca with color based on leverage values
  geom_sf(data = Ca, 
          aes(size = Mean, fill = ifelse(Mean > 0, "Positive", "Negative")),  # Use ifelse for fill based on leverage
          alpha = 0.5, 
          shape = 21, 
          color = "white") +
  
  # Use scale_fill_manual to define colors for positive and negative leverage
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue"), 
                    name = "Leverage") +
  
  # Adjust size range for visibility
  scale_size(range = c(3, 10), name = "Leverage Value") +
  
  # Customize guides
  guides(size = guide_legend(override.aes = list(fill = "black", color = "black")),
         fill = guide_legend(order = 1),  # Set order for the fill legend
         size = guide_legend(order = 2)  # Adjust size legend
  ) +
  
  theme_minimal()

### TDN ###
TDN <- levrage_avg %>%
  dplyr::filter(variable == "TDN") %>%
dplyr::select(Site, Lat, Lon, Mean)
#transform lat lon to geometries
TDN <- st_as_sf(TDN, coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

levpoints <- TDN %>%
  dplyr::select(Site, geometry)

# Plot the areas TDN
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3) +
  labs(title = "Santa Fe Watershed - TDN", fill = "Area ID") +
  scale_fill_manual(values = pal) +
  geom_sf(data = streams, color = "#2D3184") +
  geom_sf(data = TDN, aes(size = Mean), alpha = 0.5, shape = 21, fill = "black", color = "white") +
  scale_size(range = c(3, 10)) + # Adjust size range for visibility
  theme_minimal()





write.csv(levrage_avg, "leverage_avg.csv")

# Plot areas - absolute Ca values
ggplot() +
  # Plot all areas 
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3, fill = "gray") +
  labs(title = "Santa Fe Watershed - Ca", fill = "Area ID") +
  
  # Add rivers
  geom_sf(data = streams, color = "#2D3184") +
  
  # Plot Ca with color based on leverage values, and size based on abs(Mean)
  geom_sf(data = Ca, 
          aes(size = abs(Mean), fill = ifelse(Mean > 0, "Positive", "Negative")),  # Use abs(Mean) for size
          alpha = 0.5, 
          shape = 21, 
          color = "white") +
  
  # Use scale_fill_manual to define colors for positive and negative leverage
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue"), 
                    name = "Leverage") +
  
  # Adjust size range for visibility, with size based on absolute values
  scale_size_continuous(range = c(3, 10), name = "Leverage Value (abs)") +
  
  # Customize guides to reflect size and fill in the legend
  guides(size = guide_legend(override.aes = list(fill = "black", color = "black")),
         fill = guide_legend(order = 1),  # Set order for the fill legend
         size = guide_legend(order = 2)  # Adjust size legend
  ) +
  
  theme_minimal()



