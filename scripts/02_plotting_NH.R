##==============================================================================
## Project: QuEST
## Script to compare plot Dog Valley's areas
##==============================================================================

##################
#### Packages ####
##################

library(dplyr)
library(sf)
library(sp)
library(ggplot2)
library(ggspatial)
library(plotly)
library(googledrive) 
library(googlesheets4)

###############
#### Areas ####
###############

# Load areas
# List of folder names
folder_names <- c("LMP00", "DCR", "OMC", "LMP01", "SMB", "CTB", "LMP07", "LMP09", "PRC",
                  "LST01", "NCB", "NCB-down", "LMP12", "HRB", "LMP19", "NBR-up", "NBR", "DDB", "LMP27")

# Create an empty list to store the shapefiles
areas_list <- list()

# Loop through each folder name in the list
for (folder in folder_names) {
  # Construct the file path
  folder_path <- paste0("areas_NH/", folder, "/area.shp")
  
  # Check if the shapefile exists in the folder before loading
  if (file.exists(folder_path)) {
    # Store the shapefile in the list with a name corresponding to the folder
    areas_list[[folder]] <- st_read(folder_path)
  }
}

# Check the loaded shapefiles
print(areas_list)

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

# Row names into values
all_areas <- tibble::rownames_to_column(all_areas, "Area_ID")

### Plot the combined areas with smaller areas first ###
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # Use alpha for transparency
  labs(title = "Lamprey Watershed", fill = "Area ID") +
  geom_text(data = all_areas, aes(x = Longitude, y = Latitude, 
                                  label = Area_ID), color = "black", size = 3) +
  theme_minimal()

#####################################################
#### Load latitudes and longitutes vor watershed ####
#####################################################

## Load site data
sites <- drive_get("https://docs.google.com/spreadsheets/d/1j5p29rslgqH6VpyjcZJ0-qPUaECY-9VW4YKDWdc_sro/edit?gid=0#gid=0")
# Download the file as a csv file
drive_download(as_id(sites$id), path = "data/sites.csv", type = "csv", overwrite = T)
# Fetch the file
sites <- read.csv("data/sites.csv")

# Remove LMP72 (not part of watershed)
# Make DV only data set
sites <- sites %>%
  filter(SiteSub_ProjectB != "LMP72")


# Rename some columns
sites <- sites %>% rename(Site = SiteSub_ProjectB)
sites <- sites %>% rename(area = Area..m2.)

# Make DV only data set
NHsites <- sites %>%
  filter(Code == "NH")
 
# Ensure both datasets use the same CRS
# Check the CRS of all_areas
crs_all_areas <- st_crs(all_areas)

# Convert DVsites to an sf object
NHsites_sf <- st_as_sf(NHsites, coords = c("Longitude", "Latitude"), crs = 4326)  # Assuming Lon/Lat are in WGS84
NHsites_sf <- st_transform(NHsites_sf, crs = st_crs(all_areas))  # Transform to match CRS of all_areas

# Extract coordinates from NHsites_sf
NHsites_sf <- cbind(NHsites_sf, st_coordinates(NHsites_sf))

# Plot both datasets with coord_sf()
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # Plot polygons with transparency
  labs(title = "Lamprey Watershed", fill = "Area ID") +  # Add title and legend label
  geom_sf(data = NHsites_sf, aes(fill = Site), color = "black", shape = 21, size = 3) +  # Plot points from DVsites
  geom_sf_text(data = NHsites_sf, aes(label = Site), size = 4, vjust = -1, color = "black") +  # Add text labels for Site IDs
  theme_minimal() +
  theme(legend.position = "right")  # Adjust legend position


######################
#### Load streams ####
######################

streams <- st_read("areas_NH/LMP27/area_stream_network.shp")

# Plot both datasets with  the streams
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # Plot polygons with transparency
  labs(title = "Lamprey Watershed", fill = "Area ID") +  # Add title and legend label
  geom_sf(data = NHsites_sf, aes(fill = Site), color = "black", shape = 21, size = 3) +  # Plot points from DVsites
  geom_sf_text(data = NHsites_sf, aes(label = Site), size = 4, vjust = -1, color = "black") +  # Add text labels for Site IDs
  geom_sf(data = streams, color = "blue", alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "right")  # Adjust legend position

###########################
#### Plot Mengye's shp ####
###########################

lamprey <- st_read("Lamprey_basins/Lamprey_basins.shp")

ggplot() +
  geom_sf(data = lamprey) +  # Use alpha for transparency
  labs(title = "Lamprey Watershed") +
  theme_minimal()

###############################
#### Plot individual areas ####
###############################
# Loop through each area in the list
for (area in seq_along(areas_list)) {
  p <- ggplot() +
   geom_sf(data = areas_list[[area]], alpha = 0.5) +  # Use alpha for transparency
   labs(title = paste("Area", area)) +  # Dynamic title based on area number
   theme_minimal() %>%
   print()  # Explicitly print the plot
}

print(p)

# Initialize an empty list to store plots
plot_list <- list()

# Loop through each area and save each plot to the list
for (i in seq_along(areas_list)) {
  p <- ggplot() +
    geom_sf(data = areas_list[[i]], alpha = 0.5) +  # Use alpha for transparency
    labs(title = paste("Area", i)) +  # Dynamic title
    theme_minimal()
  
  # Store the plot in the list
  plot_list[[i]] <- p
}


# Combine all areas into a single plot
p <- ggplot()

for (i in seq_along(areas_list)) {
  p <- p +
    geom_sf(data = areas_list[[i]], alpha = 0.5, aes(fill = factor(i))) +  # Use fill to distinguish areas
    labs(title = "Interactive Areas", fill = "Area") +
    theme_minimal()
}

# Convert ggplot object to plotly for interactivity
interactive_plot <- ggplotly(p)

# Print the interactive plot
interactive_plot


# Display a specific plot from the list, e.g., the first one
print(plot_list[[1]])

# Assuming areas_list contains sf objects (spatial data frames)
# Create a ggplot object with multiple areas
p <- ggplot()

# Loop through your areas_list to add each geometry to the ggplot
for (i in seq_along(areas_list)) {
  p <- p +
    geom_sf(data = areas_list[[i]], alpha = 0.5, aes(fill = factor(i))) +  # Color different areas
    labs(title = "Interactive Plotly Map", fill = "Area")
}

# Convert ggplot to plotly for interactivity
interactive_plot <- ggplotly(p)

# Render the interactive plot
interactive_plot

p <- ggplot() +
  # Add polygons (areas)
  geom_sf(data = areas_list[[1]], fill = "blue", alpha = 0.5) +
  geom_sf(data = areas_list[[2]], fill = "green", alpha = 0.5) +
  
  # Add another layer, e.g., points
  geom_sf(data = points_sf, color = "red", size = 3) +  # Assuming points_sf is an sf object
  
  labs(title = "Interactive Layered Map") +
  theme_minimal()

# Convert to plotly
interactive_plot <- ggplotly(p)

# Render
interactive_plot
