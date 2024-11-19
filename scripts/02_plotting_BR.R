##==============================================================================
## Project: QuEST
## Script to compare plot Brush Creek's areas
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
folder_names <- c("BRM01", "BRMQ1", "BRAA1", "BRA01", "BRM02", "BRAB1", "BRB01", "BRM03", 
                  "BRMQ3", "BRM04", "BRCD1", "BRMQ4", "BRD01", "BRE01", "BRM05", "BRF01",
                  "BRM06", "BRM07", "BRC01", "BRA02")

# Create an empty list to store the shapefiles
areas_list <- list()

# Loop through each folder name in the list
for (folder in folder_names) {
  # Construct the file path
  folder_path <- paste0("areas_BR/", folder, "/area.shp")
  
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
  labs(title = "Brush Creek", fill = "Area ID") +
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


# Rename some columns
sites <- sites %>% rename(area = Area.m2)

# Make DV only data set
BRsites <- sites %>%
  filter(Code == "BR")

      # Ensure both datasets use the same CRS
# Check the CRS of all_areas
crs_all_areas <- st_crs(all_areas)

# Convert DVsites to an sf object
BRsites_sf <- st_as_sf(BRsites, coords = c("Lon", "Lat"), crs = 4326)  # Assuming Lon/Lat are in WGS84
BRsites_sf <- st_transform(BRsites_sf, crs = st_crs(all_areas))  # Transform to match CRS of all_areas

# Extract coordinates from BRsites_sf
BRsites_sf <- cbind(BRsites_sf, st_coordinates(BRsites_sf))

# Plot both datasets with coord_sf()
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # Plot polygons with transparency
  labs(title = "Brush Creek Watershed", fill = "Area ID") +  # Add title and legend label
  geom_sf(data = BRsites_sf, aes(fill = Site), color = "black", shape = 21, size = 3) +  # Plot points from DVsites
  geom_sf_text(data = BRsites_sf, aes(label = Site), size = 4, vjust = -1, color = "black") +  # Add text labels for Site IDs
  theme_minimal() +
  theme(legend.position = "right")  # Adjust legend position


######################
#### Load streams ####
######################

streams <- st_read("areas_BR/BRM01/area_stream_network.shp")

# Plot both datasets with  the streams
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # Plot polygons with transparency
  labs(title = "Brush Creek Watershed", fill = "Area ID") +  # Add title and legend label
  geom_sf(data = BRsites_sf, aes(fill = Site), color = "black", shape = 21, size = 3) +  # Plot points from DVsites
  geom_sf_text(data = BRsites_sf, aes(label = Site), size = 4, vjust = -1, color = "black") +  # Add text labels for Site IDs
  geom_sf(data = streams, color = "blue", alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "right")  # Adjust legend position

###########################
#### Plot Mengye's shp ####
###########################

BrushC <- st_read("BrushC_basins/BrushC_basins.shp")

ggplot() +
  geom_sf(data = BrushC) +  # Use alpha for transparency
  labs(title = "Brush Creek Watershed") +
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

