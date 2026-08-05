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
# load areas
# list of folder names
folder_names <- c("BRM01", "BRMQ1", "BRAA1", "BRA01", "BRM02", "BRAB1", "BRB01", "BRM03", 
                  "BRMQ3", "BRM04", "BRCD1", "BRMQ4", "BRD01", "BRE01", "BRM05", "BRF01",
                  "BRM06", "BRM07", "BRC01", "BRA02")

# create an empty list to store the shapefiles
areas_list <- list()

# loop through each folder name in the list
for (folder in folder_names) {
  # construct the file path
  folder_path <- paste0("areas_BR/", folder, "/area.shp")
  
  # check if the shapefile exists in the folder before loading
  if (file.exists(folder_path)) {
    # Store the shapefile in the list with a name corresponding to the folder
    areas_list[[folder]] <- st_read(folder_path)
  }
}

# check the loaded shapefiles
print(areas_list)

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

# order the areas by size, from smallest to largest
all_areas <- all_areas[order(all_areas$Size, decreasing = TRUE), ]

# row names into values
all_areas <- tibble::rownames_to_column(all_areas, "Area_ID")

### plot the combined areas with smaller areas first ###
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # use alpha for transparency
  labs(title = "Brush Creek", fill = "Area ID") +
  geom_text(data = all_areas, aes(x = Longitude, y = Latitude, 
                                  label = Area_ID), color = "black", size = 3) +
  theme_minimal()

#####################################################
#### Load latitudes and longitutes vor watershed ####
#####################################################
## load site data
sites <- drive_get("https://docs.google.com/spreadsheets/d/1j5p29rslgqH6VpyjcZJ0-qPUaECY-9VW4YKDWdc_sro/edit?gid=0#gid=0")
# download the file as a csv file
drive_download(as_id(sites$id), path = "data/sites.csv", type = "csv", overwrite = T)
# fetch the file
sites <- read.csv("data/sites.csv")

# rename some columns
sites <- sites %>% rename(area = Area.m2)

# make DV only data set
BRsites <- sites %>%
  filter(Code == "BR")

      # ensure both datasets use the same CRS
# check the CRS of all_areas
crs_all_areas <- st_crs(all_areas)

# convert DVsites to an sf object
BRsites_sf <- st_as_sf(BRsites, coords = c("Lon", "Lat"), crs = 4326)  # assuming Lon/Lat are in WGS84
BRsites_sf <- st_transform(BRsites_sf, crs = st_crs(all_areas))  # Transform to match CRS of all_areas

# extract coordinates from BRsites_sf
BRsites_sf <- cbind(BRsites_sf, st_coordinates(BRsites_sf))

# plot both datasets with coord_sf()
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # plot polygons with transparency
  labs(title = "Brush Creek Watershed", fill = "Area ID") +  # add title and legend label
  geom_sf(data = BRsites_sf, aes(fill = Site), color = "black", shape = 21, size = 3) +  # plot points from DVsites
  geom_sf_text(data = BRsites_sf, aes(label = Site), size = 4, vjust = -1, color = "black") +  # add text labels for Site IDs
  theme_minimal() +
  theme(legend.position = "right")  # adjust legend position


######################
#### Load streams ####
######################
streams <- st_read("areas_BR/BRM01/area_stream_network.shp")

# plot both datasets with  the streams
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # plot polygons with transparency
  labs(title = "Brush Creek Watershed", fill = "Area ID") +  # add title and legend label
  geom_sf(data = BRsites_sf, aes(fill = Site), color = "black", shape = 21, size = 3) +  # plot points from DVsites
  geom_sf_text(data = BRsites_sf, aes(label = Site), size = 4, vjust = -1, color = "black") +  # add text labels for Site IDs
  geom_sf(data = streams, color = "blue", alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "right")  # adjust legend position

###########################
#### Plot Mengye's shp ####
###########################
BrushC <- st_read("BrushC_basins/BrushC_basins.shp")

ggplot() +
  geom_sf(data = BrushC) +  # use alpha for transparency
  labs(title = "Brush Creek Watershed") +
  theme_minimal()

###############################
#### Plot individual areas ####
###############################
# loop through each area in the list
for (area in seq_along(areas_list)) {
  p <- ggplot() +
    geom_sf(data = areas_list[[area]], alpha = 0.5) +  # use alpha for transparency
    labs(title = paste("Area", area)) +  # dynamic title based on area number
    theme_minimal() %>%
    print()  # explicitly print the plot
}

print(p)

# initialize an empty list to store plots
plot_list <- list()

# loop through each area and save each plot to the list
for (i in seq_along(areas_list)) {
  p <- ggplot() +
    geom_sf(data = areas_list[[i]], alpha = 0.5) +  # use alpha for transparency
    labs(title = paste("Area", i)) +  # dynamic title
    theme_minimal()
  
  # Store the plot in the list
  plot_list[[i]] <- p
}


# combine all areas into a single plot
p <- ggplot()

for (i in seq_along(areas_list)) {
  p <- p +
    geom_sf(data = areas_list[[i]], alpha = 0.5, aes(fill = factor(i))) +  # use fill to distinguish areas
    labs(title = "Interactive Areas", fill = "Area") +
    theme_minimal()
}

# convert ggplot object to plotly for interactivity
interactive_plot <- ggplotly(p)

# print the interactive plot
interactive_plot

# display a specific plot from the list, e.g., the first one
print(plot_list[[1]])

# assuming areas_list contains sf objects (spatial data frames)
# create a ggplot object with multiple areas
p <- ggplot()

# loop through your areas_list to add each geometry to the ggplot
for (i in seq_along(areas_list)) {
  p <- p +
    geom_sf(data = areas_list[[i]], alpha = 0.5, aes(fill = factor(i))) +  # color different areas
    labs(title = "Interactive Plotly Map", fill = "Area")
}

# convert ggplot to plotly for interactivity
interactive_plot <- ggplotly(p)

# render the interactive plot
interactive_plot

p <- ggplot() +
  # add polygons (areas)
  geom_sf(data = areas_list[[1]], fill = "blue", alpha = 0.5) +
  geom_sf(data = areas_list[[2]], fill = "green", alpha = 0.5) +
  
  # add another layer, e.g., points
  geom_sf(data = points_sf, color = "red", size = 3) +  # assuming points_sf is an sf object
  
  labs(title = "Interactive Layered Map") +
  theme_minimal()

# convert to plotly
interactive_plot <- ggplotly(p)

# render
interactive_plot

