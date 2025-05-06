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
# load areas
# list of folder names
folder_names <- c("DVO", "DVSB1", "DVSB2", "DVMS1", "DVMS2", "DVMS3", "DVMS4", 
                  "DVMS5", "DVMS6", "DVET", "DVNWT1", "DVNWT2", "DVNWT3", 
                  "DVNWT4", "DVNWT5", "DVWT1", "DVWT2", "DVWT3", "DVWT4", "DVWT5")

# create an empty list to store the shapefiles
areas_list <- list()

# loop through each folder name in the list
for (folder in folder_names) {
  # construct the file path
  folder_path <- paste0("areas_DV/", folder, "/area.shp")
  
  # check if the shapefile exists in the folder before loading
  if (file.exists(folder_path)) {
    # store the shapefile in the list with a name corresponding to the folder
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
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # Use alpha for transparency
  labs(title = "Dog Valley Watershed", fill = "Area ID") +
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
sites <- sites %>% rename(Site = SiteSub_ProjectB)
sites <- sites %>% rename(area = Area..m2.)

# make DV only data set
DVsites <- sites %>%
  filter(Code == "DV")

# ensure both data sets use the same CRS
# check the CRS of all_areas
crs_all_areas <- st_crs(all_areas)

# convert DV sites to an sf object
DVsites_sf <- st_as_sf(DVsites, coords = c("Lon", "Lat"), crs = 4326)  # assuming Lon/Lat are in WGS84
DVsites_sf <- st_transform(DVsites_sf, crs = st_crs(all_areas))  # transform to match CRS of all_areas

# extract coordinates from DVsites_sf
DVsites_sf <- cbind(DVsites_sf, st_coordinates(DVsites_sf))

# plot both data sets with coord_sf()
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # plot polygons with transparency
  labs(title = "Dog Valley Watershed", fill = "Area ID") +  # add title and legend label
  geom_sf(data = DVsites_sf, aes(fill = Site), color = "black", shape = 21, size = 3) +  # plot points from DVsites
  geom_sf_text(data = DVsites_sf, aes(label = Site), size = 4, vjust = -1, color = "black") +  # add text labels for Site IDs
  theme_minimal() +
  theme(legend.position = "right")  # adjust legend position


######################
#### Load streams ####
######################
streams <- st_read("areas_DV/DVO/area_stream_network.shp")

# plot both datasets with  the streams
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # plot polygons with transparency
  labs(title = "Dog Valley Watershed", fill = "Area ID") +  # add title and legend label
  geom_sf(data = DVsites_sf, aes(fill = Site), color = "black", shape = 21, size = 3) +  # plot points from DVsites
  geom_sf_text(data = DVsites_sf, aes(label = Site), size = 4, vjust = -1, color = "black") +  # add text labels for Site IDs
  geom_sf(data = streams, color = "blue",  alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "right")  # adjust legend position

# plot the areas with the streams
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # set transparency for visibility
  labs(title = "Dog Valley Watershed", fill = "Area ID") +
  geom_sf(data = streams, color = "blue") +
  theme_minimal() +
  theme(legend.position = "right")  # adjust legend position as needed

