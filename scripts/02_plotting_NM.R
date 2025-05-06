##==============================================================================
## Project: QuEST
## Script to compare leverage with and without Q in NM
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
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # use alpha for transparency
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

# plot areas with the streams
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # plot polygons with transparency
  labs(title = "Santa Fe Watershed", fill = "Area ID") +  # add title and legend label
  geom_sf(data = NMsites_sf, aes(fill = Site), color = "black", shape = 21, size = 3) +  # plot points from DVsites
  geom_sf_text(data = NMsites_sf, aes(label = Site), size = 4, vjust = -1, color = "black") +  # add text labels for Site IDs
  geom_sf(data = streams, color = "blue", alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "right")  # adjust legend position


#####################################################
#### Load latitudes and longitutes vor watershed ####
#####################################################
## load site data
sites <- drive_get("https://docs.google.com/spreadsheets/d/1j5p29rslgqH6VpyjcZJ0-qPUaECY-9VW4YKDWdc_sro/edit?gid=0#gid=0")
# download the file as a csv file
drive_download(as_id(sites$id), path = "data/sites.csv", type = "csv", overwrite = T)
# Fetch the file
sites <- read.csv("data/sites.csv")

# ensure both datasets use the same CRS
# check the CRS of all_areas
crs_all_areas <- st_crs(all_areas)

# make NM only data set
NMsites <- sites %>%
  filter(Code == "NM")

# remove some sites that we don't want
NMsites <- NMsites %>%
  filter(Site != "USF31")

# convert NMsites to an sf object
NMsites_sf <- st_as_sf(NMsites, coords = c("Lon", "Lat"), crs = 4326)  # assuming Lon/Lat are in WGS84
NMsites_sf <- st_transform(NMsites_sf, crs = st_crs(all_areas))  # transform to match CRS of all_areas

# extract coordinates from NHsites_sf
NMsites_sf <- cbind(NMsites_sf, st_coordinates(NMsites_sf))

# plot both datasets with coord_sf()
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # plot polygons with transparency
  labs(title = "Santa Fe Watershed", fill = "Area ID") +  # add title and legend label
  geom_sf(data = NMsites_sf, aes(fill = Site), color = "black", shape = 21, size = 3) +  # plot points from DVsites
  geom_sf_text(data = NMsites_sf, aes(label = Site), size = 4, vjust = -1, color = "black") +  # add text labels for Site IDs
  theme_minimal() +
  theme(legend.position = "right")  # adjust legend position


##################
#### Leverage ####
##################
# load leverage data
leverage <- read.csv("data/leverage_nm.csv")

# average values across days so there is only one value per site
levrage_avg <- leverage %>%
  group_by(variable, Site, Lat, Lon) %>%
  dplyr::summarize(Mean = mean(leverage, na.rm=TRUE))

# choose colors for maps
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

# plot the areas DOC
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3) +
  labs(title = "Santa Fe Watershed - DOC", fill = "Area ID") +
  scale_fill_manual(values = pal) +
  geom_sf(data = streams, color = "#2D3184") +
  geom_sf(data = DOC, aes(size = Mean), alpha = 0.5, shape = 21, fill = "black", color = "white") +
  scale_size(range = c(3, 10)) + # adjust size range for visibility
  theme_minimal()

# plot areas - DOC
ggplot() +
  # plot all areas 
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3, fill = "gray") +
  labs(title = "Santa Fe Watershed - DOC", fill = "Area ID") +
  
  # add rivers
  geom_sf(data = streams, color = "#2D3184") +
  
  # plot DOC with color based on leverage values
  geom_sf(data = DOC, 
          aes(size = Mean, fill = ifelse(Mean > 0, "Positive", "Negative")),  # use ifelse for fill based on leverage
          alpha = 0.5, 
          shape = 21, 
          color = "white") +
  
  # use scale_fill_manual to define colors for positive and negative leverage
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue"), 
                    name = "Leverage") +
  
  # adjust size range for visibility
  scale_size(range = c(3, 10), name = "Leverage Value") +
  
  # customize guides
  guides(size = guide_legend(override.aes = list(fill = "black", color = "black")),
         fill = guide_legend(order = 1),  # set order for the fill legend
         size = guide_legend(order = 2)  # adjust size legend
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

# plot areas - NO3
ggplot() +
  # plot all areas 
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3, fill = "gray") +
  labs(title = "Santa Fe Watershed - NO3", fill = "Area ID") +
  
  # add rivers
  geom_sf(data = streams, color = "#2D3184") +
  
  # plot NO3 with color based on leverage values
  geom_sf(data = NO3, 
          aes(size = Mean, fill = ifelse(Mean > 0, "Positive", "Negative")),  # use ifelse for fill based on leverage
          alpha = 0.5, 
          shape = 21, 
          color = "white") +
  
  # use scale_fill_manual to define colors for positive and negative leverage
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue"), 
                    name = "Leverage") +
  
  # adjust size range for visibility
  scale_size(range = c(3, 10), name = "Leverage Value") +
  
  # customize guides
  guides(size = guide_legend(override.aes = list(fill = "black", color = "black")),
         fill = guide_legend(order = 1),  # set order for the fill legend
         size = guide_legend(order = 2)  # adjust size legend
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

# plot the areas Ca
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3) +
  labs(title = "Santa Fe Watershed - Ca", fill = "Area ID") +
  scale_fill_manual(values = pal) +
  geom_sf(data = streams, color = "#2D3184") +
  geom_sf(data = Ca, aes(size = Mean, fill = Mean > 0), alpha = 0.5, shape = 21, color = "white") +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"), name = "Leverage") +
  scale_size(range = c(3, 10)) + # adjust size range for visibility
  theme_minimal()

# plot areas - Ca
ggplot() +
  # plot all areas 
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3, fill = "gray") +
  labs(title = "Santa Fe Watershed - Ca", fill = "Area ID") +
  
  # add rivers
  geom_sf(data = streams, color = "#2D3184") +
  
  # plot Ca with color based on leverage values
  geom_sf(data = Ca, 
          aes(size = Mean, fill = ifelse(Mean > 0, "Positive", "Negative")),  # use ifelse for fill based on leverage
          alpha = 0.5, 
          shape = 21, 
          color = "white") +
  
  # use scale_fill_manual to define colors for positive and negative leverage
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue"), 
                    name = "Leverage") +
  
  # adjust size range for visibility
  scale_size(range = c(3, 10), name = "Leverage Value") +
  
  # customize guides
  guides(size = guide_legend(override.aes = list(fill = "black", color = "black")),
         fill = guide_legend(order = 1),  # set order for the fill legend
         size = guide_legend(order = 2)  # adjust size legend
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

# plot the areas TDN
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3) +
  labs(title = "Santa Fe Watershed - TDN", fill = "Area ID") +
  scale_fill_manual(values = pal) +
  geom_sf(data = streams, color = "#2D3184") +
  geom_sf(data = TDN, aes(size = Mean), alpha = 0.5, shape = 21, fill = "black", color = "white") +
  scale_size(range = c(3, 10)) + # adjust size range for visibility
  theme_minimal()


write.csv(levrage_avg, "leverage_avg.csv")

# plot areas - absolute Ca values
ggplot() +
  # plot all areas 
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3, fill = "gray") +
  labs(title = "Santa Fe Watershed - Ca", fill = "Area ID") +
  
  # add rivers
  geom_sf(data = streams, color = "#2D3184") +
  
  # plot Ca with color based on leverage values, and size based on abs(Mean)
  geom_sf(data = Ca, 
          aes(size = abs(Mean), fill = ifelse(Mean > 0, "Positive", "Negative")),  # use abs(Mean) for size
          alpha = 0.5, 
          shape = 21, 
          color = "white") +
  
  # use scale_fill_manual to define colors for positive and negative leverage
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue"), 
                    name = "Leverage") +
  
  # adjust size range for visibility, with size based on absolute values
  scale_size_continuous(range = c(3, 10), name = "Leverage Value (abs)") +
  
  # customize guides to reflect size and fill in the legend
  guides(size = guide_legend(override.aes = list(fill = "black", color = "black")),
         fill = guide_legend(order = 1),  # set order for the fill legend
         size = guide_legend(order = 2)  # adjust size legend
  ) +
  
  theme_minimal()



