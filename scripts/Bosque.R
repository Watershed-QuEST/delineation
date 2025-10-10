##==============================================================================
## Project: QuEST
## Script to Bosque stuff
##==============================================================================

##############
## Packages ##
##############
library(raster)
library(sf)
library(ggplot2)
library(paletteer) # for color palettes
library(mapview)
library(basemaps)
library(ggnewscale) 

###############################
#### SLO and VDO locations ####
###############################
# load bosque sites data
bosque <- read.csv("BOSQUE/bosque_sites.csv")
# transform lat lon to geometries
BSQ <- st_as_sf(bosque, coords = c("Longitude", "Latitude"), crs = '+proj=longlat +datum=WGS84 +no_defs')

# load DEM raster
dem <- raster(paste0("BOSQUE/dem_bosque.tif"))
#plot with mapview to check
mapview(dem)

# load stream shp file
streams <- st_read(paste0("BOSQUE/streams_bosque.shp"))
#assigns the coordinate reference system (CRS) of the stream data to match DEM system
st_crs(streams) <- st_crs(dem)
plot(streams)


##################################
#### Cropping DEM and Streams ####
##################################
# 1. Define a buffer around your points to create the cropping extent
# you might need to adjust the 'dist' value based on the data's scale and zoom level.
# use a projected CRS for buffering
# if the DEM is in a projected CRS. Transform BSQ to the DEM's CRS if it's not already.
BSQ_proj <- st_transform(BSQ, crs(dem))

# create a buffer around your points
buffer_dist <- 600 # meters, adjust as needed (e.g., 500 for 500 meters)
buffer_around_points <- st_buffer(BSQ_proj, dist = buffer_dist)

# get the bounding box of the buffered points
# this will be used to crop both raster and vector data.
crop_extent <- extent(buffer_around_points)

# 2. Crop the DEM raster
dem_cropped <- crop(dem, crop_extent)

# 3. Crop the streams
# use st_crop for sf objects. It can take an sf object as the extent.
streams_cropped <- st_crop(streams, buffer_around_points)

plot(dem_cropped)
plot(streams_cropped)

# convert raster to data.frame
dem_df <- as.data.frame(dem_cropped, xy = TRUE)
dem_proj <- st_transform(dem_df, crs(dem))
dem_proj <- st_as_sf(dem_df, coords = c("x", "y"), crs = '+proj=longlat +datum=WGS84 +no_defs')

############################################
#### Plot all sites with dem and stream ####
############################################
ggplot() +
  geom_sf(data = BSQ, aes(fill = WellType), alpha = 0.4) +
  labs(title = "SLO and VDO locations", fill = "WellType") +
  #scale_fill_manual(values = pal) +
  geom_sf(data = streams_cropped, color = "#5586B3") +
  geom_sf(data = dem_df) +
  theme_minimal()

plot(BSQ) + plot(streams_cropped, color = "#5586B3") + plot(dem_cropped)

# Load necessary libraries
library(raster)
library(sf)
library(ggplot2)
library(paletteer)
library(mapview)


###############################
#### SLO and VDO locations ####
###############################
# load bosque sites data
bosque <- read.csv("BOSQUE/bosque_sites.csv")
# transform lat lon to geometries
BSQ <- st_as_sf(bosque, coords = c("Longitude", "Latitude"), crs = 4326)

# load DEM raster
dem <- raster(paste0("BOSQUE/dem_newmex.tif"))
mapview(dem)

# load stream shp file
streams <- st_read(paste0("BOSQUE/streams_bosque.shp"))

# --- CRITICAL FIXES FOR CRS ALIGNMENT ---
# Check if streams has a CRS. If not, assign DEM's CRS directly.
if (is.na(st_crs(streams))) {
  message("Streams object has no CRS. Assigning DEM's CRS directly for alignment. ENSURE THIS IS GEOGRAPHICALLY CORRECT!")
  st_crs(streams) <- crs(dem)
}

# Transform BSQ to the DEM's CRS (for accurate buffering and alignment)
BSQ_proj <- st_transform(BSQ, crs(dem))

# Transform streams to the DEM's CRS if they are different
if (st_crs(streams) != crs(dem)) {
  message("Transforming streams to match DEM's CRS.")
  streams <- st_transform(streams, crs(dem))
}
# --- END CRS FIXES ---

plot(streams)


##################################
#### Cropping DEM and Streams ####
##################################
buffer_dist <- 600
BSQ_proj <- st_transform(BSQ, crs(dem)) # Ensure points are in DEM CRS
buffer_around_points <- st_buffer(BSQ_proj, dist = buffer_dist)
crop_extent <- extent(buffer_around_points)

dem_cropped <- crop(dem, crop_extent)
streams_cropped <- st_crop(streams, buffer_around_points)

plot(dem_cropped)
plot(streams_cropped)

dem_df <- as.data.frame(dem_cropped, xy = TRUE)
# Verify the column name for elevation values if 'dem_bosque' doesn't work: names(dem_df)

############################################
#### Plot all sites with dem and stream ####
############################################
ggplot() +
  # 1. Add the cropped DEM (elevation data) using geom_raster
  geom_raster(data = dem_df, aes(x = x, y = y, fill = dem_newmex)) + # Map fill to elevation
  scale_fill_gradientn(colors = paletteer_c("viridis::viridis", 100), name = "Elevation (m)") +
  
  # --- CRITICAL ADDITION: Reset the fill aesthetic scale ---
  # This tells ggplot2 that any subsequent 'fill' mappings will start a NEW fill scale.
  new_scale_fill() +
  
  # 2. Add the cropped streams using geom_sf
  geom_sf(data = streams_cropped, color = "#F0F0F0", linewidth = 2) +
  
  # 3. Add the BSQ points using geom_sf
  # Now, 'fill' here refers to the NEW fill scale established by new_scale_fill()
  geom_sf(data = BSQ_proj, aes(fill = WellSite), size = 4, shape = 21, color = "black") + # Map fill to WellType
  scale_fill_manual(name = "WellSite", # This manual scale applies to the NEW fill aesthetic
                    values = c("VDOW" = "#F8B620",
                               "VDOS" = "#57A337",
                               "SLOC" = "#A26DC2",
                               "SLOW" = "#1BA3C6",
                               "SLON" = "#C5BFBE", # Assign color to each
                               "SLOE" = "#C5BFBE",
                               "SLOS" = "#C5BFBE",
                               "VDON" = "#C5BFBE",
                               "VDOC" = "#C5BFBE",
                               "VDOE" = "#C5BFBE")) +
  
  labs(title = "SLO and VDO Locations with Elevation and Streams") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#####################################
#### Using basmap layers instead ####
#####################################
api_key <- "7b6db65ee9644fcf8f333afd6b91caf9"
data(ext)
# set defaults for the basemap
set_defaults(map_service = "osm", map_type = "landscape")


# Assuming 'bosque' is an sf object or can be converted to one
# If you know the original CRS (e.g., EPSG:4326 for WGS84 lat/long)
bosque_sf <- st_as_sf(BSQ, coords = c("longitude", "latitude"), crs = 4326)

# Then transform it to Web Mercator
bosque_transformed <- st_transform(bosque_sf, crs = 3857)

# Now plot with the transformed data
ggplot() +
  basemap_gglayer(ext) + # Or use your transformed bounding box/extent
  geom_sf(data = bosque_transformed, aes(...)) + # Plot your points
  scale_fill_identity() +
  coord_sf()

Sys.getenv("http_proxy")
Sys.getenv("https_proxy")

Sys.unsetenv("http_proxy")
Sys.unsetenv("https_proxy")

install.packages("basemap", dependencies = TRUE)
install.packages("curl", dependencies = TRUE) # curl is used for network requests
