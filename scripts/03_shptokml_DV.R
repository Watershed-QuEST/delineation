##==============================================================================
## Project: QuEST
## Script to transform shp files to KML
##==============================================================================

##################
#### Packages ####
##################
library(sf) # For handling spatial data
library(rgdal) # For KML/KMZ support

#########################
#### Load your areas ####
#########################

# List of folder names
folder_names <- c("DVO", "DVSB1", "DVSB2", "DVMS1", "DVMS2", "DVMS3", "DVMS4", 
                  "DVMS5", "DVMS6", "DVET", "DVNWT1", "DVNWT2", "DVNWT3", 
                  "DVNWT4", "DVNWT5", "DVWT1", "DVWT2", "DVWT3", "DVWT4", "DVWT5")

# Create an empty list to store the shapefiles
areas_list <- list()

# Loop through each folder name in the list
for (folder in folder_names) {
  # Construct the file path
  folder_path <- paste0("areas_DV/", folder, "/area.shp")
  
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

##############################
#### Transform shp to KML ####
##############################

#### THIS WILL SAVE THE KMLS TO YOUR FILES SO SPECIFY WHERE YOU WANT THEM ####
# Specify the directory where you want to save the KML files
output_dir <- "Dogvalley_basins/KML"

# Create an empty list to store KML areas
areas_kml <- list()

# Loop over each area in the list and write to KML
for (i in seq_along(areas_list)) {
  
  # Get the current sf object from areas_list
  single_area <- areas_list[[i]]
  
  # Get the area name from the list names
  area_name <- names(areas_list)[i]
  
  # Define a unique file name using the valid area name
  file_name <- paste0(area_name, ".kml")
  
  # Write the area to a KML file
  st_write(single_area, file_name, driver = "KML", delete_dsn = TRUE)
  
  # Store the area object in the list and set its name
  areas_kml[[area_name]] <- single_area
}

###################################
#### IF OU ONLY NEED TO DO ONE ####
###################################
# # Load the shapefile
# polygon_sf <- st_read("areas_NH/CTB/area.shp")
# 
# # Write to KML
# st_write(polygon_sf, "polygon.kml", driver = "KML")
# 
# # Create a KMZ by zipping the KML file
# zip("polygon.kmz", "polygon.kml")