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
### BR -------------------------------------------------------------------------
# List of folder names
folder_names <- c("BRM01", "BRMQ1", "BRAA1", "BRA01", "BRM02", "BRAB1", "BRB01", "BRM03", 
                  "BRMQ3", "BRM04", "BRCD1", "BRMQ4", "BRD01", "BRE01", "BRM05", "BRF01",
                  "BRM06", "BRM07", "BRC01", "BRA02")

### DV -------------------------------------------------------------------------
folder_names <- c("DVO", "DVSB1", "DVSB2", "DVMS1", "DVMS2", "DVMS3", "DVMS4", 
                  "DVMS5", "DVMS6", "DVET", "DVNWT1", "DVNWT2", "DVNWT3", 
                  "DVNWT4", "DVNWT5", "DVWT1", "DVWT2", "DVWT3", "DVWT4", "DVWT5")

### NM -------------------------------------------------------------------------
# List of folder names
folder_names <- c("USF1", "USF2", "USF3", "USF4", "USF5", "USF6", "USF7", "USF8", "USF9", "USF10", 
                  "USF11", "USF12", "USF13", "USF14", "USF15", "USF16", "USF17", "USF18", "USF19", 
                  "USF20", "USF21", "USF22", "USF23", "USF24", "USF25", "USF26", "USF27", "USF28", "USF29")

# List of folder names
folder_names <- c("USF40", "USF41", "USF4", "USF2")

### NH -------------------------------------------------------------------------
# List of folder names
folder_names <- c("LMP00", "DCR", "OMC", "LMP01", "SMB", "CTB", "LMP07", "LMP09", "PRC",
                  "LST01", "NCB", "NCB-down", "LMP12", "HRB", "LMP19", "NBR-up", "NBR", "DDB", "LMP27")

### SS -------------------------------------------------------------------------
# List of folder names
folder_names <- c("SSM01", "SST02", "SST03", "SST04", "SST05", "SST06", "SST07", 
                  "SST08", "SST09", "SSM10", "SST11", "SST12", "SST13", "SST14", 
                  "SST15", "SST16", "SST17", "SST18", "SST19", "SSM20", "SSMFN")


# Create an empty list to store the shapefiles
areas_list <- list()

# Loop through each folder name in the list
for (folder in folder_names) {
  # Construct the file path 
  #### change for areas_BR, areas_DV, areas_NH, areas_NM, areas_SS 
  folder_path <- paste0("areas_NM/", folder, "/area.shp")
  
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
 ## Change output dir according to the catchment you are working on ##
output_dir <- "SantaFe_basins/KML"

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