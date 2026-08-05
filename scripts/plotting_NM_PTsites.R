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
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # Use alpha for transparency
  labs(title = "Santa Fe Watershed", fill = "Area ID") +
  geom_text(data = all_areas, aes(x = Longitude, y = Latitude, 
                                  label = Area_ID), color = "black", size = 3) +
  theme_minimal()


#### load streams ####
streams <- st_read("areas_NM/USF1/area_stream_network.shp")

# plot the areas with the streams
ggplot() +
  #geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.5) +  # set transparency for visibility
  labs(title = "Santa Fe Watershed", fill = "Area ID") +
  geom_sf(data = streams, color = "#1C86EE", ) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
  #theme(legend.position = "right")  # adjust legend position as needed

######################
#### PT locations ####
######################
# load leverage data
pt <- read.csv("data/NM_PT.csv")

# choose colors for maps. PLAY WITH THIS!
pal <- paletteer_c("ggthemes::Green-Blue-White Diverging", 30)
pal <- paletteer_c("ggthemes::Green", 43)

# transform lat lon to geometries
PT <- st_as_sf(pt, coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

# plot the areas with PT sites
ggplot() +
  geom_sf(data = all_areas, aes(fill = Area_ID), alpha = 0.3) +
  labs(title = "Santa Fe Watershed - PT locations", fill = "Site") +
  scale_fill_manual(values = pal) +
  geom_sf(data = streams, color = "#5586B3") +
  geom_sf(data = PT, size = 3, shape = 21, fill = "black") +
  theme_minimal()

# can't get to not plot the areas in the legend if you want them with color... sorry
ggplot() +
  # plot all areas but not in the legend
  geom_sf(data = all_areas, alpha = 0.3, show.legend = FALSE) +
  
  # plot PT sites and set fill to Site to generate the legend entry
  geom_sf(data = PT, aes(fill = Site), size = 2.5, shape = 21, show.legend = TRUE) +
  
  # add streams to the map
  geom_sf(data = streams, color = "#5586B3") +
  
  # customize the legend and title
  labs(title = "Santa Fe Watershed - PT Locations", fill = "PT Sites") +
  
  # set color palette for PT site names
  scale_fill_manual(values = pal) +
  
  # minimal theme
  theme_minimal()

library(dplyr)

########################################################
#### PT locations grouped by upper middle and lower ####
########################################################
# Create the grouping column
PT <- PT %>%
  mutate(location = case_when(
    Site %in% c("USF03", "USF04", "USF05", "USF01", "USF02", "USF12", "USF24", "USF25", "USF26") ~ "lower",
    Site %in% c("USF09", "USF10", "USF11", "USF32", "USF33", "USF22", "USF23", "USF07", "USF20") ~ "middle",
    Site %in% c("USF21", "USF14", "USF13", "USF31", "USF15", "USF16", "USF17", "USF18", "USF28", "USF29", "USF19") ~ "upper",
    TRUE ~ NA_character_
  ))

ggplot() +
  # 1. Plot ONLY the specific watershed boundary
  geom_sf(data = all_areas %>% filter(Area_ID == "12"),
          fill = "gray90",       # Light gray fill
          color = "gray50",      # Boundary color
          linewidth = 0.5) +
  
  # 2. Plot streams
  geom_sf(data = streams, 
          color = "#5586B3", 
          linewidth = 0.3) +
  
  # 3. Plot PT sites
  geom_sf(data = PT, 
          aes(fill = location), 
          size = 3, 
          shape = 21, 
          color = "black") +
  
  # Colors for groups
  scale_fill_manual(values = c("upper" = "blue", 
                               "middle" = "green", 
                               "lower" = "red")) +
  
  # Styling
  labs(title = "Santa Fe Watershed", fill = "Location") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(datum = NA)

########################
#### Scan locations ####
########################
# only scan catchments
scan_areas <- all_areas[c("area12", "area20", "area21"),]
# load scan sites
pt <- read.csv("data/NM_scans.csv")

# choose colors for maps. PLAY WITH THIS!
pal <- paletteer_c("ggthemes::Green-Blue-White Diverging", 30)
pal <- paletteer_c("ggthemes::Green", 3)

#transform lat lon to geometries
PT <- st_as_sf(pt, coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

# plot the areas with scan sites
ggplot() +
  geom_sf(data = scan_areas, aes(fill = Area_ID), alpha = 0.4) +
  labs(title = "Santa Fe Watershed - scan locations", fill = "Site") +
  #scale_fill_manual(values = pal) +
  geom_sf(data = streams, color = "#5586B3") +
  geom_sf(data = PT, size = 4, shape = 21, fill = "red") +
  theme_minimal()

# plot with colored areas
ggplot() +
  geom_sf(data = scan_areas, aes(fill = Area_ID), alpha = 0.6) +
  labs(title = "Santa Fe Watershed - scan locations") + 
  scale_fill_manual(name = "Site Area", values = c("12" = "#FDE725", "20" = "#2B833E", "21" = "#440154"), 
                    labels = c("12" = "Lower", "20" = "Middle", "21" = "Upper")) +
  geom_sf(data = streams, color = "#5586B3") +
  geom_sf(data = PT, size = 4, shape = 21, fill = "black") +
  theme_minimal()

# plot with colored dots
ggplot() +
  geom_sf(data = scan_areas) +
  labs(title = "Santa Fe Watershed - scan locations") +
  geom_sf(data = streams, color = "#5586B3") +
  geom_sf(data = PT, aes(fill = Site), size = 4, shape = 21) +
  scale_fill_manual(name = "Site", values = c("USF12" = "#FDE725", "USF20" = "#2B833E", "USF21" = "#440154"), 
                                              labels = c("USF12" = "Lower", "USF20" = "Middle", "USF21" = "Upper")) +
  theme_minimal()

# can I get both colored?
ggplot() +
  geom_sf(data = scan_areas, aes(color = Area_ID), alpha = 0.6) +
  labs(title = "Santa Fe Watershed - scan locations") + 
  scale_color_manual(name = "Site Area", values = c("12" = "#FDE725", "20" = "#2B833E", "21" = "#440154"), 
                    labels = c("12" = "Lower", "20" = "Middle", "21" = "Upper")) +
  geom_sf(data = streams, color = "#5586B3") +
  geom_sf(data = PT, aes(fill = Site), size = 4, shape = 21) +
  scale_fill_manual(name = "Site", values = c("USF12" = "#FDE725", "USF20" = "#2B833E", "USF21" = "#440154"), 
                    labels = c("USF12" = "Lower", "USF20" = "Middle", "USF21" = "Upper")) +
  theme_minimal()

##########################
#### Will's locations ####
##########################
# load leverage data
pt <- read.csv("data/forwill.csv")

# choose colors for maps. PLAY WITH THIS!
pal <- paletteer_c("ggthemes::Green-Blue-White Diverging", 30)
pal <- paletteer_c("ggthemes::Green", 43)

# transform lat lon to geometries
PT <- st_as_sf(pt, coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')

# plot all sites
library(ggspatial)

usf12 <- PT[5,]

ggplot() +
  # 1. Background
  geom_sf(data = all_areas, color = NA, alpha = 0.2) +
  
  # 2. USF12 in Green (Eco Green: #4DAF4A)
  geom_sf(data = all_areas %>% filter(Area_ID == "USF12"), 
          fill = "#4DAF4A", color = "black", linewidth = 0.7) +
  
  geom_sf(data = streams, color = "#5586B3") +
  #geom_sf(data = PT, size = 2, shape = 21, fill = "black") +
  geom_sf(data = usf12, size = 2, shape = 21, fill = "black") +
  
  # 3. Add Scale Bar
  # 'location' options: "tl", "tr", "bl", "br" (top-left, bottom-right, etc.)
  annotation_scale(location = "br", width_hint = 0.4) +
  
  # 4. Add North Arrow (Optional but recommended)
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.title = element_blank()) +
  coord_sf(datum = NA)

# can't get to not plot the areas in the legend if you want them with color... sorry
ggplot() +
  # plot all areas but not in the legend
  geom_sf(data = all_areas, alpha = 0.3, show.legend = FALSE) +
  
  # plot PT sites and set fill to Site to generate the legend entry
  geom_sf(data = PT, aes(fill = Site), size = 2.5, shape = 21, show.legend = TRUE) +
  
  # add streams to the map
  geom_sf(data = streams, color = "#5586B3") +
  
  # customize the legend and title
  labs(title = "Santa Fe Watershed - PT Locations", fill = "PT Sites") +
  
  # set color palette for PT site names
  # scale_fill_manual(values = pal) +
  
  # minimal theme
  theme_minimal()


