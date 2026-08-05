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


folder_names <- c("Brush Creek", "Santa Fe", "Dog Valley", "Lamprey", "South Sandy")

shp_list <- list()

for (folder in folder_names) {
  base_path <- file.path("StreamOrder_shps", folder)
  
  area_path   <- file.path(base_path, "area.shp")
  stream_path <- file.path(base_path, "area_stream_network.shp")
  
  if (file.exists(area_path) & file.exists(stream_path)) {
    shp_list[[folder]] <- list(
      area   = st_read(area_path, quiet = TRUE),
      stream = st_read(stream_path, quiet = TRUE)
    )
  }
}

site <- "Brush Creek"
site <- "Dog Valley"
site <- "Lamprey"
site <- "Santa Fe"
site <- "South Sandy"

ggplot() +
  geom_sf(data = shp_list[[site]]$area,
          fill = "grey85", color = "black") +
  geom_sf(data = shp_list[[site]]$stream,
          color = "blue", linewidth = 0.6) +
  ggtitle(site) +
  theme_minimal()


