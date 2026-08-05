##==============================================================================
## Project: QuEST
## 01_delineation_annotated.R
##
## Script to delineate subcatchments from a DEM using whitebox tools.
##
## WHY THIS SCRIPT LOOKS DIFFERENT FROM THE ORIGINAL 01_delineation.R
## -----------------------------------------------------------------
## The original script was written to delineate ONE subcatchment at a time:
## you'd hand-edit data/outlet.csv to have a single row (site name + lat/lon),
## run the whole script top to bottom, look at the mapview() checks to see if
## the pour point snapped onto the right stream, nudge the lat/lon if it
## didn't, and re-run. That's why the script hard-codes "newmex" in every file
## name - it was always being edited in place for whichever site was being
## delineated that day, and the values for DEM resolution (z) and stream
## threshold got overwritten every time. Nothing about *why* a given z or
## threshold was chosen for a given watershed was ever saved anywhere.
##
## This version keeps the exact same delineation logic (same whitebox steps,
## same two-pass "delineate once, crop, delineate again on the cropped DEM"
## approach) but wraps it in a function and drives it from two small tables
## instead of hand-editing values:
##   1. data/outlet.csv          - one row per subcatchment: watershed, site,
##                                  lat, lon (this is the file you'll still
##                                  edit/add rows to as you snap points)
##   2. data/watershed_params.csv - one row per watershed: DEM resolution (z),
##                                  stream extraction threshold, snap distance
##
## That way the parameters that actually varied by watershed are documented
## in one place instead of living only in whatever was last typed into the
## function call.
##
## ON set.seed()
## --------------
## Nothing in this pipeline is stochastic - get_elev_raster(), the whitebox
## D8 flow routing, stream extraction, and watershed delineation are all
## deterministic given the same DEM, resolution, threshold, and pour point.
## So set.seed() doesn't do anything useful here. The thing that actually
## makes this reproducible is recording the FINAL snapped lat/lon you ended
## up using for each site (not the first guess), plus the z/threshold/snap_dist
## you ran it with. That's what watershed_params.csv and the "snapped_lat" /
## "snapped_lon" columns this script writes back out are for.
##==============================================================================

##############
## PACKAGES ##
##############
library(tidyverse)
library(dplyr)
library(raster)
library(sf)
library(sp)
library(elevatr)
library(mapview)
library(stars)
library(vroom)
library(whitebox)
library(tmaptools)

###################
#### Load data ####
###################

## data/outlet.csv - one row per subcatchment
## required columns: Watershed, Site, Lat, Lon
## (Watershed is the short code used for the areas_<code> folder, e.g. NM, NH, SS, DV, BR)
outlet_all <- read_csv("data/outlet.csv")

## data/watershed_params.csv - one row per watershed
## required columns: Watershed, z, threshold, snap_dist, expand
## see README_delineation_workflow.md for how these were chosen
params <- read_csv("data/watershed_params.csv")

## whitebox needs an absolute working directory (it doesn't respect the R
## project directory), so set that here once
wbt_wd <- getwd()

##########################################
#### Function: delineate one subcatchment ####
##########################################
# This is the same series of steps as the original script:
# 1.1 write DEM and outlet to temp
# 1.2 fill single-cell pits
# 1.3 breach depressions
# 1.4 flow direction
# 1.5 flow accumulation
# 1.5.2 stream layer
# 1.6 snap pour point
# 1.7 delineate watershed
# 1.8 read back into R
# 1.9 convert to polygon
# then repeats 1.2-1.9 on the CROPPED dem (this second pass produces the
# final, cleaner stream network that gets saved as area_stream_network.shp)
delineate_subcatchment <- function(site, lat, lon, watershed,
                                    z, threshold, snap_dist, expand = 17000,
                                    check_maps = TRUE) {

  message(sprintf("---- Delineating %s (watershed %s) ----", site, watershed))
  message(sprintf("z = %s | threshold = %s | snap_dist = %s | expand = %s",
                   z, threshold, snap_dist, expand))

  # output folder for this subcatchment, matching the existing areas_<code>/<site> layout
  out_dir <- file.path(paste0("areas_", watershed), site)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # clear temp scratch space so runs don't leak into each other
  invisible(file.remove(list.files("temp", full.names = TRUE)))

  ###################################
  #### build outlet point + pull DEM ####
  ###################################
  outlet <- tibble(Site = site, Lat = lat, Lon = lon) %>%
    st_as_sf(coords = c("Lon", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs') %>%
    st_transform(crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>%
    st_geometry()

  pour <- as_Spatial(outlet)
  pour_sf <- st_as_sf(pour)

  # z = DEM zoom/resolution level for get_elev_raster (AWS terrain tiles).
  # Higher z = finer resolution = more detail, but slower and more likely to
  # need a coarser stream threshold to avoid an unusably dense stream network.
  # This is exactly the "small watershed needs finer z, but that same z is too
  # noisy for a big watershed" tradeoff you ran into - it's now set per
  # watershed in watershed_params.csv instead of by memory.
  dem <- get_elev_raster(pour_sf, z = z, clip = "bbox", expand = expand)
  message(sprintf("Actual DEM resolution pulled: %.1f x %.1f m",
                   res(dem)[1], res(dem)[2]))

  if (check_maps) {
    plot(dem)
  }

  writeRaster(dem, "temp/dem.tif", overwrite = TRUE)
  st_write(outlet, "temp/outlet.shp", delete_layer = TRUE, quiet = TRUE)

  ############################
  ## PREP DEM AND DELINEATE (pass 1 - full-extent DEM) ##
  ############################
  wbt_fill_single_cell_pits(dem = "temp/dem.tif", output = "temp/dem_fill.tif", wd = wbt_wd)
  wbt_breach_depressions(dem = "temp/dem_fill.tif", output = "temp/dem_breach.tif", wd = wbt_wd)
  wbt_d8_pointer(dem = "temp/dem_breach.tif", output = "temp/flowdir.tif", wd = wbt_wd)
  wbt_d8_flow_accumulation(input = "temp/dem_breach.tif", output = "temp/flowaccum.tif", wd = wbt_wd)

  # snap the pour point to the nearest flow-accumulation cell within snap_dist
  wbt_snap_pour_points(
    pour_pts = "temp/outlet.shp",
    flow_accum = "temp/flowaccum.tif",
    snap_dist = snap_dist,
    output = "temp/snap_pour.shp",
    wd = wbt_wd
  )

  ## +++ CRITICAL MANUAL CHECK +++
  ## The pour point MUST land on the correct flow-accumulation stream, or the
  ## watershed below will delineate onto the wrong drainage (or not at all).
  ## snap_dist only pulls the point to the NEAREST stream cell - if the
  ## nearest stream is the wrong one, snapping won't fix that; you have to
  ## nudge the input lat/lon in data/outlet.csv itself and re-run.
  ## This is the manual, iterative part of the workflow that doesn't have a
  ## way around it: look at the mapview() below, check the red snapped point
  ## against the blue stream lines, and if it's on the wrong branch, adjust
  ## the lat/lon and re-run before going further.
  wbt_extract_streams(
    flow_accum = "temp/flowaccum.tif",
    output = "temp/streams.tif",
    threshold = threshold,
    wd = wbt_wd
  )
  wbt_raster_streams_to_vector(
    streams = "temp/streams.tif",
    d8_pntr = "temp/flowdir.tif",
    output = "temp/streams.shp",
    wd = wbt_wd
  )

  streams <- st_read("temp/streams.shp", quiet = TRUE)
  st_crs(streams) <- st_crs(dem)

  pour_pt_snap <- st_read("temp/snap_pour.shp", quiet = TRUE)
  st_crs(pour_pt_snap) <- st_crs(dem)

  if (check_maps) {
    print(mapview(dem) + mapview(streams) + mapview(pour_sf) + mapview(pour_pt_snap, color = "red"))
  }

  # record where the point actually snapped to, in lat/lon, for the log
  snapped_latlon <- pour_pt_snap %>%
    st_transform(crs = '+proj=longlat +datum=WGS84 +no_defs') %>%
    st_coordinates()

  ###################
  ## delineate (pass 1) ##
  ###################
  wbt_watershed(
    d8_pntr = "temp/flowdir.tif",
    pour_pts = "temp/snap_pour.shp",
    output = "temp/shed.tif",
    wd = wbt_wd
  )

  ws <- raster("temp/shed.tif") %>%
    st_as_stars() %>%
    st_as_sf(merge = TRUE)

  st_write(ws, file.path(out_dir, "site.shp"), delete_layer = TRUE, quiet = TRUE)

  ################################
  ## Crop the DEM and run again (pass 2 - this is what gets saved as the final output) ##
  ################################
  crop_extent <- st_read(file.path(out_dir, "site.shp"), quiet = TRUE)
  cropped_dem <- raster::crop(dem, crop_extent)
  writeRaster(cropped_dem, "temp/cropped_dem.tif", overwrite = TRUE)

  wbt_fill_single_cell_pits(dem = "temp/cropped_dem.tif", output = "temp/cropped_dem_fill.tif", wd = wbt_wd)
  wbt_breach_depressions(dem = "temp/cropped_dem_fill.tif", output = "temp/cropped_dem_breach.tif", wd = wbt_wd)
  wbt_d8_pointer(dem = "temp/cropped_dem_breach.tif", output = "temp/cropped_flowdir.tif", wd = wbt_wd)
  wbt_d8_flow_accumulation(input = "temp/cropped_dem_breach.tif", output = "temp/cropped_flowaccum.tif", wd = wbt_wd)

  wbt_extract_streams(
    flow_accum = "temp/cropped_flowaccum.tif",
    output = "temp/cropped_streams.tif",
    threshold = threshold,
    wd = wbt_wd
  )
  wbt_raster_streams_to_vector(
    streams = "temp/cropped_streams.tif",
    d8_pntr = "temp/cropped_flowdir.tif",
    output = "temp/cropped_streams.shp",
    wd = wbt_wd
  )

  streams_final <- st_read("temp/cropped_streams.shp", quiet = TRUE)
  st_crs(streams_final) <- st_crs(cropped_dem)
  streams_final <- streams_final[ws, ]

  if (check_maps) {
    print(mapview(dem, maxpixels = 742182) + mapview(ws) + mapview(streams_final) + mapview(pour_sf))
  }

  #### save final outputs (same layout as the original script) ####
  st_write(ws, file.path(out_dir, "area.shp"), delete_layer = TRUE, quiet = TRUE)
  st_write(streams_final, file.path(out_dir, "area_stream_network.shp"), delete_layer = TRUE, quiet = TRUE)
  writeRaster(cropped_dem, file.path(out_dir, "croppedDEM_area.tif"), overwrite = TRUE)

  area_m2 <- as.numeric(sum(st_area(ws)))
  message(sprintf("Delineated area: %.0f m2", area_m2))

  # return a one-row log entry so the calling loop can build a run record
  tibble(
    Site = site,
    Watershed = watershed,
    input_lat = lat,
    input_lon = lon,
    snapped_lat = snapped_latlon[1, "Y"],
    snapped_lon = snapped_latlon[1, "X"],
    z = z,
    threshold = threshold,
    snap_dist = snap_dist,
    expand = expand,
    dem_res_m = res(dem)[1],
    area_m2 = area_m2,
    run_date = as.character(Sys.Date())
  )
}

########################################
#### Run for every row in outlet.csv ####
########################################
# join each subcatchment to its watershed's parameters, then delineate one at
# a time. Because the pour point snapping has to be checked visually, this is
# still meant to be run and reviewed site-by-site (set check_maps = FALSE to
# skip the mapview() calls once you trust a batch of points and just want the
# outputs regenerated).

run_plan <- outlet_all %>%
  left_join(params, by = "Watershed")

# sanity check: make sure every row actually matched a watershed's params
missing_params <- run_plan %>% filter(is.na(z))
if (nrow(missing_params) > 0) {
  stop("These sites have no matching row in watershed_params.csv: ",
       paste(missing_params$Site, collapse = ", "))
}

run_log <- purrr::pmap_dfr(run_plan, function(Site, Lat, Lon, Watershed, z, threshold, snap_dist, expand) {
  delineate_subcatchment(
    site = Site, lat = Lat, lon = Lon, watershed = Watershed,
    z = z, threshold = threshold, snap_dist = snap_dist, expand = expand,
    check_maps = TRUE
  )
})

# append this run's log to the running record of what was actually used,
# rather than overwriting it - this is the reproducibility record for the paper
log_path <- "data/delineation_run_log.csv"
if (file.exists(log_path)) {
  write_csv(run_log, log_path, append = TRUE)
} else {
  write_csv(run_log, log_path)
}
