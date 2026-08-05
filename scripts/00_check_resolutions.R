##==============================================================================
## Project: QuEST
## 00_check_resolutions.R
##
## PURPOSE
## Delineation script (01_delineation.R) got reused/overwritten every time
## I ran a new subcatchment, so the DEM resolution (the `z` argument to
## get_elev_raster) and stream-extraction threshold that were actually used for
## each past subcatchment aren't preserved anywhere in the script history.
##
## BUT: every subcatchment folder (areas_NM/*, areas_NH/*, areas_SS/*,
## areas_DV/*, areas_BR/*) still has a saved `croppedDEM_area.tif` - the actual
## raster you delineated on. The pixel size of that raster tells us exactly
## what DEM resolution was in effect for that specific run, even though the
## threshold value itself isn't recoverable (it's not stored in the raster).
##
## This script loops through every saved croppedDEM_area.tif in the repo and
## reports its resolution (in meters, since the DEMs are in UTM), so we can
## see empirically what resolution you actually used per watershed.
##
## Run this locally, from the root of the delineation repo.
##==============================================================================

library(terra)
library(dplyr)
library(stringr)

# find every croppedDEM_area.tif in every areas_* folder
dem_files <- list.files(
  path = ".",
  pattern = "^croppedDEM_area\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

# pull out watershed code and subcatchment id from the path
# e.g. "./areas_NM/USF11/croppedDEM_area.tif" -> watershed = "NM", subcatchment = "USF11"
res_table <- lapply(dem_files, function(f) {
  r <- rast(f)
  parts <- str_split(f, "/")[[1]]
  data.frame(
    watershed    = str_remove(parts[2], "^areas_"),
    subcatchment = parts[3],
    res_x_m      = res(r)[1],
    res_y_m      = res(r)[2],
    ncell        = ncell(r),
    crs_units    = linearUnits(r),
    path         = f
  )
}) %>% bind_rows()

# summarize by watershed: what resolution(s) actually show up
summary_table <- res_table %>%
  group_by(watershed) %>%
  summarize(
    n_subcatchments   = n(),
    min_res_m         = min(res_x_m),
    max_res_m         = max(res_x_m),
    most_common_res_m = as.numeric(names(sort(table(round(res_x_m, 1)), decreasing = TRUE))[1]),
    .groups = "drop"
  )

print(summary_table)

# write both the full per-subcatchment table and the per-watershed summary
# to csv so you can drop the real numbers into watershed_params.csv
write.csv(res_table, "data/resolution_by_subcatchment.csv", row.names = FALSE)
write.csv(summary_table, "data/resolution_by_watershed.csv", row.names = FALSE)

