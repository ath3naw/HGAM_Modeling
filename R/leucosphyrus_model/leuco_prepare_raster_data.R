rm(list=ls())
library(tidyverse)
library(terra)
library(geodata)
par(mfrow=c(1,1))
# part 1

# get extent map for our basic analyses
leuco_dist_map <- gadm(
  country = c("THA", "MM", "IN", "KHM", "IDN", "VN", "MYS","LA", "CN", "PH", "NPL", "BTN", "BD"),
  level = 0,
  path = "data/downloads"
) # slow to run though not large

plot(leuco_dist_map)

# let's download some data

# bioclimactic variables from worldclim
# https://worldclim.org/data/bioclim.html
# bio <- worldclim_global(var = "bio", res = 0.5, path = "data/downloads")


library(terra)

# Path to the folder with the .tif files
bioclim_path <- "grids"

# Stack all 19 .tif files
bioclim_stack <- rast(file.path(bioclim_path, paste0("wc2.1_30s_bio_", 1:19, ".tif")))

# crop to the southeast asia area
seasia_ext <- ext(60, 150, -15, 55)  # adjust as needed

bioclim_leuco_map <- crop(bioclim_stack, seasia_ext)
plot(bioclim_leuco_map[[1]])


# generate mask of our area of interest so we can use it to process other data into this shape
leuco_map_mask <- bioclim_leuco_map[[1]] %>%
  mask(leuco_dist_map) * 0 + 1

plot(leuco_map_mask)


# process bioclim into shape
bc_leuco_map <- mask(
  x = bioclim_leuco_map,
  mask = leuco_map_mask
)

# travel time from city to cell
# you can explore the sizes of city using ?travel_time, and change size if you wish
travel <- travel_time(
  to = "city",
  size = 2,
  up=TRUE,
  path = "data"
)

# it's a good idea to have a look at your objects and even plot them
travel
plot(travel)


# process travel data, can change as needed
# this is a very large raster, we speed up the operation by cropping then masking
travel_leuco_map <- crop(
  x = travel,
  y = leuco_map_mask
) %>%
  mask(
    mask = leuco_map_mask
  )

# let's have a look at it

plot(travel_leuco_map)


# now let's create a bias layer out of our travel data
# it should scale from 0-1 where 0 is hard to get to and 1 is easy
rescale_travel <- travel_leuco_map

rescale_travel[] <- rescale_travel[]/ max(rescale_travel[], na.rm = TRUE)

rescale_travel[] <- 1 - rescale_travel[]

plot(rescale_travel)



# now let's write our key rasters to disk
# NB the above download functions will only need to download once;
# once those objects are saved in that location, they will be re-read from disk

# write rasters to save
terra::writeRaster(
  x = leuco_map_mask,
  filename = "data/grids/leuco_map_mask.tif",
  overwrite=TRUE
)

terra::writeRaster(
  x = bc_leuco_map,
  filename = "data/grids/bc_leuco_map.tif",
  overwrite=TRUE
)

terra::writeRaster(
  x = rescale_travel,
  filename = "data/grids/leuco_rescale_travel.tif"
)

# write in the maps
leuco_map_mask <- terra::rast("data/grids/leuco_map_mask.tif")
bc_leuco_map <- terra::rast("data/grids/bc_leuco_map.tif")

# inspect them
plot(bc_leuco_map)
plot(leuco_map_mask)

# Specify covariates used for the model (only temp and prec)
# BIO1 - Annual Mean Temperature
ttemp <- bc_leuco_map[[1]]
# BIO12 = Annual Precipitation
pprec <- bc_leuco_map[[12]]

covs <- c(ttemp, pprec)
names(covs) <- c("ttemp", "pprec")

# save covariates
writeRaster(covs,
            "data/grids/covs_hgam.tif",
            overwrite = TRUE)

