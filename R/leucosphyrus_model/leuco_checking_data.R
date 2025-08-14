rm(list = ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
library(mgcv)
library(sp)
source("R/leucosphyrus_model/leuco_functions.R")
# part 3

# read in data
leuco_data <- read_csv("leuco_data/Leucosphyrus_Data.csv")

# filter columns
leuco_data <- leuco_data |> select(-any_of(c(
  "DNA/No DNA",
  "Coordinate system",
  "Coordinates Inferred",
  "Geographical\nregion",
  "GenBank\naccession\nnumbers",
  "Source",
  "Geographical\nregion"
)))

# rename columns to make sense
leuco_data <- leuco_data |> rename(lon_ddm="DDM (Lon)",
                                   lat_ddm="DDM (Lat)", 
                                   lat2=Latitude,
                                   lon2=longitude,
                                   lon= "Longitude (GPS)",
                                   lat= "Latitude (GPS)")

# divide based on these characters
chd <- "°"
chm <- "'"
chs <- "\""
# fix missing data
leuco_data$lon_ddm <- fix_missing(leuco_data$lon_ddm)
leuco_data$lat_ddm <- fix_missing(leuco_data$lat_ddm)
# convert to decimal degree coords
leuco_data$lon_ddm <- sapply(leuco_data$lon_ddm, function(x) {
  if (is.na(x)) {
    NA_real_
  } else {
    as.numeric(char2dms(x, chd = chd, chm = chm, chs = chs))
  }
})
leuco_data$lat_ddm <- sapply(leuco_data$lat_ddm, function(x) {
  if (is.na(x)) {
    NA_real_
  } else {
    as.numeric(char2dms(x, chd = chd, chm = chm, chs = chs))
  }
})
# apply the functions to take average of coordinates if there is a range
leuco_data$lat2 <- sapply(leuco_data$lat2, average_coords)
leuco_data$lon2 <- sapply(leuco_data$lon2, average_coords)
leuco_data$lon <- sapply(leuco_data$lon, clean_coords)
leuco_data$lat <- sapply(leuco_data$lat, clean_coords)
leuco_data$lon <- as.numeric(leuco_data$lon)
leuco_data$lat <- as.numeric(leuco_data$lat)

# combine all lon and lat columns
leuco_data <- leuco_data %>%
  mutate(lonx = coalesce(lon_ddm, lon2, lon), laty = coalesce(lat_ddm, lat2, lat)) %>%
  select(-c(lon_ddm,lon2,lon,lat_ddm,lat2,lat))

# remove all the data for which there are no coords
leuco_data <- leuco_data[!is.na(leuco_data$lonx),]
# convert to presence-absence
leuco_data$Number <- case_when(
  leuco_data$Number == "Found" ~ 1,
  leuco_data$Number=="Nothing" ~ 0,
  !is.na(as.numeric(leuco_data$Number)) & as.numeric(leuco_data$Number)>0 ~ 1,
  TRUE ~ 0
)
# make sure they all are the same "species", no repeats
leuco_data$Species <- str_to_title(leuco_data$Species)
leuco_data$Species[leuco_data$Species=="Leucosphyrus Group"] <- "Leucosphyrus Subgroup"

# adjust mapping data to inspect collected data
library(geodata)
# world map
wrld <- world(path=".")
plot(wrld, xlim=c(50, 155), ylim=c(-20, 40), col="light yellow", border="light gray")
points(leuco_data$lonx, leuco_data$laty, col="red", pch=20)
# convert them to points
leuco_vector <- vect(leuco_data, geom=c("lonx", "laty"), crs="+proj=longlat +datum=WGS84")
mis_coords <- extract(wrld, leuco_vector)
# ones where there are no countries associated with points
cntr <- mis_coords$NAME_0
i <- which(is.na(cntr))
i
plot(wrld, xlim=c(50, 155), ylim=c(-20, 40), col="light yellow", border="light gray")
# plot the ones that are missing
points(leuco_data$lonx[i], leuco_data$laty[i], col="red", pch=20)
off <- cbind(mis_coords[i,], leuco_vector$Country[i])

# some border lines are different
# is just a little off, should be an island
# which ones are off the country that it's listed as
j <- which(cntr != leuco_vector$Country)
j
m <- cbind(cntr[j], leuco_vector$Country[j])
colnames(m) <- c("polygons", "data")
m

# plotting coordinates that don't match
plot(wrld, xlim=c(50, 155), ylim=c(-20, 40), col="light yellow", border="light gray")
points(leuco_vector[j[1:2]])
# India data doesn't really make sense, remove it
leuco_data <- leuco_data[-(39:42),]

# again change to title case
leuco_data$Species <- str_to_title(leuco_data$Species)

# prepare data and convert to same format as model data
pa_tabular <- leuco_data %>%
  group_by(lonx, laty) %>%
  mutate(site_id = cur_group_id()) %>%
  ungroup() %>%
  select(site_id, lonx, laty, Species, Number, Collection) |>
  mutate(not_complex = ifelse(Species %in% c("Leucosphyrus Subgroup", "Dirus Complex", "Leucosphyrus Group"), 0, 1)) |>
  mutate(dirus = ifelse(Species %in% c("Dirus Complex", "Anopheles Dirus", "Anopheles Cracens", "Anopheles Scanloni", "Anopheles Baimaii", "Anopheles Elegans", "Anopheles Takasagoensis", "Anopheles Nemophilous"), 1, 0)) |>
  mutate(leucosphyrus = ifelse(Species %in% c("Leucosphyrus Complex", "Anopheles Leucosphyrus", "Anopheles Latens", "Anopheles Introlatus", "Anopheles Balabacensis", "Anopheles Baisasi"), 1, 0))

methods <- read_csv("leuco_data/sample_methods.csv")
pa_tabular$Collection <- tolower(pa_tabular$Collection)

# add on standardized Collection data in model data format
methods_df <- get_methods_df(pa_tabular$Collection)
pa_tabular <- cbind(pa_tabular, methods_df)
pa_tabular <- pa_tabular |>
  select(-Collection)

# clean collection method data to be standardized
pa_tabular$method1[pa_tabular$method1 %in% c("outdoor human leg catch", "outdoor human landing catches", 
                                                   "human landing catch cultivation sites", "outdoor human-landing catch",
                                                   "outdoor human landing catch", "outdoor bare-leg capture")] <- "man biting outdoors"
pa_tabular$method1[pa_tabular$method1 %in% c("human landing catch house","indoor human leg catch")] <- "man biting indoors"
pa_tabular$method1[pa_tabular$method1 %in% c("human landing catch","human-landing catch")] <- "man biting"
pa_tabular$method1[grepl("mosquito magnet", pa_tabular$method1)] <- "carbon dioxide baited net trap"
pa_tabular$method1[pa_tabular$method1 %in% c("human-baited double net traps (hbnt)", "human baited trap", 
                                             "human-baited electrocuting net")] <- "human baited net trap"
pa_tabular$method1[pa_tabular$method1 %in% c("cattle baited net trap collections", "monkey-baited trap", 
                                             "monkey-baited electrocuting net")] <- "animal baited net trap"
pa_tabular$method1[grepl("light trap", pa_tabular$method1)] <- "light trap"
pa_tabular$method2[grepl("mosquito magnet", pa_tabular$method2)] <- "carbon dioxide baited net trap"
pa_tabular$method2[grepl("light trap", pa_tabular$method2)] <- "light trap"
pa_tabular$method2[pa_tabular$method2 %in% c("cattle bait collection", "cow-bait trap",
                                             "cow bait collection")] <- "animal baited net trap"
pa_tabular$method2[pa_tabular$method2 %in% c("human-baited double net traps (hbnt)", "human baited trap", 
                                             "human-baited electrocuting net")] <- "human baited net trap"
pa_tabular$method2[pa_tabular$method2 %in% c("outdoor resting catch")] <- "resting outdoors"
pa_tabular$method3[grepl("light trap", pa_tabular$method3)] <- "light trap"
pa_tabular$method3[pa_tabular$method3 %in% c("human-baited double net traps (hbnt)", "human baited trap", 
                                             "human-baited electrocuting net")] <- "human baited net trap"
pa_tabular$method4[grepl("mosquito magnet", pa_tabular$method4)] <- "carbon dioxide baited net trap"

# find max number of sites collected
max_id <- max(pa_tabular$site_id)
# read in leuco data (po points)
asia_vec_data <- read_csv("leuco_data/asia_vec_data.csv")
# convert to tabular format
pa_tabular2 <- asia_vec_data %>%
  group_by(lonx, laty) %>%
  mutate(site_id = cur_group_id()+max_id, Number = 1) %>%
  ungroup() %>%
  select(site_id, lonx, laty, Species, Number) |>
  mutate(not_complex = ifelse(Species %in% c("Leucosphyrus Subgroup", "Dirus Complex", "Leucosphyrus Group"), 0, 1)) |>
  mutate(dirus = ifelse(Species %in% c("Dirus Complex", "Anopheles Dirus", "Anopheles Cracens", "Anopheles Scanloni", "Anopheles Baimaii", "Anopheles Elegans", "Anopheles Takasagoensis", "Anopheles Nemophilous"), 1, 0)) |>
  mutate(leucosphyrus = ifelse(Species %in% c("Leucosphyrus Complex", "Anopheles Leucosphyrus", "Anopheles Latens", "Anopheles Introlatus", "Anopheles Balabacensis", "Anopheles Baisasi"), 1, 0))

#could do this or not, depending on if you want po data in
#pa_tabular <- rbind(pa_tabular,pa_tabular2)

# convert the headings, etc. to be more readable
pa_tabular <- pa_tabular %>% rename(pa = Number, sp = Species)
covs_vals <- extract(x=covs, y=pa_tabular |> dplyr::select(lonx, laty))

# add to make model data for presence-absence, grouping based on site id, species, and pa
pa_model_data <- cbind(pa_tabular, covs_vals) |> 
  dplyr::select(-ID)
pa_model_data <- pa_model_data %>%
  distinct(site_id, sp, pa, .keep_all = TRUE)
pa_model_data$sp <- as.factor(pa_model_data$sp)

# save it
write.csv(pa_model_data,
          file = "leuco_data/hgam_leuco_data_nobias.csv",
          row.names = FALSE)

