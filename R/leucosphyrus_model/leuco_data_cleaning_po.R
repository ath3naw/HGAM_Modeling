rm(list=ls())
library(malariaAtlas)
library(stringr)
library(tidyr)
library(tidyverse)
library(gratia)
library(dplyr)

# part 2 if you want to include po coords
# read in data
asia_vec_data <- getVecOcc(continent = "Asia")
unique(asia_vec_data$species_plain)
asia_vec_data$species_plain <- str_to_title(asia_vec_data$species_plain)
mos <- paste("Anopheles", c("Leucosphyrus", "Latens", "Introlatus", "Balabacensis", "Baisasi", "Dirus", "Cracens",
                            "Scanloni", "Baimaii", "Elegans", "Takasagoensis", "Nemophilous", "Mirans", "Hackeri",
                            "Pujutensis", "Recens", "Sulawesi", "Riparis", "Cristatus", "Macarthuri"))
asia_vec_data <- asia_vec_data[asia_vec_data$species_plain %in% mos,]
asia_vec_data <- as.data.frame(asia_vec_data) |> select(latitude, longitude, country, species_plain, id_method1, id_method2, sample_method1, sample_method2)

library(geodata)
wrld <- world(path=".")
plot(wrld, xlim=c(50, 155), ylim=c(-20, 40), col="light yellow", border="light gray")
points(asia_vec_data$longitude, asia_vec_data$latitude, col="red", pch=20)
leuco_vector <- vect(asia_vec_data, geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
mis_coords <- extract(wrld, leuco_vector)
cntr <- mis_coords$NAME_0
i <- which(is.na(cntr))
i
plot(wrld, xlim=c(50, 155), ylim=c(-20, 40), col="light yellow", border="light gray")
points(asia_vec_data$longitude[i], asia_vec_data$latitude[i], col="red", pch=20)
off <- cbind(mis_coords[i,], asia_vec_data$country[i])
off
# ok, the things that are off are fine, borders are just not that accurate

# ok so these are presence-only points
asia_vec_data <- asia_vec_data |> rename(lonx=longitude, laty=latitude, Species=species_plain)
write.csv(asia_vec_data,
          file = "data/tabular/asia_vec_data.csv",
          row.names = FALSE)
