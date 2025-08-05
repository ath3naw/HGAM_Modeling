rm(list=ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
source("R/hgam_simplified/functions.R")

# Load in variables
par(mfrow = c(1,1))
prob_pres <- terra::rast("data/grids/spec_prob_pres_hgam.tif")
covs <- terra::rast("data/grids/covariates.tif")
mad_mask <- terra::rast("data/grids/mad_mask.tif")
bc_mad <- terra::rast("data/grids/bc_mad.tif")
bias <- terra::rast("data/grids/bias.tif")
# bias is probability of presence of "rarest" species
means <- global(prob_pres, "mean", na.rm = TRUE)[, 1]
i <- which.min(means)
sp_bias <- prob_pres[[i]]
writeRaster(sp_bias,
            "data/grids/sp_bias.tif",
            overwrite = TRUE)
all_bias <- bias*sp_bias

# defining number of species
n_sp <- nlyr(prob_pres)
# checking bias
plot(bias)
plot(sp_bias)
## Now for simulation of the data
# presence-absence data *********************************************************
# Unbiased #####################################################################
# Medium data ##################################################################
n_samples <- 300
pa_tab <- generate_data_tabular(n_samples, mad_mask, prob_pres=prob_pres)
write.csv(pa_tab,
          file = "data/tabular/hgam_pa_tab_data_med.csv",
          row.names = FALSE)

# checking presence and absence data
plot(prob_pres[[1]], range=c(0,1))
points(pa_tab$x, pa_tab$y, pch=21, bg=pa_tab$a==1)
plot(prob_pres[[2]], range=c(0,1))
points(pa_tab$x, pa_tab$y, pch=21, bg=pa_tab$b==1)

## 5/6 complex -----------------------------------------------------------------
n_cp <- round(n_samples*5/6)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)
pa_model_data

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_med_nobias_56.csv",
          row.names = FALSE)

## 2/3 complex -----------------------------------------------------------------
n_cp <- round(n_samples*2/3)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_med_nobias_23.csv",
          row.names = FALSE)


## 1/3 complex -----------------------------------------------------------------
n_cp <- round(n_samples*1/3)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_med_nobias_13.csv",
          row.names = FALSE)

## 1/6 complex -----------------------------------------------------------------
n_cp <- round(n_samples*1/6)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_med_nobias_16.csv",
          row.names = FALSE)


# Large amount of data #########################################################
n_samples <- 900
pa_tab <- generate_data_tabular(n_samples, mad_mask, prob_pres=prob_pres)
write.csv(pa_tab,
          file = "data/tabular/hgam_pa_tab_data_max.csv",
          row.names = FALSE)

# checking presence and absence data
plot(prob_pres[[1]], range=c(0,1))
points(pa_tab$x, pa_tab$y, pch=21, bg=pa_tab$a==1)
plot(prob_pres[[2]], range=c(0,1))
points(pa_tab$x, pa_tab$y, pch=21, bg=pa_tab$b==1)

## 5/6 complex -----------------------------------------------------------------
n_cp <- round(n_samples*5/6)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)
pa_model_data

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_max_nobias_56.csv",
          row.names = FALSE)

## 2/3 complex -----------------------------------------------------------------
n_cp <- round(n_samples*2/3)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_max_nobias_23.csv",
          row.names = FALSE)


## 1/3 complex -----------------------------------------------------------------
n_cp <- round(n_samples*1/3)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_max_nobias_13.csv",
          row.names = FALSE)

## 1/6 complex -----------------------------------------------------------------
n_cp <- round(n_samples*1/6)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_max_nobias_16.csv",
          row.names = FALSE)

# Small amount of data #########################################################
n_samples <- 100
pa_tab <- generate_data_tabular(n_samples, mad_mask, prob_pres=prob_pres)
write.csv(pa_tab,
          file = "data/tabular/hgam_pa_tab_data_min.csv",
          row.names = FALSE)

# checking presence and absence data
plot(prob_pres[[1]], range=c(0,1))
points(pa_tab$x, pa_tab$y, pch=21, bg=pa_tab$a==1)
plot(prob_pres[[2]], range=c(0,1))
points(pa_tab$x, pa_tab$y, pch=21, bg=pa_tab$b==1)

## 5/6 complex -----------------------------------------------------------------
n_cp <- round(n_samples*5/6)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)
pa_model_data

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_min_nobias_56.csv",
          row.names = FALSE)

## 2/3 complex -----------------------------------------------------------------
n_cp <- round(n_samples*2/3)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_min_nobias_23.csv",
          row.names = FALSE)


## 1/3 complex -----------------------------------------------------------------
n_cp <- round(n_samples*1/3)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_min_nobias_13.csv",
          row.names = FALSE)

## 1/6 complex -----------------------------------------------------------------
n_cp <- round(n_samples*1/6)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_min_nobias_16.csv",
          row.names = FALSE)

# Biased #######################################################################
# with bias, travel time #######################################################
n_samples <- 300
pa_tab <- generate_data_tabular(n_samples, bias, prob_pres=prob_pres, weighted=TRUE)
write.csv(pa_tab,
          file = "data/tabular/hgam_pa_tab_data_med_biased.csv",
          row.names = FALSE)

plot(bias, main="Biased Sample Locations")
points(pa_tab$x, pa_tab$y, pch = 16)

# number of complex data points
n_cp <- round(n_samples*2/3)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_med_bias_23.csv",
          row.names = FALSE)

# with bias, species ############################################################
n_samples <- 300
pa_tab <- generate_data_tabular(n_samples, sp_bias, prob_pres=prob_pres, weighted=TRUE)
write.csv(pa_tab,
          file = "data/tabular/hgam_pa_tab_data_med_spbiased.csv",
          row.names = FALSE)

plot(sp_bias, main="Biased Sample Locations")
points(pa_tab$x, pa_tab$y, pch = 16)

# number of complex data points
n_cp <- round(n_samples*2/3)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_med_spbias_23.csv",
          row.names = FALSE)

# with bias, all bias ###########################################################
n_samples <- 300
pa_tab <- generate_data_tabular(n_samples, all_bias, prob_pres=prob_pres, weighted=TRUE)
write.csv(pa_tab,
          file = "data/tabular/hgam_pa_tab_data_med_allbiased.csv",
          row.names = FALSE)

plot(all_bias, main="Biased Sample Locations")
points(pa_tab$x, pa_tab$y, pch = 16)

# saving data of varying quality
# number of complex data points
# 2/3 --------------------------------------------------------------------------
n_cp <- round(n_samples*2/3)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_med_allbias_23.csv",
          row.names = FALSE)

# 5/6 --------------------------------------------------------------------------
n_cp <- round(n_samples*5/6)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_med_allbias_56.csv",
          row.names = FALSE)

# 1/3 --------------------------------------------------------------------------
n_cp <- round(n_samples*1/3)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_med_allbias_13.csv",
          row.names = FALSE)

# 1/6 --------------------------------------------------------------------------
n_cp <- round(n_samples*1/6)
pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)

write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data_med_allbias_16.csv",
          row.names = FALSE)


# presence-only data ***********************************************************
# unbiased data ################################################################
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_nobias_23.csv")
occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
plot(prob_pres[[1]])
points(occurrence_coords$x, occurrence_coords$y, pch=16)
# presence_only
write.csv(
  x = occurrence_coords,
  file = "data/tabular/presence_only_data_hgam.csv",
  row.names = FALSE
)

# number of background points - pick 300 for now
n_bg_points <- 300
random_bg <- random_locations(mad_mask,
                              n_bg_points)
site_id <- seq(from=max(occurrence_coords$site_id)+1, length.out=nrow(random_bg))
# check points
plot(mad_mask)
points(random_bg)
po_tab <- tibble()
# add bg points into table for presence-absence
for(letter in c(letters[1:n_sp], "x")){
  po_tab <- rbind(po_tab, bind_cols(
    site_id=site_id,
    as_tibble(crds(random_bg)),
    tibble(
      pa=0,
      sp=rep(letter, length.out=nrow(random_bg)),
      not_complex = ifelse(sp == "x", 0, 1)
    )
  )
  )
}

# extract covariate values for those points
covs_vals <- extract(x=covs, y=crds(random_bg)) |> select(-any_of(c("not_complex", "sp")))
covs_vals <- bind_rows(replicate(n_sp+1, covs_vals, simplify=FALSE))
po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
  mutate(sp = as.factor(sp))

write.csv(
  x = po_model_data,
  file = "data/tabular/presence_only_data_rbg_hgam.csv",
  row.names = FALSE
)

# check if they seem right
plot(prob_pres[[1]])
i <- pa_model_data$sp=="a"
points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)

plot(prob_pres[[1]])
i <- po_model_data$sp=="a"
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)

plot(prob_pres[[2]])
i <- po_model_data$sp=="b"
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)

# biased data ##################################################################
# random background but travel-biased data #####################################
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_bias_23.csv")
occurrence_coords <- pa_model_data[pa_model_data$pa==1,]

write.csv(
  x = occurrence_coords,
  file = "data/tabular/presence_only_data_biased_hgam.csv",
  row.names = FALSE
)

po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
  mutate(sp = as.factor(sp))

write.csv(
  x = po_model_data,
  file = "data/tabular/presence_only_data_biased_rbg_hgam.csv",
  row.names = FALSE
)
# check points to see if they seem right
plot(prob_pres[[1]])
i <- pa_model_data$sp=="a"
points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)

plot(bias)
points(pa_model_data[i, c("x", "y")], pch=16)

plot(prob_pres[[1]])
i <- po_model_data$sp=="a"
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)

# random background but species-biased data ####################################
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_spbias_23.csv")
occurrence_coords <- pa_model_data[pa_model_data$pa==1,]

write.csv(
  x = occurrence_coords,
  file = "data/tabular/presence_only_data_spbiased_hgam.csv",
  row.names = FALSE
)
# combine into model data for po points
po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
  mutate(sp = as.factor(sp))

write.csv(
  x = po_model_data,
  file = "data/tabular/presence_only_data_spbiased_rbg_hgam.csv",
  row.names = FALSE
)

# check points to see if they seem right
plot(prob_pres[[1]])
i <- pa_model_data$sp=="a"
points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)

plot(sp_bias)
points(pa_model_data[i, c("x", "y")], pch=16)

plot(prob_pres[[1]])
i <- po_model_data$sp=="a"
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)

# random background but all-biased data #########################################
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_allbias_23.csv")
occurrence_coords <- pa_model_data[pa_model_data$pa==1,]

write.csv(
  x = occurrence_coords,
  file = "data/tabular/presence_only_data_allbiased_hgam.csv",
  row.names = FALSE
)

# combine into model data for po points
po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
  mutate(sp = as.factor(sp))

write.csv(
  x = po_model_data,
  file = "data/tabular/presence_only_data_allbiased_rbg_hgam.csv",
  row.names = FALSE
)
# check points to see if they seem right
plot(prob_pres[[1]])
i <- pa_model_data$sp=="a"
points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)

plot(all_bias)
points(pa_model_data[i, c("x", "y")], pch=16)

plot(prob_pres[[1]])
i <- po_model_data$sp=="a"
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)


# biased background and biased data ############################################
# generating biased background points, travel-biased ###########################
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_bias_23.csv")
occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
# can change number of background points, smaller is faster but maybe not as accurate
n_bg_points <- 300
biased_bg <- random_locations(bias,
                              (n_sp+1)*n_bg_points)
# converting into form to combine with occurrence coords
site_id <- seq(from=max(occurrence_coords$site_id), length.out=nrow(biased_bg))
po_tab <- tibble()
po_tab <- bind_cols(
    site_id=site_id,
    as_tibble(crds(biased_bg)),
    tibble(
      pa=0,
      sp=rep(c(letters[1:n_sp], "x"), length.out=nrow(biased_bg)),
      not_complex = ifelse(sp == "x", 0, 1)
    )
  )

# extract cov values at those locations
covs_vals <- extract(x=covs, y=crds(biased_bg)) |> select(-any_of(c("not_complex", "sp")))

# combine into model data for po points
po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
  mutate(sp = as.factor(sp))

write.csv(
  x = po_model_data,
  file = "data/tabular/presence_only_data_biased_bbg_hgam.csv",
  row.names = FALSE
)

# check points to see if they seem right
plot(prob_pres[[1]])
i <- pa_model_data$sp=="a"
points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)

plot(bias)
points(pa_model_data[i, c("x", "y")], pch=16)

plot(prob_pres[[1]])
i <- po_model_data$sp=="a"
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)

plot(prob_pres[[2]])
i <- po_model_data$sp=="b"
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)


# species-biased ################################################################
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_spbias_23.csv")
occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
n_bg_points <- 500
biased_bg <- random_locations(sp_bias,
                              (n_sp+1)*n_bg_points)
site_id <- seq(from=max(occurrence_coords$site_id), length.out=nrow(biased_bg))
po_tab <- tibble()
po_tab <- bind_cols(
  site_id=site_id,
  as_tibble(crds(biased_bg)),
  tibble(
    pa=0,
    sp=rep(c(letters[1:n_sp], "x"), length.out=nrow(biased_bg)),
    not_complex = ifelse(sp == "x", 0, 1)
  )
)


covs_vals <- extract(x=covs, y=crds(biased_bg)) |> select(-any_of(c("not_complex", "sp")))
# combine into model data for po points
po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
  mutate(sp = as.factor(sp))

write.csv(
  x = po_model_data,
  file = "data/tabular/presence_only_data_spbiased_bbg_hgam.csv",
  row.names = FALSE
)

# check points to see if they seem right
plot(prob_pres[[1]])
i <- pa_model_data$sp=="a"
points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)

plot(sp_bias)
points(pa_model_data[i, c("x", "y")], pch=16)

plot(prob_pres[[1]])
i <- po_model_data$sp=="a"
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)

plot(prob_pres[[2]])
i <- po_model_data$sp=="b"
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)


# use only species data, no complex data $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# unbiased ######################################################################
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_nobias_23.csv")
# removing group data
pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_pa_data_med_nobias_23_nocp.csv",
  row.names = FALSE
)

pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_nobias_56.csv")
# removing group data
pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_pa_data_med_nobias_56_nocp.csv",
  row.names = FALSE
)

# biased #########################################################################
# travel-biased
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_bias_23.csv")
# removing group data
pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_pa_data_med_bias_23_nocp.csv",
  row.names = FALSE
)
# species-biased
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_spbias_23.csv")
# removing group data
pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_pa_data_med_spbias_23_nocp.csv",
  row.names = FALSE
)
# all biased ###################################################################
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_allbias_23.csv")
pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_pa_data_med_allbias_23_nocp.csv",
  row.names = FALSE
)

# varying quality (fraction of complex) ########################################
# unbiased #####################################################################
# 5/6 complex ------------------------------------------------------------------
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_nobias_56.csv")
pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_pa_data_med_nobias_56_nocp.csv",
  row.names = FALSE
)

# 1/3 complex ------------------------------------------------------------------
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_nobias_13.csv")
pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_pa_data_med_nobias_13_nocp.csv",
  row.names = FALSE
)

# 1/6 complex ------------------------------------------------------------------
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_nobias_16.csv")
pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_pa_data_med_nobias_16_nocp.csv",
  row.names = FALSE
)

# biased #######################################################################
# 5/6 complex -------------------------------------------------------------------
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_allbias_56.csv")
pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_pa_data_med_allbias_56_nocp.csv",
  row.names = FALSE
)

# 1/3 complex -------------------------------------------------------------------
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_allbias_13.csv")
pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_pa_data_med_allbias_13_nocp.csv",
  row.names = FALSE
)

# 1/6 complex -------------------------------------------------------------------
pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_allbias_16.csv")
pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_pa_data_med_allbias_16_nocp.csv",
  row.names = FALSE
)

# presence-only data ***********************************************************
# unbiased ######################################################################
po_model_data <- read_csv("data/tabular/presence_only_data_rbg_hgam.csv")
po_model_data <- po_model_data[po_model_data$sp != "x",]
write.csv(
  x = po_model_data,
  file = "data/tabular/presence_only_data_rbg_hgam_nocp.csv",
  row.names = FALSE
)

# biased #########################################################################
# travel-biased
po_model_data <- read_csv("data/tabular/presence_only_data_biased_rbg_hgam.csv")
po_model_data <- po_model_data[po_model_data$sp != "x",]
write.csv(
  x = po_model_data,
  file = "data/tabular/presence_only_data_biased_rbg_hgam_nocp.csv",
  row.names = FALSE
)
# species-biased
po_model_data <- read_csv("data/tabular/presence_only_data_spbiased_rbg_hgam.csv")
po_model_data <- po_model_data[po_model_data$sp != "x",]
write.csv(
  x = po_model_data,
  file = "data/tabular/presence_only_data_spbiased_rbg_hgam_nocp.csv",
  row.names = FALSE
)
# all-biased
po_model_data <- read_csv("data/tabular/presence_only_data_allbiased_rbg_hgam.csv")
po_model_data <- po_model_data[po_model_data$sp != "x",]
write.csv(
  x = po_model_data,
  file = "data/tabular/presence_only_data_allbiased_rbg_hgam_nocp.csv",
  row.names = FALSE
)
