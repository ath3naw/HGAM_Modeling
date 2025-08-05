rm(list=ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
source("R/hgam_multiple_layers/functions_multiple.R")

# Load in variables
par(mfrow = c(1,1))
prob_pres_sp <- terra::rast("data/grids/hgam_multiple_layers/spec_prob_pres_hgam.tif")
prob_pres_cp <- terra::rast("data/grids/hgam_multiple_layers/complex_prob_pres_hgam.tif")
covs <- terra::rast("data/grids/covariates.tif")
mad_mask <- terra::rast("data/grids/mad_mask.tif")
bc_mad <- terra::rast("data/grids/bc_mad.tif")
bias <- terra::rast("data/grids/bias.tif")
n_sp <- read_csv("data/tabular/hgam_multiple_layers/n_sp")
n_sp <- as.data.frame(n_sp)
n_cp <- nrow(n_sp)
# choose sp_bias
sp_bias <- prob_pres_sp[[9]]
writeRaster(sp_bias,
            "data/grids/sp_bias.tif",
            overwrite = TRUE)
all_bias <- bias*sp_bias

## Now for simulation of the data
### Medium data, no bias ********************************************************************
n_samples <- 149
num_species <- nlyr(prob_pres_sp)

pa_tab <- generate_data_tabular(n_samples, mad_mask, prob_pres=prob_pres_sp, n_sp=n_sp)
write.csv(pa_tab,
          file = "data/tabular/hgam_multiple_layers/hgam_pa_tab_data_med.csv",
          row.names = FALSE)

# checking presence and absence data
plot(prob_pres_sp[[1]], range=c(0,1))
points(pa_tab$x, pa_tab$y, pch=21, bg=pa_tab$"1"==1)
plot(prob_pres_sp[[2]], range=c(0,1))
points(pa_tab$x, pa_tab$y, pch=21, bg=pa_tab$"2"==1)

## picking number of complex points, etc.
n_group <- 49
n_complex <- c(49, 73)

pa_model_data <- generate_model_data(n_samples, n_group, n_complex, n_cp, n_sp, pa_tab)

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias.csv",
          row.names = FALSE)

### with bias, travel time ######################################################
pa_tab <- generate_data_tabular(n_samples, bias, prob_pres=prob_pres_sp, n_sp=n_sp, weighted=TRUE)
write.csv(pa_tab,
          file = "data/tabular/hgam_multiple_layers/hgam_pa_tab_data_med_biased.csv",
          row.names = FALSE)

plot(bias, main="Biased Sample Locations")
points(pa_tab$x, pa_tab$y, pch = 16)

pa_model_data <- generate_model_data(n_samples, n_group, n_complex, n_cp, n_sp, pa_tab)

write.csv(pa_model_data,
          file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_bias.csv",
          row.names = FALSE)

### with bias, species ##########################################################
pa_tab <- generate_data_tabular(n_samples, sp_bias, prob_pres=prob_pres_sp, n_sp=n_sp, weighted=TRUE)
write.csv(pa_tab,
          file = "data/tabular/hgam_multiple_layers/hgam_pa_tab_data_med_spbiased.csv",
          row.names = FALSE)

plot(sp_bias, main="Biased Sample Locations")
points(pa_tab$x, pa_tab$y, pch = 16)

pa_model_data <- generate_model_data(n_samples, n_group, n_complex, n_cp, n_sp, pa_tab)

write.csv(pa_model_data,
          file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_spbias.csv",
          row.names = FALSE)

### with bias, all bias #########################################################
pa_tab <- generate_data_tabular(n_samples, all_bias, prob_pres=prob_pres_sp, n_sp=n_sp, weighted=TRUE)
write.csv(pa_tab,
          file = "data/tabular/hgam_multiple_layers/hgam_pa_tab_data_med_allbiased.csv",
          row.names = FALSE)

plot(all_bias, main="Biased Sample Locations")
points(pa_tab$x, pa_tab$y, pch = 16)

pa_model_data <- generate_model_data(n_samples, n_group, n_complex, n_cp, n_sp, pa_tab)

write.csv(pa_model_data,
          file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias.csv",
          row.names = FALSE)

# use only species-data, no complex data $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# unbiased
pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias.csv")
pa_model_data <- pa_model_data[!pa_model_data$sp %in% c("group", "complex1", "complex2"),]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias_nocp.csv",
  row.names = FALSE
)

# use only species-data, no complex data $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# biased
pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias.csv")
pa_model_data <- pa_model_data[!pa_model_data$sp %in% c("group", "complex1", "complex2"),]
write.csv(
  x = pa_model_data,
  file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias_nocp.csv",
  row.names = FALSE
)

### presence-only data **********************************************************************
## unbiased data ###############################################################
par(mfrow=c(1,1))
pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias.csv")
plot(prob_pres_sp[[1]], range=c(0,1))
points(pa_tab$x, pa_tab$y, pch=21, bg=pa_tab$"1"==1)
occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
plot(prob_pres_sp[[1]])
points(occurrence_coords[occurrence_coords$sp==1,]$x, occurrence_coords[occurrence_coords$sp==1,]$y, pch=16)
# presence_only
write.csv(
  x = occurrence_coords,
  file = "data/tabular/hgam_multiple_layers/presence_only_data_hgam.csv",
  row.names = FALSE
)

# define number of points
n_bg_points <- 200
num_groups <- 15
random_bg <- random_locations(mad_mask,
                              n_bg_points)

# adjust data frames
site_id <- seq(from=max(occurrence_coords$site_id)+1, length.out=nrow(random_bg))
plot(mad_mask)
points(random_bg)
complex_names <- paste0("complex", 1:n_cp)
po_tab <- tibble()
for(i in c(1:num_species, complex_names, "group")){
    po_tab <- rbind(po_tab, bind_cols(
    site_id=site_id,
    as_tibble(crds(random_bg)),
    tibble(
      pa=0,
      sp=rep(i, length.out=nrow(random_bg)),
      not_complex = ifelse(sp %in% c(complex_names, "group"), 0, 1)
    )
  )
  )
}
j <- 0
for(i in 1:n_cp){
  po_tab <- po_tab |> 
    mutate(!!complex_names[i] := ifelse(sp %in% c(complex_names[i], (1+j):(n_sp[i,]+j)),1 ,0))
  j <- j+n_sp[i,]
}

covs_vals <- extract(x=covs, y=crds(random_bg)) |> select(-any_of(c("not_complex", "sp", "complex1", "complex2")))
covs_vals <- bind_rows(replicate(num_groups, covs_vals, simplify=FALSE))

# create the po_model_data for model
po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
  mutate(sp = as.factor(sp))

# save it
write.csv(
  x = po_model_data,
  file = "data/tabular/hgam_multiple_layers/presence_only_data_rbg_hgam.csv",
  row.names = FALSE
)

# plotting it to make sure it makes sense
plot(prob_pres_sp[[1]])
i <- pa_model_data$sp==1
points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)

plot(prob_pres_sp[[1]])
i <- po_model_data$sp==1
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)

plot(prob_pres_sp[[3]])
i <- po_model_data$sp==3
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)

## random background but biased data
pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias.csv")
occurrence_coords <- pa_model_data[pa_model_data$pa==1,]

# presence_only *****************************************************************
write.csv(
  x = occurrence_coords,
  file = "data/tabular/hgam_multiple_layers/presence_only_data_allbiased_hgam.csv",
  row.names = FALSE
)

po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
  mutate(sp = as.factor(sp))

write.csv(
  x = po_model_data,
  file = "data/tabular/hgam_multiple_layers/presence_only_data_allbiased_rbg_hgam.csv",
  row.names = FALSE
)

plot(prob_pres[[1]])
i <- pa_model_data$sp=="1"
points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)

plot(all_bias)
points(pa_model_data[i, c("x", "y")], pch=16)

plot(prob_pres[[1]])
i <- po_model_data$sp=="1"
points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)

# if you want other types of bias in simulations
# ## random background but travel-biased data ###################################
# pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_bias_23.csv")
# occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
# 
# # presence_only
# write.csv(
#   x = occurrence_coords,
#   file = "data/tabular/hgam_multiple_layers/presence_only_data_biased_hgam.csv",
#   row.names = FALSE
# )
# 
# po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
#   mutate(sp = as.factor(sp))
# 
# write.csv(
#   x = po_model_data,
#   file = "data/tabular/hgam_multiple_layers/presence_only_data_biased_rbg_hgam.csv",
#   row.names = FALSE
# )
# 
# plot(prob_pres[[1]])
# i <- pa_model_data$sp=="1"
# points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)
# 
# plot(bias)
# points(pa_model_data[i, c("x", "y")], pch=16)
# 
# plot(prob_pres[[1]])
# i <- po_model_data$sp=="1"
# points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)
# 
# # random background but species-biased data ###################################
# pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_spbias_23.csv")
# occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
# 
# # presence_only
# write.csv(
#   x = occurrence_coords,
#   file = "data/tabular/hgam_multiple_layers/presence_only_data_spbiased_hgam.csv",
#   row.names = FALSE
# )
# 
# po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
#   mutate(sp = as.factor(sp))
# 
# write.csv(
#   x = po_model_data,
#   file = "data/tabular/hgam_multiple_layers/presence_only_data_spbiased_rbg_hgam.csv",
#   row.names = FALSE
# )
# 
# plot(prob_pres[[1]])
# i <- pa_model_data$sp=="1"
# points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)
# 
# plot(sp_bias)
# points(pa_model_data[i, c("x", "y")], pch=16)
# 
# plot(prob_pres[[1]])
# i <- po_model_data$sp=="1"
# points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)

# use only species-data, no complex data $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# unbiased po ###################################################################
po_model_data <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_rbg_hgam.csv")
po_model_data <- po_model_data[!po_model_data$sp %in% c("group", "complex1", "complex2"),]
write.csv(
  x = po_model_data,
  file = "data/tabular/hgam_multiple_layers/hgam_po_data_med_nocp.csv",
  row.names = FALSE
)
# biased po #####################################################################
po_model_data <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_allbiased_rbg_hgam.csv")
po_model_data <- po_model_data[!po_model_data$sp %in% c("group", "complex1", "complex2"),]
write.csv(
  x = po_model_data,
  file = "data/tabular/hgam_multiple_layers/hgam_po_data_med_allbias_nocp.csv",
  row.names = FALSE
)

# if you want to test out biased background with biased data
# # biased background and biased data
# # generating biased background points, travel-biased
# pa_model_data <- read_csv("data/tabular/hglm_pa_data_med_bias_23.csv")
# occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
#
# # set number of background points as 200 for now
# n_bg_points <- 200
# biased_bg <- random_locations(bias,
#                               (n_sp+1)*n_bg_points)
# site_id <- seq(from=max(occurrence_coords$site_id), length.out=nrow(biased_bg))
# po_tab <- tibble()
# po_tab <- bind_cols(
#   site_id=site_id,
#   as_tibble(crds(biased_bg)),
#   tibble(
#     pa=0,
#     sp=rep(c(letters[1:n_sp], "x"), length.out=nrow(biased_bg)),
#     not_complex = ifelse(sp == "x", 0, 1)
#   )
# )
# 
# 
# covs_vals <- extract(x=covs, y=crds(biased_bg)) |> select(-any_of(c("not_complex", "sp")))
# po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
#   mutate(sp = as.factor(sp))
# 
# write.csv(
#   x = po_model_data,
#   file = "data/tabular/presence_only_data_biased_bbg_hgam.csv",
#   row.names = FALSE
# )
# 
# plot(prob_pres[[1]])
# i <- pa_model_data$sp=="a"
# points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)
# 
# plot(bias)
# points(pa_model_data[i, c("x", "y")], pch=16)
# 
# plot(prob_pres[[1]])
# i <- po_model_data$sp=="a"
# points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)
# 
# plot(prob_pres[[2]])
# i <- po_model_data$sp=="b"
# points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)
# 
# 
# # species-biased
# pa_model_data <- read_csv("data/tabular/hglm_pa_data_med_spbias_23.csv")
# occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
# n_bg_points <- 500
# biased_bg <- random_locations(sp_bias,
#                               (n_sp+1)*n_bg_points)
# site_id <- seq(from=max(occurrence_coords$site_id), length.out=nrow(biased_bg))
# po_tab <- tibble()
# po_tab <- bind_cols(
#   site_id=site_id,
#   as_tibble(crds(biased_bg)),
#   tibble(
#     pa=0,
#     sp=rep(c(letters[1:n_sp], "x"), length.out=nrow(biased_bg)),
#     not_complex = ifelse(sp == "x", 0, 1)
#   )
# )
# 
# 
# covs_vals <- extract(x=covs, y=crds(biased_bg)) |> select(-any_of(c("not_complex", "sp")))
# po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
#   mutate(sp = as.factor(sp))
# 
# write.csv(
#   x = po_model_data,
#   file = "data/tabular/presence_only_data_spbiased_bbg_hgam.csv",
#   row.names = FALSE
# )
# 
# plot(prob_pres[[1]])
# i <- pa_model_data$sp=="a"
# points(pa_model_data[i, c("x", "y")], pch=21, bg=pa_model_data$pa[i]==1)
# 
# plot(sp_bias)
# points(pa_model_data[i, c("x", "y")], pch=16)
# 
# plot(prob_pres[[1]])
# i <- po_model_data$sp=="a"
# points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)
# 
# plot(prob_pres[[2]])
# i <- po_model_data$sp=="b"
# points(po_model_data[i, c("x", "y")], pch=21, bg=po_model_data$pa[i]==1)


