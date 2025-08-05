rm(list=ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
library(mgcv)
library(sp)
source("R/leucosphyrus_model/leuco_functions.R")
# part 4
pa_model_data <- read_csv("data/tabular/hgam_leuco_data_nobias.csv")
covs <- terra::rast("data/grids/covs_hgam.tif")
leuco_map_mask <- terra::rast("data/grids/leuco_map_mask.tif")
cp <- c("leucosphyrus", "dirus")

full_formula <- "pa ~ te(ttemp, pprec, bs=c('tp', 'tp')) + 
                      te(ttemp, pprec, bs=c('tp', 'tp'), by=leucosphyrus) +
                      te(ttemp, pprec, bs=c('tp', 'tp'), by=dirus) +
                      te(ttemp, pprec, sp, bs=c('tp', 'tp', 're'),
                      by=not_complex)"
full_formula <- as.formula(full_formula)
full_formula
pa_model_data$sp <- factor(pa_model_data$sp)
  
mos_modGS <- gam(formula=full_formula, 
                 data = pa_model_data, 
                 optimizer=c("outer", "bfgs"),
                 family = "binomial", 
                 method = "REML")

# group model
covs$leucosphyrus <- 0
covs$dirus <- 0
covs$not_complex <- 0
covs$sp <- "Leucosphyrus Subgroup"

pred_pa_modGS_group <- sdm_predict(
  model = mos_modGS,
  covariates = covs
)

par(mfrow=c(1,1))
plot(pred_pa_modGS_group, main="predicted_dist - unbiased", range=c(0,1))
points(pa_model_data$lonx, pa_model_data$laty, bg=pa_model_data$pa, pch=21)

# complex model
n_cp <- 2
cp_names <- c("Leucosphyrus Complex", "Dirus Complex")
cp <- c("leucosphyrus", "dirus")
pred_pa_modGS_complex <- rast(rep(leuco_map_mask, n_cp))
covs$not_complex <- 0
par(mfrow=c(1,2))
# Plot all complexes
for(i in 1:n_cp){
  covs[["leucosphyrus"]] <- 0  
  covs[["dirus"]] <- 0  
  
  # Set current complex to 1
  covs[[cp[i]]] <- 1
  covs$sp <- cp_names[i]
  
  pred_pa_modGS_complex[[i]] <- sdm_predict(
    model = mos_modGS,
    covariates = covs
  )
  plot(pred_pa_modGS_complex[[i]], main = paste(cp_names[i], "- unbiased"), range=c(0,1))
}

# species model
covs$not_complex <- 1
species <- c("Anopheles Leucosphyrus", "Anopheles Latens", "Anopheles Introlatus", "Anopheles Balabacensis", "Anopheles Baisasi")
num_species <- length(species)
pred_pa_modGS_sp <- rast(rep(leuco_map_mask, num_species))
spcp <- data.frame(
  cp=c(1,1,1,1,1,2,2,2,2,2,2,2), sp=c("Anopheles Leucosphyrus", "Anopheles Latens",
                                      "Anopheles Introlatus", "Anopheles Balabacensis",
                                      "Anopheles Baisasi", "Anopheles Dirus", "Anopheles Cracens", "Anopheles Scanloni",
                                      "Anopheles Baimaii", "Anopheles Elegans", "Anopheles Takasagoensis", "Anopheles Nemophilous")
)
# Plot all species
par(mfrow=c(2,3))
for(i in 1:num_species){
  covs$sp <- species[i]
  comp <- spcp[i,1]
  covs[["leucosphyrus"]] <- 0  
  covs[["dirus"]] <- 0 
  # Set current complex to 1
  covs[[cp[comp]]] <- 1
  pred_pa_modGS_sp[[i]] <- sdm_predict(
    model = mos_modGS,
    covariates = covs
  )
  plot(pred_pa_modGS_sp[[i]], main = paste(species[i], "Complex", spcp[i,1], "- unbiased"), range=c(0,1))
}

par(mfrow=c(1,2))

partial_response_plot(
  model = mos_modGS,
  data = pa_model_data,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS,
  data = pa_model_data,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

round(k.check(mos_modGS),2)