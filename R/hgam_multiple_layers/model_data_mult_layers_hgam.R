rm(list = ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
library(mgcv)
source("R/hgam_multiple_layers/functions_multiple.R")

# read in variables
prob_pres_sp <- terra::rast("data/grids/hgam_multiple_layers/spec_prob_pres_hgam.tif")
prob_pres_cp <- terra::rast("data/grids/hgam_multiple_layers/complex_prob_pres_hgam.tif")
covs <- terra::rast("data/grids/covariates.tif")
mad_mask <- terra::rast("data/grids/mad_mask.tif")
bias <- terra::rast("data/grids/bias.tif")
spbias <- terra::rast("data/grids/sp_bias.tif") # can comment out whichever one

# defining number of species + complexes
n_sp <- read_csv("data/tabular/hgam_multiple_layers/n_sp")
n_sp <- as.data.frame(n_sp)
n_cp <- nrow(n_sp)
num_species <- sum(n_sp)
species <- data.frame(
  species_id = 1:num_species,
  complex = rep(seq_len(nrow(n_sp)), times=n_sp$n_sp)
)


# calculating group probability of presence from individual species
complex_prob_pres <- rast(rep(mad_mask, n_cp))
j <- 0
for(i in 1:n_cp){
  layers <- prob_pres_sp[[(1+j):(n_sp[i,]+j)]]
  complex_prob_pres[[i]] <- 1-app(1-layers, fun=prod, na.rm=TRUE)
  j <- j+n_sp[i,]
}

group_prob_pres <- 1-app(1-complex_prob_pres, fun=prod, na.rm=TRUE)

pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias_23.csv")
pa_model_data$sp <- as.factor(pa_model_data$sp)

# generating formula
smooths <- paste0("te(ttemp, pprec, bs=c('tp', 'tp'), by=complex",1:n_cp,")")
full_formula <- paste("pa ~ te(ttemp, pprec, bs=c('tp', 'tp')) + ",
                 paste(smooths, collapse = " + "),
                 " + te(ttemp, pprec, sp, bs=c('tp', 'tp', 're'),
                      by=not_complex)")
full_formula <- as.formula(full_formula)
full_formula

# presence-absence data ********************************************************
# unbiased #####################################################################
# need to have categories of complex
# model: all data, all smooths
mos_modGS <- gam(formula=full_formula, 
                 data = pa_model_data, 
                 optimizer=c("outer", "bfgs"),
                 family = "binomial", 
                 method = "REML")

# group model
covs$complex1 <- 0
covs$complex2 <- 0
covs$not_complex <- 0
covs$sp <- "group"

pred_pa_modGS_group <- sdm_predict(
  model = mos_modGS,
  covariates = covs
)

par(mfrow=c(1,2))
# plot predicted vs true distributions
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_pa_modGS_group, main="predicted_dist - unbiased", range=c(0,1))

# complex model
pred_pa_modGS_complex <- rast(rep(mad_mask, n_cp))
covs$not_complex <- 0
par(mfrow=c(2,2))
# Loop through all complexes
for(i in 1:n_cp){
  covs$sp <- paste0("complex", i)
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  
  # Set current complex to 1
  covs[[paste0("complex", i)]] <- 1
  pred_pa_modGS_complex[[i]] <- sdm_predict(
    model = mos_modGS,
    covariates = covs
  )
  # plot predicted vs true distributions
  plot(complex_prob_pres[[i]], main = "True Prob of Pres", range=c(0,1))
  plot(pred_pa_modGS_complex[[i]], main = paste("Complex", i, "- unbiased"), range=c(0,1))
}

# species model
pred_pa_modGS_sp <- rast(rep(mad_mask, num_species))
covs$not_complex <- 1
# Plot all species
for(i in 1:num_species){
  covs$sp <- i
  cp <- species[i, 2]
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  # Set current complex to 1
  covs[[paste0("complex", cp)]] <- 1
  pred_pa_modGS_sp[[i]] <- sdm_predict(
    model = mos_modGS,
    covariates = covs
  )
  par(mfrow=c(1,2))
  # plot predicted vs true distributions
  plot(prob_pres_sp[[i]], main = "True Prob of Pres", range=c(0,1))
  plot(pred_pa_modGS_sp[[i]], main = paste("Species", i, "Complex", species$complex[i], "- unbiased"), range=c(0,1))
}

par(mfrow=c(1,2))
# plot partial response plots
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

# check models
round(k.check(mos_modGS),2)

# biased #######################################################################
pa_model_data_allbiased <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias.csv")
pa_model_data_allbiased$sp <- as.factor(pa_model_data_allbiased$sp)

# model: all data, all smooths
mos_modGS_allbiased <- gam(formula=full_formula, 
                        data = pa_model_data_allbiased, 
                        optimizer=c("outer", "bfgs"),
                        family = "binomial", 
                        method = "REML")

# group
covs$not_complex <- 0
covs$complex1 <- 0
covs$complex2 <- 0
covs$sp <- "group"
# predict based on models
pred_pa_modGS_group_allbiased <- sdm_predict(
  model = mos_modGS_allbiased,
  covariates = covs
)

par(mfrow=c(2,2))
# plot predicted vs true distributions
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_pa_modGS_group, main="predicted_dist - unbiased", range=c(0,1))
plot(pred_pa_modGS_group_allbiased, main="predicted_dist - biased", range=c(0,1))

# complex
pred_pa_modGS_complex_allbiased <- rast(rep(mad_mask, n_cp))
covs$not_complex <- 0
# loop through all complexes
for(i in 1:n_cp){
  covs$sp <- paste0("complex", i)
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  
  # Set current complex to 1
  covs[[paste0("complex", i)]] <- 1
  pred_pa_modGS_complex_allbiased[[i]] <- sdm_predict(
    model = mos_modGS_allbiased,
    covariates = covs
  )
  
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(complex_prob_pres[[i]], main = "True Prob of Pres")
  plot(pred_pa_modGS_complex[[i]], main = paste("Complex", i, "- unbiased"))
  plot(pred_pa_modGS_complex_allbiased[[i]], main = paste("Complex", i, "- biased"))
}

# species model
pred_pa_modGS_sp_allbiased <- rast(rep(mad_mask, num_species))
covs$not_complex <- 1
# Plot all species
for(i in 1:num_species){
  covs$sp <- i
  cp <- species[i, 2]
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  # Set current complex to 1
  covs[[paste0("complex", cp)]] <- 1
  pred_pa_modGS_sp_allbiased[[i]] <- sdm_predict(
    model = mos_modGS_allbiased,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(prob_pres_sp[[i]], main = "True Prob of Pres")
  plot(pred_pa_modGS_sp[[i]], main = paste("Species", i, "Complex", species$complex[i], "- unbiased"))
  plot(pred_pa_modGS_sp_allbiased[[i]], main = paste("Species", i, "Complex", species$complex[i], "- biased"))
}

par(mfrow=c(1,1))
# check to see if seems right
plot(all_bias)
points(pa_model_data_allbiased$x, pa_model_data_allbiased$y, pch=20)

par(mfrow=c(1,2))

# plot partial response plots
partial_response_plot(
  model = mos_modGS_allbiased,
  data = pa_model_data_allbiased,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_allbiased,
  data = pa_model_data_allbiased,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# check models
round(k.check(mos_modGS_allbiased),2)

# presence-only data ***********************************************************
# presence-only model, unbiased ################################################
po_model_data_rbg <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_rbg_hgam.csv")
po_model_data_rbg$sp <- as.factor(po_model_data_rbg$sp)
# model: all data, all smooths
mos_modGS_rbg <- gam(formula = full_formula, 
                     data = po_model_data_rbg, 
                     optimizer=c("outer", "bfgs"),
                     family = "binomial", 
                     method = "REML")

# group
covs$not_complex <- 0
covs$complex1 <- 0
covs$complex2 <- 0
covs$sp <- "group"
pred_po_modGS_rbg_group <- sdm_predict(
  model = mos_modGS_rbg,
  covariates = covs
)
par(mfrow=c(1,2))
# plot predicted vs true distributions
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_po_modGS_rbg_group, main="predicted_dist - unbiased")

# complex
pred_po_modGS_complex_rbg <- rast(rep(mad_mask, n_cp))
covs$not_complex <- 0
par(mfrow=c(2,2))
# Plot all complexes
for(i in 1:n_cp){
  covs$sp <- paste0("complex", i)
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  
  # Set current complex to 1
  covs[[paste0("complex", i)]] <- 1
  pred_po_modGS_complex_rbg[[i]] <- sdm_predict(
    model = mos_modGS_rbg,
    covariates = covs
  )
  # plot predicted vs true distributions
  plot(complex_prob_pres[[i]], main = "True Prob of Pres")
  plot(pred_po_modGS_complex_rbg[[i]], main = paste("Complex", i, "- unbiased"))
}

# species
pred_po_modGS_sp_rbg <- rast(rep(mad_mask, num_species))
covs$not_complex <- 1
par(mfrow=c(1,2))
# Plot all species
for(i in 1:num_species){
  covs$sp <- i
  cp <- species[i, 2]
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  # Set current complex to 1
  covs[[paste0("complex", cp)]] <- 1
  pred_po_modGS_sp_rbg[[i]] <- sdm_predict(
    model = mos_modGS_rbg,
    covariates = covs
  )
  # plot predicted vs true distributions
  plot(prob_pres_sp[[i]], main = "True Prob of Pres")
  plot(pred_po_modGS_sp_rbg[[i]], main = paste("Species", i, "Complex", species$complex[i], "- unbiased"))
}

par(mfrow=c(1,2))

# plot partial response plots
partial_response_plot(
  model = mos_modGS_rbg,
  data = po_model_data_rbg,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_rbg,
  data = po_model_data_rbg,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# check models
round(k.check(mos_modGS_rbg),2)

# presence-only model, biased ###################################################
# biased data with random background
po_model_data_allbiased_rbg <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_allbiased_rbg_hgam.csv")
po_model_data_allbiased_rbg$sp <- as.factor(po_model_data_allbiased_rbg$sp)

# model: all data, all smooths
mos_modGS_allbiased_rbg <- gam(formula = full_formula, 
                            data = po_model_data_allbiased_rbg,
                            optimizer=c("outer", "bfgs"),
                            family = "binomial", 
                            method = "REML")

# group
covs$not_complex <- 0
covs$complex1 <- 0
covs$complex2 <- 0
covs$sp <- "group"

# predict based on models
pred_po_modGS_allbiased_rbg_group <- sdm_predict(
  model = mos_modGS_allbiased_rbg,
  covariates = covs
)

par(mfrow=c(2,2))
# plot predicted vs true distributions
plot(group_prob_pres, main="Group Prob of Pres")
plot(pred_po_modGS_rbg_group, main="predicted_dist - unbiased")
plot(pred_po_modGS_allbiased_rbg_group, main="predicted_dist - biased")

# complex distributions
pred_po_modGS_complex_allbiased_rbg <- rast(rep(mad_mask, n_cp))
covs$not_complex <- 0
# Plot all complexes
for(i in 1:n_cp){
  covs$sp <- paste0("complex", i)
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  
  # Set current complex to 1
  covs[[paste0("complex", i)]] <- 1
  pred_po_modGS_complex_allbiased_rbg[[i]] <- sdm_predict(
    model = mos_modGS_allbiased_rbg,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(complex_prob_pres[[i]], main = "True Prob of Pres")
  plot(pred_po_modGS_complex_rbg[[i]], main = paste("Complex", i, "- unbiased"))
  plot(pred_po_modGS_complex_allbiased_rbg[[i]], main = paste("Complex", i, "- biased"))
}

# species
pred_po_modGS_sp_allbiased_rbg <- rast(rep(mad_mask, num_species))
covs$not_complex <- 1
# Plot all species
for(i in 1:num_species){
  covs$sp <- i
  cp <- species[i, 2]
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  # Set current complex to 1
  covs[[paste0("complex", cp)]] <- 1
  pred_po_modGS_sp_allbiased_rbg[[i]] <- sdm_predict(
    model = mos_modGS_allbiased_rbg,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(prob_pres_sp[[i]], main = "True Prob of Pres")
  plot(pred_po_modGS_sp_rbg[[i]], main = paste("Species", i, "Complex", species$complex[i], "- unbiased"))
  plot(pred_po_modGS_sp_allbiased_rbg[[i]], main = paste("Species", i, "Complex", species$complex[i], "- biased"))
}

par(mfrow=c(3,2))
# plot partial response plots
partial_response_plot(
  model = mos_modGS_rbg,
  data = po_model_data_rbg,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_rbg,
  data = po_model_data_rbg,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

partial_response_plot(
  model = mos_modGS_allbiased_rbg,
  data = po_model_data_allbiased_rbg,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_allbiased_rbg,
  data = po_model_data_allbiased_rbg,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# check models
round(k.check(mos_modGS_rbg),2)
round(k.check(mos_modGS_allbiased_rbg),2)

