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

pa_tab <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_tab_data_med.csv")

# calculating group probability of presence from individual species
complex_prob_pres <- rast(rep(mad_mask, n_cp))
j <- 0
for(i in 1:n_cp){
  layers <- prob_pres_sp[[(1+j):(n_sp[i,]+j)]]
  complex_prob_pres[[i]] <- 1-app(1-layers, fun=prod, na.rm=TRUE)
  j <- j+n_sp[i,]
}

group_prob_pres <- 1-app(1-complex_prob_pres, fun=prod, na.rm=TRUE)

# generating formula
smooths <- paste0("te(ttemp, pprec, bs=c('tp', 'tp'), by=complex",1:n_cp,")")
full_formula <- paste("pa ~ te(ttemp, pprec, bs=c('tp', 'tp')) + ",
                      paste(smooths, collapse = " + "),
                      " + te(ttemp, pprec, sp, bs=c('tp', 'tp', 're'),
                      by=not_complex)")
full_formula <- as.formula(full_formula)
full_formula

# presence-absence data! ********************************************************
pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias.csv")
pa_model_data$sp <- as.factor(pa_model_data$sp)
# species-only data
pa_model_data_nocp <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias_nocp.csv")
pa_model_data_nocp$sp <- as.factor(pa_model_data_nocp$sp)

# unbiased ######################################################################
# model: all data, all smooths
mos_modGS <- gam(formula = full_formula, 
                 data = pa_model_data,
                 optimizer=c("outer", "bfgs"),
                 family = "binomial", 
                 method = "REML")
# model: species data, all smooths
mos_modGS_nocp <- gam(formula = full_formula, 
                      data = pa_model_data_nocp, 
                      optimizer=c("outer", "bfgs"),
                      family = "binomial", 
                      method = "REML")

# group
covs$not_complex <- 0
covs$sp <- "group"
# predict based on models
pred_pa_modGS_group <- sdm_predict(
  model = mos_modGS,
  covariates = covs
)
pred_pa_modGS_nocp_group <- sdm_predict(
  model = mos_modGS_nocp,
  covariates = covs
)

par(mfrow=c(2,2))
# plot predicted vs true distributions
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_pa_modGS_group, main="Dist W/ Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_nocp_group, main="Dist W/O Complex - unbiased", range=c(0,1))

# complex
pred_pa_modGS_complex <- rast(rep(mad_mask, n_cp))
pred_pa_modGS_complex_nocp <- rast(rep(mad_mask, n_cp))
covs$not_complex <- 0
# loop through each complex
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
  pred_pa_modGS_complex_nocp[[i]] <- sdm_predict(
    model = mos_modGS_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(complex_prob_pres[[i]], main = "True Prob of Pres")
  plot(pred_pa_modGS_complex[[i]], main = paste("Complex", i, "- unbiased CP"), range=c(0,1))
  plot(pred_pa_modGS_complex_nocp[[i]], main = paste("Complex", i, "- unbiased No CP"), range=c(0,1))  
}

# species
pred_pa_modGS_sp <- rast(rep(mad_mask, num_species))
pred_pa_modGS_sp_nocp <- rast(rep(mad_mask, num_species))
covs$not_complex <- 1
# data frames for storing results
mse <- data.frame(CP = 0, NoCP=0)
cor <- data.frame(CP = 0, NoCP=0)
for(i in 1:num_species){
  covs$sp <- factor(i)
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
  pred_pa_modGS_sp_nocp[[i]] <- sdm_predict(
    model = mos_modGS_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(prob_pres_sp[[i]], main = "True Prob of Pres")
  plot(pred_pa_modGS_sp[[i]], main = paste("Species", i, "- unbiased CP"), range=c(0,1))
  plot(pred_pa_modGS_sp_nocp[[i]], main = paste("Species", i, "- unbiased No CP"), range=c(0,1))
  # store results
  mse[i, 1] <- inverse_probit(prob_pres_sp[[i]], pred_pa_modGS_sp[[i]])
  mse[i, 2] <- inverse_probit(prob_pres_sp[[i]], pred_pa_modGS_sp_nocp[[i]])
  cor[i, 1] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS_sp[[i]])
  cor[i, 2] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS_sp_nocp[[i]])
}
# performance
mse
cor

par(mfrow=c(2,2))
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
partial_response_plot(
  model = mos_modGS_nocp,
  data = pa_model_data_nocp,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_nocp,
  data = pa_model_data_nocp,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# checking models
round(k.check(mos_modGS),2)
round(k.check(mos_modGS_nocp),2)

# biased ########################################################################
pa_model_data_allbiased <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias.csv")
pa_model_data_allbiased$sp <- as.factor(pa_model_data_allbiased$sp)
# species-only data
pa_model_data_allbiased_nocp <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias_nocp.csv")
pa_model_data_allbiased_nocp$sp <- as.factor(pa_model_data_allbiased_nocp$sp)

# model: all data, all smooths
mos_modGS_allbiased <- gam(formula = full_formula, 
                        data = pa_model_data_allbiased, 
                        optimizer=c("outer", "bfgs"), 
                        family = "binomial", 
                        method = "REML")
# model: species data, all smooths
mos_modGS_allbiased_nocp <- gam(formula=full_formula, 
                             data = pa_model_data_allbiased_nocp, 
                             optimizer=c("outer", "bfgs"), 
                             family = "binomial", 
                             method = "REML")
# group
covs$not_complex <- 0
covs$sp <- "group"
# predict based on models
pred_pa_modGS_allbiased_group <- sdm_predict(
  model = mos_modGS_allbiased,
  covariates = covs
)
pred_pa_modGS_allbiased_nocp_group <- sdm_predict(
  model = mos_modGS_allbiased_nocp,
  covariates = covs
)

par(mfrow=c(2,2))

# plot predicted vs true distributions
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_pa_modGS_allbiased_group, main="Dist W/ Complex - biased", range=c(0,1))
plot(pred_pa_modGS_allbiased_nocp_group, main="Dist W/O Complex - biased", range=c(0,1))


# complex
pred_pa_modGS_allbiased_complex <- rast(rep(mad_mask, n_cp))
pred_pa_modGS_allbiased_complex_nocp <- rast(rep(mad_mask, n_cp))
covs$not_complex <- 0
# loop through all complexes
for(i in 1:n_cp){
  covs$sp <- paste0("complex", i)
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  
  # Set current complex to 1
  covs[[paste0("complex", i)]] <- 1
  pred_pa_modGS_allbiased_complex[[i]] <- sdm_predict(
    model = mos_modGS_allbiased,
    covariates = covs
  )
  pred_pa_modGS_allbiased_complex_nocp[[i]] <- sdm_predict(
    model = mos_modGS_allbiased_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(complex_prob_pres[[i]], main = "True Prob of Pres")
  plot(pred_pa_modGS_allbiased_complex[[i]], main = paste("Complex", i, "- biased CP"), range=c(0,1))
  plot(pred_pa_modGS_allbiased_complex_nocp[[i]], main = paste("Complex", i, "- biased No CP"), range=c(0,1))  
}

# species
pred_pa_modGS_allbiased_sp <- rast(rep(mad_mask, num_species))
pred_pa_modGS_allbiased_sp_nocp <- rast(rep(mad_mask, num_species))
covs$not_complex <- 1
# data frames for storing results
mse <- data.frame(CP = 0, NoCP=0)
cor <- data.frame(CP = 0, NoCP=0)
for(i in 1:num_species){
  covs$sp <- factor(i)
  cp <- species[i, 2]
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  # Set current complex to 1
  covs[[paste0("complex", cp)]] <- 1
  pred_pa_modGS_allbiased_sp[[i]] <- sdm_predict(
    model = mos_modGS_allbiased,
    covariates = covs
  )
  pred_pa_modGS_allbiased_sp_nocp[[i]] <- sdm_predict(
    model = mos_modGS_allbiased_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(prob_pres_sp[[i]], main = "True Prob of Pres")
  plot(pred_pa_modGS_allbiased_sp[[i]], main = paste("Species", i, "- biased CP"), range=c(0,1))
  plot(pred_pa_modGS_allbiased_sp_nocp[[i]], main = paste("Species", i, "- biased No CP"), range=c(0,1))
  # store results
  mse[i, 1] <- inverse_probit(prob_pres_sp[[i]], pred_pa_modGS_allbiased_sp[[i]])
  mse[i, 2] <- inverse_probit(prob_pres_sp[[i]], pred_pa_modGS_allbiased_sp_nocp[[i]])
  cor[i, 1] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS_allbiased_sp[[i]])
  cor[i, 2] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS_allbiased_sp_nocp[[i]])
}
# performance
mse
cor

par(mfrow=c(2,2))

# plot partial response plots
partial_response_plot(
  model = mos_modGS_allbiased,
  data = pa_model_data,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_allbiased,
  data = pa_model_data,
  var = "pprec",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_allbiased_nocp,
  data = pa_model_data_allbiased_nocp,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_nocp,
  data = pa_model_data_allbiased_nocp,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# checking models
round(k.check(mos_modGS),2)
round(k.check(mos_modGS_nocp),2)

# presence-only data! **********************************************************
po_model_data <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_rbg_hgam.csv")
po_model_data$sp <- as.factor(po_model_data$sp)
# species-only data
po_model_data_nocp <- read_csv("data/tabular/hgam_multiple_layers/hgam_po_data_med_nocp.csv")
po_model_data_nocp$sp <- as.factor(po_model_data_nocp$sp)

# unbiased #####################################################################
# model: all data, all smooths
mos_modGS_rbg <- gam(formula=full_formula, 
                     data = po_model_data, 
                     optimizer=c("outer", "bfgs"),
                     family = "binomial", 
                     method = "REML")
# model: species data, all smooths
mos_modGS_rbg_nocp <- gam(formula=full_formula, 
                          data = po_model_data_nocp, 
                          optimizer=c("outer", "bfgs"),
                          family = "binomial", 
                          method = "REML")

# group
covs$not_complex <- 0
covs$sp <- "group"
covs$complex1 <- 0
covs$complex2 <- 0
# predict based on models
pred_po_modGS_group <- sdm_predict(
  model = mos_modGS_rbg,
  covariates = covs
)
pred_po_modGS_nocp_group <- sdm_predict(
  model = mos_modGS_rbg_nocp,
  covariates = covs
)

par(mfrow=c(2,2))
# plot predicted vs true distributions
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_po_modGS_group, main="Dist W/ Complex - unbiased")
plot(pred_po_modGS_nocp_group, main="Dist W/O Complex - unbiased")


# complex
pred_po_modGS_complex <- rast(rep(mad_mask, n_cp))
pred_po_modGS_complex_nocp <- rast(rep(mad_mask, n_cp))
covs$not_complex <- 0
# loop through all complexes
for(i in 1:n_cp){
  covs$sp <- paste0("complex", i)
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  
  # Set current complex to 1
  covs[[paste0("complex", i)]] <- 1
  pred_po_modGS_complex[[i]] <- sdm_predict(
    model = mos_modGS_rbg,
    covariates = covs
  )
  pred_po_modGS_complex_nocp[[i]] <- sdm_predict(
    model = mos_modGS_rbg_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(complex_prob_pres[[i]], main = "True Prob of Pres")
  plot(pred_po_modGS_complex[[i]], main = paste("Complex", i, "- unbiased CP"))
  plot(pred_po_modGS_complex_nocp[[i]], main = paste("Complex", i, "- unbiased No CP"))  
}

# species
pred_po_modGS_sp <- rast(rep(mad_mask, num_species))
pred_po_modGS_sp_nocp <- rast(rep(mad_mask, num_species))
covs$not_complex <- 1
# data frame for storing results
cor <- data.frame(CP = 0, NoCP=0)
# loop through all species
for(i in 1:num_species){
  covs$sp <- factor(i)
  cp <- species[i, 2]
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  # Set current complex to 1
  covs[[paste0("complex", cp)]] <- 1
  pred_po_modGS_sp[[i]] <- sdm_predict(
    model = mos_modGS_rbg,
    covariates = covs
  )
  pred_po_modGS_sp_nocp[[i]] <- sdm_predict(
    model = mos_modGS_rbg_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(prob_pres_sp[[i]], main = "True Prob of Pres")
  plot(pred_po_modGS_sp[[i]], main = paste("Species", i, "- unbiased CP"))
  plot(pred_po_modGS_sp_nocp[[i]], main = paste("Species", i, "- unbiased No CP"))
  # store results
  cor[i, 1] <- compute_cor(prob_pres_sp[[i]], pred_po_modGS_sp[[i]])
  cor[i, 2] <- compute_cor(prob_pres_sp[[i]], pred_po_modGS_sp_nocp[[i]])
}
# performance
cor

par(mfrow=c(2,2))
# plot partial response plots
partial_response_plot(
  model = mos_modGS_rbg,
  data = po_model_data,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_rbg,
  data = po_model_data,
  var = "pprec",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_rbg_nocp,
  data = po_model_data_nocp,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_rbg_nocp,
  data = po_model_data_nocp,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# checking models
round(k.check(mos_modGS),2)
round(k.check(mos_modGS_nocp),2)


# biased ########################################################################
po_model_data_allbiased <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_allbiased_rbg_hgam.csv")
po_model_data_allbiased$sp <- as.factor(po_model_data_allbiased$sp)
# species-only data
po_model_data_allbiased_nocp <- read_csv("data/tabular/hgam_multiple_layers/hgam_po_data_med_allbias_nocp.csv")
po_model_data_allbiased_nocp$sp <- as.factor(po_model_data_allbiased_nocp$sp)

# model: all data, all smooths
mos_modGS_allbiased_rbg <- gam(formula=full_formula, 
                            data = po_model_data_allbiased, 
                            optimizer=c("outer", "bfgs"), 
                            family = "binomial", 
                            method = "REML")
# model: species data, all smooths
mos_modGS_allbiased_rbg_nocp <- gam(formula=full_formula, 
                                 data = po_model_data_allbiased_nocp, 
                                 optimizer=c("outer", "bfgs"), 
                                 family = "binomial", 
                                 method = "REML")

# group
covs$not_complex <- 0
covs$complex1 <- 0
covs$complex2 <- 0
covs$sp <- "group"
# predict based on models
pred_po_modGS_allbiased_group <- sdm_predict(
  model = mos_modGS_allbiased_rbg,
  covariates = covs
)
pred_po_modGS_allbiased_nocp_group <- sdm_predict(
  model = mos_modGS_allbiased_rbg_nocp,
  covariates = covs
)

par(mfrow=c(2,2))
# plot predicted vs true distributions
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_po_modGS_allbiased_group, main="Dist W/ Complex - biased")
plot(pred_po_modGS_allbiased_nocp_group, main="Dist W/O Complex - biased")

# complex
pred_po_modGS_allbiased_complex <- rast(rep(mad_mask, n_cp))
pred_po_modGS_allbiased_complex_nocp <- rast(rep(mad_mask, n_cp))
covs$not_complex <- 0
# loop through all complexes
for(i in 1:n_cp){
  covs$sp <- paste0("complex", i)
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  
  # Set current complex to 1
  covs[[paste0("complex", i)]] <- 1
  pred_po_modGS_allbiased_complex[[i]] <- sdm_predict(
    model = mos_modGS_allbiased_rbg,
    covariates = covs
  )
  pred_po_modGS_allbiased_complex_nocp[[i]] <- sdm_predict(
    model = mos_modGS_allbiased_rbg_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(complex_prob_pres[[i]], main = "True Prob of Pres")
  plot(pred_po_modGS_allbiased_complex[[i]], main = paste("Complex", i, "- unbiased CP"))
  plot(pred_po_modGS_allbiased_complex_nocp[[i]], main = paste("Complex", i, "- unbiased No CP"))  
}

# species
pred_po_modGS_allbiased_sp <- rast(rep(mad_mask, num_species))
pred_po_modGS_allbiased_sp_nocp <- rast(rep(mad_mask, num_species))
covs$not_complex <- 1
# data frame for storing results
cor <- data.frame(CP = 0, NoCP=0)
# loop through all species
for(i in 1:num_species){
  covs$sp <- factor(i)
  cp <- species[i, 2]
  for(j in 1:n_cp){
    covs[[paste0("complex", j)]] <- 0
  }
  # Set current complex to 1
  covs[[paste0("complex", cp)]] <- 1
  pred_po_modGS_allbiased_sp[[i]] <- sdm_predict(
    model = mos_modGS_allbiased_rbg,
    covariates = covs
  )
  pred_po_modGS_allbiased_sp_nocp[[i]] <- sdm_predict(
    model = mos_modGS_allbiased_rbg_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot predicted vs true distributions
  plot(prob_pres_sp[[i]], main = "True Prob of Pres")
  plot(pred_po_modGS_alliased_sp[[i]], main = paste("Species", i, "- unbiased CP"))
  plot(pred_po_modGS_allbiased_sp_nocp[[i]], main = paste("Species", i, "- unbiased No CP"))
  # store results
  cor[i, 1] <- compute_cor(prob_pres_sp[[i]], pred_po_modGS_allbiased_sp[[i]])
  cor[i, 2] <- compute_cor(prob_pres_sp[[i]], pred_po_modGS_allbiased_sp_nocp[[i]])
}
# performance
cor

# checking models
round(k.check(mos_modGS_allbiased_rbg),2)
round(k.check(mos_modGS_allbiased_rbg_nocp),2)
