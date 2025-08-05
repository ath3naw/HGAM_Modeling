rm(list = ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
library(mgcv)
source("R/hgam_simplified/functions.R")

# read in variables
prob_pres <- terra::rast("data/grids/spec_prob_pres_hgam.tif")
covs <- terra::rast("data/grids/covariates.tif")
mad_mask <- terra::rast("data/grids/mad_mask.tif")
bias <- terra::rast("data/grids/bias.tif")

pa_tab <- read_csv("data/tabular/hgam_pa_tab_data_med.csv")
n_samples <- nrow(pa_tab)

# calculating group probability of presence from individual species
group_prob_abs <- app(1-prob_pres, fun=prod, na.rm=TRUE)
group_prob_pres <- 1-group_prob_abs
n_sp <- nlyr(prob_pres)

# No bias comparison of model quality (whether more complex data or more individual species data)
pa_model_data_med_nobias_56 <- read_csv("data/tabular/hgam_pa_data_med_nobias_56.csv")
pa_model_data_med_nobias_56$sp <- as.factor(pa_model_data_med_nobias_56$sp)
pa_model_data_med_nobias_23 <- read_csv("data/tabular/hgam_pa_data_med_nobias_23.csv")
pa_model_data_med_nobias_23$sp <- as.factor(pa_model_data_med_nobias_23$sp)
pa_model_data_med_nobias_13 <- read_csv("data/tabular/hgam_pa_data_med_nobias_13.csv")
pa_model_data_med_nobias_13$sp <- as.factor(pa_model_data_med_nobias_13$sp)
pa_model_data_med_nobias_16 <- read_csv("data/tabular/hgam_pa_data_med_nobias_16.csv")
pa_model_data_med_nobias_16$sp <- as.factor(pa_model_data_med_nobias_16$sp)


# Creating the models - unbiased
# group
mos_modGS_med_nobias_56 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                   t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                      by=not_complex), 
                 data = pa_model_data_med_nobias_56, 
                 family = "binomial", 
                 method = "REML")

mos_modGS_med_nobias_23 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                 t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                    by=not_complex), 
                               data = pa_model_data_med_nobias_23, 
                               family = "binomial", 
                               method = "REML")

mos_modGS_med_nobias_13 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                 t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                    by=not_complex), 
                               data = pa_model_data_med_nobias_13, 
                               family = "binomial", 
                               method = "REML")

mos_modGS_med_nobias_16 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                 t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                    by=not_complex), 
                               data = pa_model_data_med_nobias_16, 
                               family = "binomial", 
                               method = "REML")

# group
covs$not_complex <- 0
covs$sp <- "x"
# predict based on models
pred_pa_modGS_group_med_nobias_56 <- sdm_predict(
  model = mos_modGS_med_nobias_56,
  covariates = covs
)
pred_pa_modGS_group_med_nobias_23 <- sdm_predict(
  model = mos_modGS_med_nobias_23,
  covariates = covs
)
pred_pa_modGS_group_med_nobias_13 <- sdm_predict(
  model = mos_modGS_med_nobias_13,
  covariates = covs
)
pred_pa_modGS_group_med_nobias_16 <- sdm_predict(
  model = mos_modGS_med_nobias_16,
  covariates = covs
)


par(mfrow=c(2,3))
# plot real vs predicted models
plot(group_prob_pres, main=paste("Group Prob of Pres -", n_samples, "points"), range=c(0,1))
plot(pred_pa_modGS_group_med_nobias_56, main="5/6 Complex Pred_Group_Dist", range=c(0,1))
plot(pred_pa_modGS_group_med_nobias_23, main="2/3 Complex Pred_Group_Dist", range=c(0,1))
plot(pred_pa_modGS_group_med_nobias_13, main="1/3 Complex Pred_Group_Dist", range=c(0,1))
plot(pred_pa_modGS_group_med_nobias_16, main="1/6 Complex Pred_Group_Dist", range=c(0,1))

# species
covs$not_complex <- 1
pred_pa_modGS_med_nobias_56 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_med_nobias_23 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_med_nobias_13 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_med_nobias_16 <- rast(rep(mad_mask, n_sp))

# Plot all species
for(letter in letters[1:n_sp]){
  covs$sp <- letter
  i <- match(letter, letters)
  pred_pa_modGS_med_nobias_56[[i]] <- sdm_predict(
    model = mos_modGS_med_nobias_56,
    covariates = covs
  )
  pred_pa_modGS_med_nobias_23[[i]] <- sdm_predict(
    model = mos_modGS_med_nobias_23,
    covariates = covs
  )
  pred_pa_modGS_med_nobias_13[[i]] <- sdm_predict(
    model = mos_modGS_med_nobias_13,
    covariates = covs
  )
  pred_pa_modGS_med_nobias_16[[i]] <- sdm_predict(
    model = mos_modGS_med_nobias_16,
    covariates = covs
  )
  par(mfrow=c(2,3))
  # plot real vs predicted models
  plot(prob_pres[[i]], main = paste("True Prob of Pres -", n_samples, "points"), range=c(0,1))
  plot(pred_pa_modGS_med_nobias_56[[i]], main = paste("5/6 Complex - Species", letter), range=c(0,1))
  plot(pred_pa_modGS_med_nobias_23[[i]], main = paste("2/3 Complex - Species", letter), range=c(0,1))
  plot(pred_pa_modGS_med_nobias_13[[i]], main = paste("1/3 Complex - Species", letter), range=c(0,1))
  plot(pred_pa_modGS_med_nobias_16[[i]], main = paste("1/6 Complex - Species", letter), range=c(0,1))
}


par(mfrow=c(2,2))
# plot partial response plots for varying data quality
partial_response_plot(
  model = mos_modGS_med_nobias_56,
  data = pa_model_data_med_nobias_56,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_med_nobias_23,
  data = pa_model_data_med_nobias_23,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_med_nobias_13,
  data = pa_model_data_med_nobias_13,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_med_nobias_16,
  data = pa_model_data_med_nobias_16,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)

partial_response_plot(
  model = mos_modGS_med_nobias_56,
  data = pa_model_data_med_nobias_56,
  var = "pprec",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_med_nobias_23,
  data = pa_model_data_med_nobias_23,
  var = "pprec",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_med_nobias_13,
  data = pa_model_data_med_nobias_13,
  var = "pprec",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_med_nobias_16,
  data = pa_model_data_med_nobias_16,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# checking models
round(k.check(mos_modGS_med_nobias_56),2)
round(k.check(mos_modGS_med_nobias_23),2)
round(k.check(mos_modGS_med_nobias_13),2)
round(k.check(mos_modGS_med_nobias_16),2)

