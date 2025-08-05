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

# to know number of sampling locations in case it changed from simulation
pa_tab_min <- read_csv("data/tabular/hgam_pa_tab_data_min.csv")
n_samples_min <- nrow(pa_tab_min)
pa_tab_med <- read_csv("data/tabular/hgam_pa_tab_data_med.csv")
n_samples_med <- nrow(pa_tab_med)
pa_tab_max <- read_csv("data/tabular/hgam_pa_tab_data_max.csv")
n_samples_max <- nrow(pa_tab_max)

# calculating group probability of presence from individual species
group_prob_abs <- app(1-prob_pres, fun=prod, na.rm=TRUE)
group_prob_pres <- 1-group_prob_abs
n_sp <- nlyr(prob_pres)

# No bias comparison of number of data points
# 2/3 complex
pa_model_data_min_nobias_23 <- read_csv("data/tabular/hgam_pa_data_min_nobias_23.csv")
pa_model_data_min_nobias_23$sp <- as.factor(pa_model_data_min_nobias_23$sp)
pa_model_data_med_nobias_23 <- read_csv("data/tabular/hgam_pa_data_med_nobias_23.csv")
pa_model_data_med_nobias_23$sp <- as.factor(pa_model_data_med_nobias_23$sp)
pa_model_data_max_nobias_23 <- read_csv("data/tabular/hgam_pa_data_max_nobias_23.csv")
pa_model_data_max_nobias_23$sp <- as.factor(pa_model_data_max_nobias_23$sp)

# 1/3 complex
pa_model_data_min_nobias_13 <- read_csv("data/tabular/hgam_pa_data_min_nobias_13.csv")
pa_model_data_min_nobias_13$sp <- as.factor(pa_model_data_min_nobias_13$sp)
pa_model_data_med_nobias_13 <- read_csv("data/tabular/hgam_pa_data_med_nobias_13.csv")
pa_model_data_med_nobias_13$sp <- as.factor(pa_model_data_med_nobias_13$sp)
pa_model_data_max_nobias_13 <- read_csv("data/tabular/hgam_pa_data_max_nobias_13.csv")
pa_model_data_max_nobias_13$sp <- as.factor(pa_model_data_max_nobias_13$sp)


# min = 100 data points, med = 300 data points, max = 900 data points, all 2/3 complex
mos_modGS_min_nobias_23 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                 t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                    by=not_complex), 
                               data = pa_model_data_min_nobias_23, 
                               family = "binomial", 
                               method = "REML")
mos_modGS_med_nobias_23 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                 t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                    by=not_complex), 
                               data = pa_model_data_med_nobias_23, 
                               family = "binomial", 
                               method = "REML")
mos_modGS_max_nobias_23 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                 t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                    by=not_complex), 
                               data = pa_model_data_max_nobias_23, 
                               family = "binomial", 
                               method = "REML")

# group
covs$not_complex <- 0
covs$sp <- "x"
# predict based on models
pred_pa_modGS_group_min_nobias_23 <- sdm_predict(
  model = mos_modGS_min_nobias_23,
  covariates = covs
)
pred_pa_modGS_group_med_nobias_23 <- sdm_predict(
  model = mos_modGS_med_nobias_23,
  covariates = covs
)
pred_pa_modGS_group_max_nobias_23 <- sdm_predict(
  model = mos_modGS_max_nobias_23,
  covariates = covs
)

par(mfrow=c(2,2))
# plot real vs predicted models
plot(group_prob_pres, main=paste("Group Prob of Pres"), range=c(0,1))
plot(pred_pa_modGS_group_min_nobias_23, main=paste("2/3 Complex Pred_Group_Dist -", n_samples_min, "points"), range=c(0,1))
plot(pred_pa_modGS_group_med_nobias_23, main=paste("2/3 Complex Pred_Group_Dist -", n_samples_med, "points"), range=c(0,1))
plot(pred_pa_modGS_group_max_nobias_23, main=paste("2/3 Complex Pred_Group_Dist -", n_samples_max, "points"), range=c(0,1))

# species
covs$not_complex <- 1
pred_pa_modGS_min_nobias_23 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_med_nobias_23 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_max_nobias_23 <- rast(rep(mad_mask, n_sp))

# Plot all species
for(letter in letters[1:n_sp]){
  covs$sp <- letter
  i <- match(letter, letters)
  pred_pa_modGS_min_nobias_23[[i]] <- sdm_predict(
    model = mos_modGS_min_nobias_23,
    covariates = covs
  )
  pred_pa_modGS_med_nobias_23[[i]] <- sdm_predict(
    model = mos_modGS_med_nobias_23,
    covariates = covs
  )
  pred_pa_modGS_max_nobias_23[[i]] <- sdm_predict(
    model = mos_modGS_max_nobias_23,
    covariates = covs
  )

  par(mfrow=c(2,2))
  # plot real vs predicted models
  plot(prob_pres[[i]], main = paste("True Prob of Pres - Species", letter), range=c(0,1))
  plot(pred_pa_modGS_min_nobias_23[[i]], main = paste("2/3 Complex -", n_samples_min, "points"), range=c(0,1))
  plot(pred_pa_modGS_med_nobias_23[[i]], main = paste("2/3 Complex -", n_samples_med, "points"), range=c(0,1))
  plot(pred_pa_modGS_max_nobias_23[[i]], main = paste("2/3 Complex -", n_samples_max, "points"), range=c(0,1))
}


par(mfrow=c(2,2))
# plot partial response plots
partial_response_plot(
  model = mos_modGS_min_nobias_23,
  data = pa_model_data_min_nobias_23,
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
  model = mos_modGS_max_nobias_23,
  data = pa_model_data_max_nobias_23,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)

par(mfrow=c(2,2))
partial_response_plot(
  model = mos_modGS_min_nobias_23,
  data = pa_model_data_min_nobias_23,
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
  model = mos_modGS_max_nobias_23,
  data = pa_model_data_max_nobias_23,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# checking models
round(k.check(mos_modGS_min_nobias_23),2)
round(k.check(mos_modGS_med_nobias_23),2)
round(k.check(mos_modGS_max_nobias_23),2)



# min = 100 data points, med = 300 data points, max = 900 data points, all 1/3 complex
mos_modGS_min_nobias_13 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                 t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                    by=not_complex), 
                               data = pa_model_data_min_nobias_13, 
                               family = "binomial", 
                               method = "REML")
mos_modGS_med_nobias_13 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                 t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                    by=not_complex), 
                               data = pa_model_data_med_nobias_13, 
                               family = "binomial", 
                               method = "REML")
mos_modGS_max_nobias_13 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                 t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                    by=not_complex), 
                               data = pa_model_data_max_nobias_13, 
                               family = "binomial", 
                               method = "REML")

# group
covs$not_complex <- 0
covs$sp <- "x"
# predict based on models
pred_pa_modGS_group_min_nobias_13 <- sdm_predict(
  model = mos_modGS_min_nobias_13,
  covariates = covs
)
pred_pa_modGS_group_med_nobias_13 <- sdm_predict(
  model = mos_modGS_med_nobias_13,
  covariates = covs
)
pred_pa_modGS_group_max_nobias_13 <- sdm_predict(
  model = mos_modGS_max_nobias_13,
  covariates = covs
)

par(mfrow=c(2,2))
# plot real vs predicted models
plot(group_prob_pres, main=paste("Group Prob of Pres"), range=c(0,1))
plot(pred_pa_modGS_group_min_nobias_13, main=paste("1/3 Complex Pred_Group_Dist -", n_samples_min, "points"), range=c(0,1))
plot(pred_pa_modGS_group_med_nobias_13, main=paste("1/3 Complex Pred_Group_Dist -", n_samples_med, "points"), range=c(0,1))
plot(pred_pa_modGS_group_max_nobias_13, main=paste("1/3 Complex Pred_Group_Dist -", n_samples_max, "points"), range=c(0,1))

# group
covs$not_complex <- 1
pred_pa_modGS_min_nobias_13 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_med_nobias_13 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_max_nobias_13 <- rast(rep(mad_mask, n_sp))

# Plot all species
for(letter in letters[1:n_sp]){
  covs$sp <- letter
  i <- match(letter, letters)
  pred_pa_modGS_min_nobias_13[[i]] <- sdm_predict(
    model = mos_modGS_min_nobias_13,
    covariates = covs
  )
  pred_pa_modGS_med_nobias_13[[i]] <- sdm_predict(
    model = mos_modGS_med_nobias_13,
    covariates = covs
  )
  pred_pa_modGS_max_nobias_13[[i]] <- sdm_predict(
    model = mos_modGS_max_nobias_13,
    covariates = covs
  )
  
  par(mfrow=c(2,2))
  # plot real vs predicted models
  plot(prob_pres[[i]], main = paste("True Prob of Pres - Species", letter), range=c(0,1))
  plot(pred_pa_modGS_min_nobias_13[[i]], main = paste("1/3 Complex -", n_samples_min, "points"), range=c(0,1))
  plot(pred_pa_modGS_med_nobias_13[[i]], main = paste("1/3 Complex -", n_samples_med, "points"), range=c(0,1))
  plot(pred_pa_modGS_max_nobias_13[[i]], main = paste("1/3 Complex -", n_samples_max, "points"), range=c(0,1))
}


par(mfrow=c(2,2))
# plot partial response plots
partial_response_plot(
  model = mos_modGS_min_nobias_13,
  data = pa_model_data_min_nobias_13,
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
  model = mos_modGS_max_nobias_13,
  data = pa_model_data_max_nobias_13,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)

par(mfrow=c(2,2))
partial_response_plot(
  model = mos_modGS_min_nobias_13,
  data = pa_model_data_min_nobias_13,
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
  model = mos_modGS_max_nobias_13,
  data = pa_model_data_max_nobias_13,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# checking models
round(k.check(mos_modGS_min_nobias_13),2)
round(k.check(mos_modGS_med_nobias_13),2)
round(k.check(mos_modGS_max_nobias_13),2)

par(mfrow=c(3,3))
# plot real vs predicted models, comparing data quality predictions
# group
plot(group_prob_pres, main=paste("Group Prob of Pres"), range=c(0,1))
plot(pred_pa_modGS_group_min_nobias_23, main=paste("2/3 Complex Pred_Group_Dist -", n_samples_min, "points"), range=c(0,1))
plot(pred_pa_modGS_group_med_nobias_23, main=paste("2/3 Complex Pred_Group_Dist -", n_samples_med, "points"), range=c(0,1))
plot(pred_pa_modGS_group_max_nobias_23, main=paste("2/3 Complex Pred_Group_Dist -", n_samples_max, "points"), range=c(0,1))
plot(pred_pa_modGS_group_min_nobias_13, main=paste("1/3 Complex Pred_Group_Dist -", n_samples_min, "points"), range=c(0,1))
plot(pred_pa_modGS_group_med_nobias_13, main=paste("1/3 Complex Pred_Group_Dist -", n_samples_med, "points"), range=c(0,1))
plot(pred_pa_modGS_group_max_nobias_13, main=paste("1/3 Complex Pred_Group_Dist -", n_samples_max, "points"), range=c(0,1))

# plot real vs predicted models, comparing data quality predictions
# species
for(letter in letters[1:n_sp]){
  covs$sp <- letter
  i <- match(letter, letters)
  
  par(mfrow=c(3,3))
  # plot real vs predicted models, comparing min, med, and max amount of data for 2/3 and 1/3 complexes
  plot(prob_pres[[i]], main = paste("True Prob of Pres - Species", letter), range=c(0,1))
  plot(pred_pa_modGS_min_nobias_23[[i]], main = paste("2/3 Complex -", n_samples_min, "points"), range=c(0,1))
  plot(pred_pa_modGS_med_nobias_23[[i]], main = paste("2/3 Complex -", n_samples_med, "points"), range=c(0,1))
  plot(pred_pa_modGS_max_nobias_23[[i]], main = paste("2/3 Complex -", n_samples_max, "points"), range=c(0,1))
  plot(pred_pa_modGS_min_nobias_13[[i]], main = paste("1/3 Complex -", n_samples_min, "points"), range=c(0,1))
  plot(pred_pa_modGS_med_nobias_13[[i]], main = paste("1/3 Complex -", n_samples_med, "points"), range=c(0,1))
  plot(pred_pa_modGS_max_nobias_13[[i]], main = paste("1/3 Complex -", n_samples_max, "points"), range=c(0,1))
}


