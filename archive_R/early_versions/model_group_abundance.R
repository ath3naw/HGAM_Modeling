# ****************************** TESTING CODE ************************************
# No real use here
rm(list = ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
library(mgcv)
source("R/hgam_simplified/functions.R")
# read in variables
pa_model_data <- read_csv("data/tabular/hgam_pa_data.csv")
pa_model_data$sp <- as.factor(pa_model_data$sp)
prob_pres <- terra::rast("data/grids/spec_prob_pres_hgam.tif")

# calculating group probability of presence
group_prob_abs <- app(1-prob_pres, fun=prod, na.rm=TRUE)
group_prob_pres <- 1-group_prob_abs

# Creating the models, only temperature
mos_modGS <- gam(pa ~ s(ttemp) + 
                   s(ttemp, sp, bs = "fs", by=not_complex), 
                 data = pa_model_data, 
                 family = "binomial", 
                 method = "REML")
plot(mos_modGS, xlab = "temperature")

# group
covs$not_complex <- 0
covs$sp <- "x"
# predict based on model
pred_pa_modGS <- sdm_predict(
  model = mos_modGS,
  covariates = covs
)
# plot real vs predicted models
par(mfrow=c(1,1))
plot(pred_pa_modGS)

par(mfrow=c(1,2))
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_pa_modGS, main="predicted_distribution", range=c(0,1))

# species
covs$not_complex <- 1
# Plot all species
for(letter in letters[1:10]){
  covs$sp <- letter
  pred_pa_modGS <- sdm_predict(
    model = mos_modGS,
    covariates = covs
  )
  par(mfrow=c(1,2))
  # plot real vs predicted models
  plot(prob_pres[[match(letter, letters)]], main = "True Prob of Pres", range=c(0,1))
  plot(pred_pa_modGS, main = paste("Species ", letter), range=c(0,1))
}

par(mfrow=c(1,1))


# Creating the models
# group, tried to do it with 3 variables
mos_modGS <- gam(pa ~ te(ttemp, tseas, pprec, bs=c("tp", "tp", "tp", "tp")) + 
                   t2(ttemp, tseas, pprec, sp, bs=c("tp", "tp", "tp", "tp", "re"),
                      by=not_complex), 
                 data = pa_model_data, 
                 family = "binomial", 
                 method = "REML")

# group
covs$not_complex <- 0
covs$sp <- "x"
# specify the species
pred_pa_modGS <- sdm_predict(
  model = mos_modGS,
  covariates = covs
)
par(mfrow=c(1,1))
plot(pred_pa_modGS)
par(mfrow=c(1,2))
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_pa_modGS, main="predicted_distribution", range=c(0,1))

# species
covs$not_complex <- 1
# Plot all species
for(letter in letters[1:10]){
  covs$sp <- letter
  pred_pa_modGS <- sdm_predict(
    model = mos_modGS,
    covariates = covs
  )
  par(mfrow=c(1,2))
  # plot real vs predicted models
  plot(prob_pres[[match(letter, letters)]], main = "True Prob of Pres", range=c(0,1))
  plot(pred_pa_modGS, main = paste("Species ", letter), range=c(0,1))
}

par(mfrow=c(1,1))

# GI model
pa_practice <- pa_model_data |>
  mutate(sp_indiv= ifelse(not_complex == 1, as.character(sp), NA)) |>
  mutate(sp_indiv = factor(sp_indiv))
mos_modGI <- gam(pa ~ s(ttemp, bs="tp") +
                   s(ttemp, by=sp_indiv, bs="tp") +
                   s(sp, by= not_complex, bs="re"),
                 data=pa_practice,
                 family = "binomial",
                 method="REML")


# Use one consistent level for each factor
covs$sp_indiv <- factor(levels(pa_practice$sp_indiv)[1],
                             levels = levels(pa_practice$sp_indiv))

covs$sp <- factor(levels(pa_practice$sp)[1],
                       levels = levels(pa_practice$sp))

covs$not_complex <- 0  # or 0 if more appropriate

# Custom predict function to extract only the s(ttemp) term
predict_s_ttemp <- function(model, newdata) {
  predict(model, newdata = newdata, type = "terms", terms = "s(ttemp)")
}

# Apply prediction function to raster
pred_pa_modGI <- predict(covs, mos_modGI, fun = predict_s_ttemp)

plot(ttemp_effect_raster, main = "Effect of ttemp (s(ttemp))")
par(mfrow=c(1,2))
plot(group_prob_pres, main="True prob of pres")
plot(pred_pa_modGI, main="predicted_distribution")

covs$not_complex <- 1
for(letter in letters[1:10]){
  covs$sp <- letter
  pred_pa_modGI <- sdm_predict(
    model = mos_modGI,
    covariates = covs
  )
  par(mfrow=c(1,2))
  plot(prob_pres[[match(letter, letters)]], main = "True Distribution")
  plot(pred_pa_modGI, main = paste("Species ", letter))
}



# Custom function to only return global smooth
predict_global_smooth <- function(model, data, ...) {
  # Prepare data for prediction
  data$sp <- factor("x", levels = levels(pa_practice$sp))
  data$not_complex <- 0
  data$sp_indiv <- NA
  data$sp_indiv <- factor(data$sp_indiv, levels = levels(pa_practice$sp_indiv))
  
  # Predict with excluded smooth
  predict(model, newdata = data, type = "response")
}

pred_raster_global <- terra::predict(
  object = covs,
  model = mos_modGI,
  fun = predict_global_smooth,
  type = "response"
)

pred_pa_modGI <- sdm_predict(
  model = mos_modGI,
  covariates = covs
)
plot(pred_pa_modGI)
# mos_modGI <- gam(pa ~ sp*not_complex +
#                    te(ttemp, pprec, bs=c("tp","tp")) +
#                    te(ttemp, pprec, by=sp, bs=c("tp","tp")),
#                  data=pa_model_data,
#                  family="binomial",
#                  method="REML")

