rm(list = ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
library(mgcv)
source("R/hgam_simplified/functions.R")
# note: no group = species-only data in this case since there are only
# group and species layers

# read in variables
prob_pres <- terra::rast("data/grids/spec_prob_pres_hglm.tif")
covs <- terra::rast("data/grids/covariates.tif")
mad_mask <- terra::rast("data/grids/mad_mask.tif")
bias <- terra::rast("data/grids/bias.tif")
spbias <- terra::rast("data/grids/sp_bias.tif") # can comment out whichever one
max_catch_size <- 20

pa_tab <- read_csv("data/tabular/hglm_pa_tab_data_med.csv")
pa_tab_biased <- read_csv("data/tabular/hglm_pa_tab_data_med_biased.csv")

# calculating group probability of presence from individual species
group_prob_abs <- app(1-prob_pres, fun=prod, na.rm=TRUE)
group_prob_pres <- 1-group_prob_abs
n_sp <- nlyr(prob_pres)


# presence-absence data! ********************************************************
pa_model_data <- read_csv("data/tabular/hglm_pa_data_med_nobias_23.csv")
pa_model_data$sp <- as.factor(pa_model_data$sp)
# no group data
pa_model_data_nocp <- read_csv("data/tabular/hglm_pa_data_med_nobias_23_nocp.csv")
pa_model_data_nocp$sp <- as.factor(pa_model_data_nocp$sp)

# unbiased ######################################################################
mos_modGS <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                   t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                      by=not_complex), 
                 data = pa_model_data, 
                 family = "binomial", 
                 method = "REML")
# using species-only data
mos_modGS_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                   t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                      by=not_complex), 
                 data = pa_model_data_nocp, 
                 family = "binomial", 
                 method = "REML")

# group
covs$not_complex <- 0
covs$sp <- "x"
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
# plot real vs predicted models
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_pa_modGS_group, main="Dist W/ Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_nocp_group, main="Dist W/O Complex - unbiased", range=c(0,1))

# species
pred_pa_modGS <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_nocp <- rast(rep(mad_mask, n_sp))
covs$not_complex <- 1
# data frames to hold results
mse <- data.frame(CP = 0, NoCP=0)
cor <- data.frame(CP = 0, NoCP=0)
# Plot all species
for(letter in letters[1:n_sp]){
  covs$sp <- letter
  i <- match(letter, letters)
  pred_pa_modGS[[i]] <- sdm_predict(
    model = mos_modGS,
    covariates = covs
  )
  pred_pa_modGS_nocp[[i]] <- sdm_predict(
    model = mos_modGS_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot real vs predicted models
  plot(prob_pres[[i]], main = "True Prob of Pres", range=c(0,1))
  plot(pred_pa_modGS[[i]], main = paste("Species", letter, "- unbiased CP"), range=c(0,1))
  plot(pred_pa_modGS_nocp[[i]], main = paste("Species", letter, "- unbiased No CP"), range=c(0,1))
  # store results
  mse[i, 1] <- inverse_probit(prob_pres[[i]], pred_pa_modGS[[i]])
  mse[i, 2] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_nocp[[i]])
  cor[i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS[[i]])
  cor[i, 2] <- compute_cor(prob_pres[[i]], pred_pa_modGS_nocp[[i]])
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
pa_model_data_allbiased <- read_csv("data/tabular/hglm_pa_data_med_allbias_23.csv")
pa_model_data_allbiased$sp <- as.factor(pa_model_data_allbiased$sp)
# no group data
pa_model_data_allbiased_nocp <- read_csv("data/tabular/hglm_pa_data_med_allbias_23_nocp.csv")
pa_model_data_allbiased_nocp$sp <- as.factor(pa_model_data_allbiased_nocp$sp)

mos_modGS_allbiased <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                   t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                      by=not_complex), 
                 data = pa_model_data_allbiased, 
                 family = "binomial", 
                 method = "REML")
# using species-only data
mos_modGS_allbiased_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                        t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                           by=not_complex), 
                      data = pa_model_data_allbiased_nocp, 
                      family = "binomial", 
                      method = "REML")

# group
covs$not_complex <- 0
covs$sp <- "x"
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
# plot real vs predicted models
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_pa_modGS_allbiased_group, main="Dist W/ Complex - biased", range=c(0,1))
plot(pred_pa_modGS_allbiased_nocp_group, main="Dist W/O Complex - biased", range=c(0,1))

# species
pred_pa_modGS_allbiased <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_allbiased_nocp <- rast(rep(mad_mask, n_sp))
covs$not_complex <- 1
# data frames to store results
mse <- data.frame(CP = 0, NoCP=0)
cor <- data.frame(CP = 0, NoCP=0)
# Plot all species
for(letter in letters[1:n_sp]){
  covs$sp <- letter
  i <- match(letter, letters)
  pred_pa_modGS_allbiased[[i]] <- sdm_predict(
    model = mos_modGS_allbiased,
    covariates = covs
  )
  pred_pa_modGS_allbiased_nocp[[i]] <- sdm_predict(
    model = mos_modGS_allbiased_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot real vs predicted models
  plot(prob_pres[[i]], main = "True Prob of Pres", range=c(0,1))
  plot(pred_pa_modGS_allbiased[[i]], main = paste("Species", letter, "- biased CP"), range=c(0,1))
  plot(pred_pa_modGS_allbiased_nocp[[i]], main = paste("Species", letter, "- biased No CP"), range=c(0,1))
  # store results
  mse[i, 1] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_allbiased[[i]])
  mse[i, 2] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_allbiased_nocp[[i]])
  cor[i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS_allbiased[[i]])
  cor[i, 2] <- compute_cor(prob_pres[[i]], pred_pa_modGS_allbiased_nocp[[i]])
}
# performance
mse
cor

par(mfrow=c(2,2))
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
partial_response_plot(
  model = mos_modGS_allbiased_nocp,
  data = pa_model_data_allbiased_nocp,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_allbiased_nocp,
  data = pa_model_data_allbiased_nocp,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# checking models
round(k.check(mos_modGS_biased),2)
round(k.check(mos_modGS_biased_nocp),2)
round(k.check(mos_modGS_spbiased),2)
round(k.check(mos_modGS_spbiased_nocp),2)

# presence-only data! ***********************************************************
po_model_data <- read_csv("data/tabular/presence_only_data_rbg_hgam.csv")
po_model_data$sp <- as.factor(po_model_data$sp)
# no group data
po_model_data_nocp <- read_csv("data/tabular/presence_only_data_rbg_hgam_nocp.csv")
po_model_data_nocp$sp <- as.factor(po_model_data_nocp$sp)

# unbiased ######################################################################
mos_modGS_rbg <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                   t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                      by=not_complex), 
                 data = po_model_data, 
                 family = "binomial", 
                 method = "REML")
# using species-only data
mos_modGS_rbg_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                        t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                           by=not_complex), 
                      data = po_model_data_nocp, 
                      family = "binomial", 
                      method = "REML")

# group
covs$not_complex <- 0
covs$sp <- "x"
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
# plot real vs predicted models
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_po_modGS_group, main="Dist W/ Complex - unbiased")
plot(pred_po_modGS_nocp_group, main="Dist W/O Complex - unbiased")

# species
pred_po_modGS <- rast(rep(mad_mask, n_sp))
pred_po_modGS_nocp <- rast(rep(mad_mask, n_sp))
covs$not_complex <- 1
# data frame to store results
cor <- data.frame(CP = 0, NoCP=0)
# Plot all species
for(letter in letters[1:n_sp]){
  covs$sp <- letter
  i <- match(letter, letters)
  pred_po_modGS[[i]] <- sdm_predict(
    model = mos_modGS_rbg,
    covariates = covs
  )
  pred_po_modGS_nocp[[i]] <- sdm_predict(
    model = mos_modGS_rbg_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot real vs predicted models
  plot(prob_pres[[i]], main = "True Prob of Pres", range=c(0,1))
  plot(pred_po_modGS[[i]], main = paste("Species", letter, "- unbiased CP"))
  plot(pred_po_modGS_nocp[[i]], main = paste("Species", letter, "- unbiased No CP"))
  # store results
  cor[i, 1] <- compute_cor_po(prob_pres[[i]], pred_po_modGS[[i]])
  cor[i, 2] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_nocp[[i]])
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
round(k.check(mos_modGS_rbg),2)
round(k.check(mos_modGS_rbg_nocp),2)

# biased ########################################################################
po_model_data_allbiased <- read_csv("data/tabular/presence_only_data_allbiased_rbg_hgam.csv")
po_model_data_allbiased$sp <- as.factor(po_model_data_allbiased$sp)
# no group data
po_model_data_allbiased_nocp <- read_csv("data/tabular/presence_only_data_allbiased_rbg_hgam_nocp.csv")
po_model_data_allbiased_nocp$sp <- as.factor(po_model_data_allbiased_nocp$sp)

mos_modGS_allbiased_rbg <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                          t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                             by=not_complex), 
                        data = po_model_data_allbiased, 
                        family = "binomial", 
                        method = "REML")
# using species-only data
mos_modGS_allbiased_rbg_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                               t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                  by=not_complex), 
                             data = po_model_data_allbiased_nocp, 
                             family = "binomial", 
                             method = "REML")

# group
covs$not_complex <- 0
covs$sp <- "x"
# predicts based on models
pred_po_modGS_allbiased_group <- sdm_predict(
  model = mos_modGS_allbiased_rbg,
  covariates = covs
)
pred_po_modGS_allbiased_nocp_group <- sdm_predict(
  model = mos_modGS_allbiased_rbg_nocp,
  covariates = covs
)

par(mfrow=c(2,2))
# plot real vs predicted models
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_po_modGS_allbiased_group, main="Dist W/ Complex - biased")
plot(pred_po_modGS_allbiased_nocp_group, main="Dist W/O Complex - biased")

# species
pred_po_modGS_allbiased <- rast(rep(mad_mask, n_sp))
pred_po_modGS_allbiased_nocp <- rast(rep(mad_mask, n_sp))
covs$not_complex <- 1
# data frame to store results
cor <- data.frame(CP = 0, NoCP=0)
# Plot all species
for(letter in letters[1:n_sp]){
  covs$sp <- letter
  i <- match(letter, letters)
  pred_po_modGS_allbiased[[i]] <- sdm_predict(
    model = mos_modGS_allbiased_rbg,
    covariates = covs
  )
  pred_po_modGS_allbiased_nocp[[i]] <- sdm_predict(
    model = mos_modGS_allbiased_rbg_nocp,
    covariates = covs
  )
  par(mfrow=c(2,2))
  # plot real vs predicted models
  plot(prob_pres[[i]], main = "True Prob of Pres", range=c(0,1))
  plot(pred_po_modGS_allbiased[[i]], main = paste("Species", letter, "- biased CP"))
  plot(pred_po_modGS_allbiased_nocp[[i]], main = paste("Species", letter, "- biased No CP"))
  # store results
  cor[i, 1] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_allbiased[[i]])
  cor[i, 2] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_allbiased_nocp[[i]])
}
# performance
cor

par(mfrow=c(2,2))

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
partial_response_plot(
  model = mos_modGS_allbiased_nocp,
  data = pa_model_data_allbiased_nocp,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_allbiased_nocp,
  data = pa_model_data_allbiased_nocp,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# checking models
round(k.check(mos_modGS_biased),2)
round(k.check(mos_modGS_biased_nocp),2)
round(k.check(mos_modGS_spbiased),2)
round(k.check(mos_modGS_spbiased_nocp),2)


### checking data quality *******************************************************
pa_model_data_56 <- read_csv("data/tabular/hglm_pa_data_med_nobias_56.csv")
pa_model_data_56$sp <- as.factor(pa_model_data_56$sp)
# no group data
pa_model_data_56_nocp <- read_csv("data/tabular/hglm_pa_data_med_nobias_56_nocp.csv")
pa_model_data_56_nocp$sp <- as.factor(pa_model_data_56_nocp$sp)

pa_model_data_23 <- read_csv("data/tabular/hglm_pa_data_med_nobias_23.csv")
pa_model_data_23$sp <- as.factor(pa_model_data_23$sp)
# no group data
pa_model_data_23_nocp <- read_csv("data/tabular/hglm_pa_data_med_nobias_23_nocp.csv")
pa_model_data_23_nocp$sp <- as.factor(pa_model_data_23_nocp$sp)

pa_model_data_13 <- read_csv("data/tabular/hglm_pa_data_med_nobias_13.csv")
pa_model_data_13$sp <- as.factor(pa_model_data_13$sp)
# no group data
pa_model_data_13_nocp <- read_csv("data/tabular/hglm_pa_data_med_nobias_13_nocp.csv")
pa_model_data_13_nocp$sp <- as.factor(pa_model_data_13_nocp$sp)

pa_model_data_16 <- read_csv("data/tabular/hglm_pa_data_med_nobias_16.csv")
pa_model_data_16$sp <- as.factor(pa_model_data_16$sp)
# no group data
pa_model_data_16_nocp <- read_csv("data/tabular/hglm_pa_data_med_nobias_16_nocp.csv")
pa_model_data_16_nocp$sp <- as.factor(pa_model_data_16_nocp$sp)

# unbiased ######################################################################
mos_modGS_56 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                      t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                         by=not_complex), 
                    data = pa_model_data_56, 
                    family = "binomial", 
                    method = "REML")
# using species-only data
mos_modGS_56_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                           t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                              by=not_complex), 
                         data = pa_model_data_56_nocp, 
                         family = "binomial", 
                         method = "REML")

mos_modGS_23 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                      t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                         by=not_complex), 
                    data = pa_model_data_23, 
                    family = "binomial", 
                    method = "REML")
# using species-only data
mos_modGS_23_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                           t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                              by=not_complex), 
                         data = pa_model_data_23_nocp, 
                         family = "binomial", 
                         method = "REML")

mos_modGS_13 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                      t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                         by=not_complex), 
                    data = pa_model_data_13, 
                    family = "binomial", 
                    method = "REML")
# using species-only data
mos_modGS_13_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                           t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                              by=not_complex), 
                         data = pa_model_data_13_nocp, 
                         family = "binomial", 
                         method = "REML")

mos_modGS_16 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                      t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                         by=not_complex), 
                    data = pa_model_data_16, 
                    family = "binomial", 
                    method = "REML")
# using species-only data
mos_modGS_16_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                           t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                              by=not_complex), 
                         data = pa_model_data_16_nocp, 
                         family = "binomial", 
                         method = "REML")

# species
covs$not_complex <- 0
covs$sp <- "x"
# predict based on models
pred_pa_modGS_56_group <- sdm_predict(
  model = mos_modGS_56,
  covariates = covs
)
pred_pa_modGS_56_nocp_group <- sdm_predict(
  model = mos_modGS_56_nocp,
  covariates = covs
)

pred_pa_modGS_23_group <- sdm_predict(
  model = mos_modGS_23,
  covariates = covs
)
pred_pa_modGS_23_nocp_group <- sdm_predict(
  model = mos_modGS_23_nocp,
  covariates = covs
)

pred_pa_modGS_13_group <- sdm_predict(
  model = mos_modGS_13,
  covariates = covs
)
pred_pa_modGS_13_nocp_group <- sdm_predict(
  model = mos_modGS_13_nocp,
  covariates = covs
)

pred_pa_modGS_16_group <- sdm_predict(
  model = mos_modGS_16,
  covariates = covs
)
pred_pa_modGS_16_nocp_group <- sdm_predict(
  model = mos_modGS_16_nocp,
  covariates = covs
)

par(mfrow=c(2,2))
# plot real vs predicted models
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_pa_modGS_56_group, main="Dist W/ 5/6 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_56_nocp_group, main="Dist W/O 5/6 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_23_group, main="Dist W/ 2/3 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_23_nocp_group, main="Dist W/O 2/3 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_13_group, main="Dist W/ 1/3 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_13_nocp_group, main="Dist W/O 1/3 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_16_group, main="Dist W/ 1/6 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_16_nocp_group, main="Dist W/O 1/6 Complex - unbiased", range=c(0,1))

# species
pred_pa_modGS_56 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_56_nocp <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_23 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_23_nocp <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_13 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_13_nocp <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_16 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_16_nocp <- rast(rep(mad_mask, n_sp))
covs$not_complex <- 1
# data frames to store results
mse_56 <- data.frame(CP = 0, NoCP=0)
cor_56 <- data.frame(CP=0, NoCP=0)
mse_23 <- data.frame(CP = 0, NoCP=0)
cor_23 <- data.frame(CP=0, NoCP=0)
mse_13 <- data.frame(CP = 0, NoCP=0)
cor_13 <- data.frame(CP=0, NoCP=0)
mse_16 <- data.frame(CP = 0, NoCP=0)
cor_16 <- data.frame(CP=0, NoCP=0)
# Plot all species
for(letter in letters[1:n_sp]){
  covs$sp <- letter
  i <- match(letter, letters)
  pred_pa_modGS_56[[i]] <- sdm_predict(
    model = mos_modGS_56,
    covariates = covs
  )
  pred_pa_modGS_56_nocp[[i]] <- sdm_predict(
    model = mos_modGS_56_nocp,
    covariates = covs
  )
  pred_pa_modGS_23[[i]] <- sdm_predict(
    model = mos_modGS_23,
    covariates = covs
  )
  pred_pa_modGS_23_nocp[[i]] <- sdm_predict(
    model = mos_modGS_23_nocp,
    covariates = covs
  )
  pred_pa_modGS_13[[i]] <- sdm_predict(
    model = mos_modGS_13,
    covariates = covs
  )
  pred_pa_modGS_13_nocp[[i]] <- sdm_predict(
    model = mos_modGS_13_nocp,
    covariates = covs
  )
  pred_pa_modGS_16[[i]] <- sdm_predict(
    model = mos_modGS_16,
    covariates = covs
  )
  pred_pa_modGS_16_nocp[[i]] <- sdm_predict(
    model = mos_modGS_16_nocp,
    covariates = covs
  )
  par(mfrow=c(3,3))
  # plot real vs predicted models
  plot(prob_pres[[i]], main = "True Prob of Pres", range=c(0,1))
  plot(pred_pa_modGS_56[[i]], main = paste("Species", letter, "- unbiased 5/6CP"), range=c(0,1))
  plot(pred_pa_modGS_56_nocp[[i]], main = paste("Species", letter, "- unbiased No 5/6CP"), range=c(0,1))
  plot(pred_pa_modGS_23[[i]], main = paste("Species", letter, "- unbiased 2/3CP"), range=c(0,1))
  plot(pred_pa_modGS_23_nocp[[i]], main = paste("Species", letter, "- unbiased No 2/3CP"), range=c(0,1))
  plot(pred_pa_modGS_13[[i]], main = paste("Species", letter, "- unbiased 1/3CP"), range=c(0,1))
  plot(pred_pa_modGS_13_nocp[[i]], main = paste("Species", letter, "- unbiased No 1/3CP"), range=c(0,1))
  plot(pred_pa_modGS_16[[i]], main = paste("Species", letter, "- unbiased 1/6CP"), range=c(0,1))
  plot(pred_pa_modGS_16_nocp[[i]], main = paste("Species", letter, "- unbiased No 1/6CP"), range=c(0,1))
  # store results
  cor_56[i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS_56[[i]])
  cor_56[i, 2] <- compute_cor(prob_pres[[i]], pred_pa_modGS_56_nocp[[i]])
  mse_56[i, 1] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_56[[i]])
  mse_56[i, 2] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_56_nocp[[i]])
  cor_23[i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS_23[[i]])
  cor_23[i, 2] <-compute_cor(prob_pres[[i]], pred_pa_modGS_23_nocp[[i]])
  mse_23[i, 1] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_23[[i]])
  mse_23[i, 2] <-inverse_probit(prob_pres[[i]], pred_pa_modGS_23_nocp[[i]])
  cor_13[i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS_13[[i]])
  cor_13[i, 2] <-compute_cor(prob_pres[[i]], pred_pa_modGS_13_nocp[[i]])
  mse_13[i, 1] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_13[[i]])
  mse_13[i, 2] <-inverse_probit(prob_pres[[i]], pred_pa_modGS_13_nocp[[i]])
  cor_16[i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS_16[[i]])
  cor_16[i, 2] <-compute_cor(prob_pres[[i]], pred_pa_modGS_16_nocp[[i]])
  mse_16[i, 1] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_16[[i]])
  mse_16[i, 2] <-inverse_probit(prob_pres[[i]], pred_pa_modGS_16_nocp[[i]])
}
# performance for data of varying quality
mse_56
cor_56
mse_23
cor_23
mse_13
cor_13
mse_16
cor_16


par(mfrow=c(2,2))

# plot partial response plots
partial_response_plot(
  model = mos_modGS_56,
  data = pa_model_data_56,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_56,
  data = pa_model_data_56,
  var = "pprec",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_56_nocp,
  data = pa_model_data_56_nocp,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_56_nocp,
  data = pa_model_data_56_nocp,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# checking models
round(k.check(mos_modGS_56),2)
round(k.check(mos_modGS_56_nocp),2)


# checking data quality
# biased ########################################################################
pa_model_data_56 <- read_csv("data/tabular/hglm_pa_data_med_allbias_56.csv")
pa_model_data_56$sp <- as.factor(pa_model_data_56$sp)
# no group data
pa_model_data_56_nocp <- read_csv("data/tabular/hglm_pa_data_med_allbias_56_nocp.csv")
pa_model_data_56_nocp$sp <- as.factor(pa_model_data_56_nocp$sp)

pa_model_data_23 <- read_csv("data/tabular/hglm_pa_data_med_allbias_23.csv")
pa_model_data_23$sp <- as.factor(pa_model_data_23$sp)
# no group data
pa_model_data_23_nocp <- read_csv("data/tabular/hglm_pa_data_med_allbias_23_nocp.csv")
pa_model_data_23_nocp$sp <- as.factor(pa_model_data_23_nocp$sp)

pa_model_data_13 <- read_csv("data/tabular/hglm_pa_data_med_allbias_13.csv")
pa_model_data_13$sp <- as.factor(pa_model_data_13$sp)
# no group data
pa_model_data_13_nocp <- read_csv("data/tabular/hglm_pa_data_med_allbias_13_nocp.csv")
pa_model_data_13_nocp$sp <- as.factor(pa_model_data_13_nocp$sp)

pa_model_data_16 <- read_csv("data/tabular/hglm_pa_data_med_allbias_16.csv")
pa_model_data_16$sp <- as.factor(pa_model_data_16$sp)
# no group data
pa_model_data_16_nocp <- read_csv("data/tabular/hglm_pa_data_med_allbias_16_nocp.csv")
pa_model_data_16_nocp$sp <- as.factor(pa_model_data_16_nocp$sp)

mos_modGS_56 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                      t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                         by=not_complex), 
                    data = pa_model_data_56, 
                    family = "binomial", 
                    method = "REML")
# using species-only data
mos_modGS_56_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                           t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                              by=not_complex), 
                         data = pa_model_data_56_nocp, 
                         family = "binomial", 
                         method = "REML")

mos_modGS_23 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                      t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                         by=not_complex), 
                    data = pa_model_data_23, 
                    family = "binomial", 
                    method = "REML")
# using species-only data
mos_modGS_23_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                           t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                              by=not_complex), 
                         data = pa_model_data_23_nocp, 
                         family = "binomial", 
                         method = "REML")

mos_modGS_13 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                      t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                         by=not_complex), 
                    data = pa_model_data_13, 
                    family = "binomial", 
                    method = "REML")
# using species-only data
mos_modGS_13_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                           t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                              by=not_complex), 
                         data = pa_model_data_13_nocp, 
                         family = "binomial", 
                         method = "REML")

mos_modGS_16 <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                      t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                         by=not_complex), 
                    data = pa_model_data_16, 
                    family = "binomial", 
                    method = "REML")
# using species-only data
mos_modGS_16_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                           t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                              by=not_complex), 
                         data = pa_model_data_16_nocp, 
                         family = "binomial", 
                         method = "REML")

# species
covs$not_complex <- 0
covs$sp <- "x"

# predict based on models
pred_pa_modGS_56_group <- sdm_predict(
  model = mos_modGS_56,
  covariates = covs
)
pred_pa_modGS_56_nocp_group <- sdm_predict(
  model = mos_modGS_56_nocp,
  covariates = covs
)

pred_pa_modGS_23_group <- sdm_predict(
  model = mos_modGS_23,
  covariates = covs
)
pred_pa_modGS_23_nocp_group <- sdm_predict(
  model = mos_modGS_23_nocp,
  covariates = covs
)

pred_pa_modGS_13_group <- sdm_predict(
  model = mos_modGS_13,
  covariates = covs
)
pred_pa_modGS_13_nocp_group <- sdm_predict(
  model = mos_modGS_13_nocp,
  covariates = covs
)

pred_pa_modGS_16_group <- sdm_predict(
  model = mos_modGS_16,
  covariates = covs
)
pred_pa_modGS_16_nocp_group <- sdm_predict(
  model = mos_modGS_16_nocp,
  covariates = covs
)

par(mfrow=c(2,2))
# plot real vs predicted models
plot(group_prob_pres, main="Group Prob of Pres", range=c(0,1))
plot(pred_pa_modGS_56_group, main="Dist W/ 5/6 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_56_nocp_group, main="Dist W/O 5/6 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_23_group, main="Dist W/ 2/3 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_23_nocp_group, main="Dist W/O 2/3 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_13_group, main="Dist W/ 1/3 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_13_nocp_group, main="Dist W/O 1/3 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_16_group, main="Dist W/ 1/6 Complex - unbiased", range=c(0,1))
plot(pred_pa_modGS_16_nocp_group, main="Dist W/O 1/6 Complex - unbiased", range=c(0,1))

# species
pred_pa_modGS_56 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_56_nocp <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_23 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_23_nocp <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_13 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_13_nocp <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_16 <- rast(rep(mad_mask, n_sp))
pred_pa_modGS_16_nocp <- rast(rep(mad_mask, n_sp))
covs$not_complex <- 1
# data frames to store results
mse_56 <- data.frame(CP = 0, NoCP=0)
cor_56 <- data.frame(CP=0, NoCP=0)
mse_23 <- data.frame(CP = 0, NoCP=0)
cor_23 <- data.frame(CP=0, NoCP=0)
mse_13 <- data.frame(CP = 0, NoCP=0)
cor_13 <- data.frame(CP=0, NoCP=0)
mse_16 <- data.frame(CP = 0, NoCP=0)
cor_16 <- data.frame(CP=0, NoCP=0)
# Plot all species
for(letter in letters[1:n_sp]){
  covs$sp <- letter
  i <- match(letter, letters)
  pred_pa_modGS_56[[i]] <- sdm_predict(
    model = mos_modGS_56,
    covariates = covs
  )
  pred_pa_modGS_56_nocp[[i]] <- sdm_predict(
    model = mos_modGS_56_nocp,
    covariates = covs
  )
  pred_pa_modGS_23[[i]] <- sdm_predict(
    model = mos_modGS_23,
    covariates = covs
  )
  pred_pa_modGS_23_nocp[[i]] <- sdm_predict(
    model = mos_modGS_23_nocp,
    covariates = covs
  )
  pred_pa_modGS_13[[i]] <- sdm_predict(
    model = mos_modGS_13,
    covariates = covs
  )
  pred_pa_modGS_13_nocp[[i]] <- sdm_predict(
    model = mos_modGS_13_nocp,
    covariates = covs
  )
  pred_pa_modGS_16[[i]] <- sdm_predict(
    model = mos_modGS_16,
    covariates = covs
  )
  pred_pa_modGS_16_nocp[[i]] <- sdm_predict(
    model = mos_modGS_16_nocp,
    covariates = covs
  )
  par(mfrow=c(3,3))
  # plot real vs predicted models
  plot(prob_pres[[i]], main = "True Prob of Pres", range=c(0,1))
  plot(pred_pa_modGS_56[[i]], main = paste("Species", letter, "- unbiased 5/6CP"), range=c(0,1))
  plot(pred_pa_modGS_56_nocp[[i]], main = paste("Species", letter, "- unbiased No 5/6CP"), range=c(0,1))
  plot(pred_pa_modGS_23[[i]], main = paste("Species", letter, "- unbiased 2/3CP"), range=c(0,1))
  plot(pred_pa_modGS_23_nocp[[i]], main = paste("Species", letter, "- unbiased No 2/3CP"), range=c(0,1))
  plot(pred_pa_modGS_13[[i]], main = paste("Species", letter, "- unbiased 1/3CP"), range=c(0,1))
  plot(pred_pa_modGS_13_nocp[[i]], main = paste("Species", letter, "- unbiased No 1/3CP"), range=c(0,1))
  plot(pred_pa_modGS_16[[i]], main = paste("Species", letter, "- unbiased 1/6CP"), range=c(0,1))
  plot(pred_pa_modGS_16_nocp[[i]], main = paste("Species", letter, "- unbiased No 1/6CP"), range=c(0,1))
  # store results
  cor_56[i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS_56[[i]])
  cor_56[i, 2] <- compute_cor(prob_pres[[i]], pred_pa_modGS_56_nocp[[i]])
  mse_56[i, 1] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_56[[i]])
  mse_56[i, 2] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_56_nocp[[i]])
  cor_23[i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS_23[[i]])
  cor_23[i, 2] <-compute_cor(prob_pres[[i]], pred_pa_modGS_23_nocp[[i]])
  mse_23[i, 1] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_23[[i]])
  mse_23[i, 2] <-inverse_probit(prob_pres[[i]], pred_pa_modGS_23_nocp[[i]])
  cor_13[i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS_13[[i]])
  cor_13[i, 2] <-compute_cor(prob_pres[[i]], pred_pa_modGS_13_nocp[[i]])
  mse_13[i, 1] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_13[[i]])
  mse_13[i, 2] <-inverse_probit(prob_pres[[i]], pred_pa_modGS_13_nocp[[i]])
  cor_16[i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS_16[[i]])
  cor_16[i, 2] <-compute_cor(prob_pres[[i]], pred_pa_modGS_16_nocp[[i]])
  mse_16[i, 1] <- inverse_probit(prob_pres[[i]], pred_pa_modGS_16[[i]])
  mse_16[i, 2] <-inverse_probit(prob_pres[[i]], pred_pa_modGS_16_nocp[[i]])
}
# performance
mse_56
cor_56
mse_23
cor_23
mse_13
cor_13
mse_16
cor_16


par(mfrow=c(2,2))

# plot partial response plots
partial_response_plot(
  model = mos_modGS_56,
  data = pa_model_data_56,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_56,
  data = pa_model_data_56,
  var = "pprec",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_56_nocp,
  data = pa_model_data_56_nocp,
  var = "ttemp",
  # scale = "link"
  scale = "response"
)
partial_response_plot(
  model = mos_modGS_56_nocp,
  data = pa_model_data_56_nocp,
  var = "pprec",
  # scale = "link"
  scale = "response"
)

# checking models
round(k.check(mos_modGS_56),2)
round(k.check(mos_modGS_56_nocp),2)
