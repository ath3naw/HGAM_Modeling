# fit models

library(terra)
library(tidyverse)
#library(predicts) # remotes::install_github("rspatial/predicts")
library(glmnet)
library(maxnet)
source("R/functions.R")

# read in our data!

# rasters
rel_abund <- terra::rast("data/grids/rel_abund.tif")
prob_present <- terra::rast("data/grids/prob_present.tif")
bias <- terra::rast("data/grids/bias.tif")
reported_occurrence_rate <- terra::rast("data/grids/reported_occurrence_rate.tif")

mad_mask <- terra::rast("data/grids/mad_mask.tif")
bc_mad <- terra::rast("data/grids/bc_mad.tif")
rescale_travel <- terra::rast("data/grids/rescale_travel.tif")

# points 
occurrence_coords <- read_csv("data/tabular/presence_only_points.csv")
pa_random_data <- read_csv("data/tabular/presence_absence_random_sampling.csv")
pa_bias_data <- read_csv("data/tabular/presence_absence_bias_sampling.csv")
pa_bias_abund_data  <- read_csv("data/tabular/presence_absence_bias_abund_sampling.csv")

# species_df <- read_csv("data/tabular/species_df.csv")

# subset bc_mad to our covariate set (and save this again for ease of use)
covs <- bc_mad[[c(1,3,4,8,12,15,18)]]
names(covs) <- c("ttemp", "tiso", "tseas", "twet", "pprec", "pseas", "pwarm")

terra::writeRaster(
  x = covs,
  filename = "data/grids/covariates.tif",
  overwrite=TRUE
)


### Model: logistic regression of random presence-absence data

pa_random_data

data_pa_random <- model_data_presence_absence(
  pa_data = pa_random_data,
  covariates = covs
)

data_pa_random

# fit a simple model!
model_pa_random_logistic <- glm(
  presence ~ ttemp + tiso + tseas + twet + pprec + pseas + pwarm + twet*pwarm + 
    tseas*pseas + pprec*twet + ttemp*pwarm + twet*ttemp,
  data = data_pa_random,
  family = binomial()
)
summary(model_pa_random_logistic)


# plot the partial responses for each
# predictor variable (covariate)
partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "ttemp"
)
# now do other covariates
partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "tiso"
)

partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "tseas"
)

partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "twet"
)

partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "pprec"
)

partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "pseas"
)

partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "pwarm"
)

# predict our distribution based on our model
# and covariates
pred_pa_random_logistic <- sdm_predict(
  model = model_pa_random_logistic,
  covariates = covs
)
par(mfrow=c(1,1))

# plot it
plot(pred_pa_random_logistic)

#compare it with the truth
plot(c(rel_abund, prob_present, pred_pa_random_logistic))
plot(c(prob_present, pred_pa_random_logistic))

### Model: logistic regression of presence-only data
# with random background 
par(mfrow=c(1,1))

# sample random background points
n_background_points <- 1000

random_bg <- terra::spatSample(
  x = mad_mask,
  size = n_background_points,
  na.rm = TRUE,
  as.points = TRUE
)

rastpointplot(mad_mask, random_bg)

# put presence and background data together
# with covariates

data_po_random_bg <- model_data_presence_only(
  presences = occurrence_coords,
  absences = random_bg,
  covariates = covs
)


# fit a simple model!
model_po_random_bg_logistic <- glm(
  presence ~ ttemp + tiso + tseas + twet + pprec + pseas + pwarm + 
    twet*pwarm + tseas*pseas + pprec*twet + ttemp*pwarm + twet*ttemp,
  data = data_po_random_bg,
  family = binomial()
)
summary(model_po_random_bg_logistic)

# partial response of each variable
partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "ttemp"
)
# now do other covariates
partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "tiso"
)

partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "tseas"
)

partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "twet"
)

partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "pprec"
)

partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "pseas"
)

partial_response_plot(
  model = model_pa_random_logistic,
  data = data_pa_random,
  var = "pwarm"
)
# predict our distribution based on our model and covariates
pred_po_random_bg_logistic <- sdm_predict(
  model = model_po_random_bg_logistic,
  covariates = covs
)

# plot it
plot(pred_po_random_bg_logistic)

plot(c(pred_pa_random_logistic, pred_po_random_bg_logistic))

# now compare that prediction with the truth
plot(c(prob_present, pred_pa_random_logistic, pred_po_random_bg_logistic, reported_occurrence_rate))


# ### Model: logistic regression of presence-only data (less variables)
# # with random background 
# par(mfrow=c(1,1))
# 
# # sample random background points
# n_background_points <- 500
# 
# random_bg <- terra::spatSample(
#   x = mad_mask,
#   size = n_background_points,
#   na.rm = TRUE,
#   as.points = TRUE
# )
# 
# rastpointplot(mad_mask, random_bg)
# 
# # put presence and background data together
# # with covariates
# 
# data_po_random_bg <- model_data_presence_only(
#   presences = occurrence_coords,
#   absences = random_bg,
#   covariates = covs
# )
# 
# 
# # fit a simple model!
# model_po_random_bg_logistic <- glm(
#   presence ~ ttemp + tiso + tseas + pprec + pwarm,
#   data = data_po_random_bg,
#   family = binomial()
# )
# summary(model_po_random_bg_logistic)
# 
# # partial response of each variable
# partial_response_plot(
#   model = model_pa_random_logistic,
#   data = data_pa_random,
#   var = "ttemp"
# )
# # now do other covariates
# partial_response_plot(
#   model = model_pa_random_logistic,
#   data = data_pa_random,
#   var = "tiso"
# )
# 
# partial_response_plot(
#   model = model_pa_random_logistic,
#   data = data_pa_random,
#   var = "tseas"
# )
# 
# partial_response_plot(
#   model = model_pa_random_logistic,
#   data = data_pa_random,
#   var = "pprec"
# )
# 
# partial_response_plot(
#   model = model_pa_random_logistic,
#   data = data_pa_random,
#   var = "pwarm"
# )
# 
# # predict our distribution based on our model and covariates
# pred_po_random_bg_logistic <- sdm_predict(
#   model = model_po_random_bg_logistic,
#   covariates = covs
# )
# 
# # plot it
# plot(pred_po_random_bg_logistic)
# 
# plot(c(pred_pa_random_logistic, pred_po_random_bg_logistic))
# 
# # now compare that prediction with the truth
# plot(c(prob_present, pred_pa_random_logistic, pred_po_random_bg_logistic, reported_occurrence_rate))

### Model:presence-only with maxnet (R version of maxent)

#use the presence only and random background again
data_po_random_bg

# model in maxnet

model_po_random_bg_maxent <- maxnet(
  p = data_po_random_bg$presence,
  data = data_po_random_bg %>% select(-presence),
  f = maxnet.formula(
    p = data_po_random_bg$presence,
    data = data_po_random_bg %>% select(-presence),
    classes = "lqp"
  ),
  addsamplestobackground = FALSE # already created them
)


# partial response of each variable
# different for maxnet than the glms
plot(model_po_random_bg_maxent, "ttemp")

plot(model_po_random_bg_maxent, "tiso")

plot(model_po_random_bg_maxent, "tseas")

plot(model_po_random_bg_maxent, "tseas")

plot(model_po_random_bg_maxent, "twet")

plot(model_po_random_bg_maxent, "pprec")

plot(model_po_random_bg_maxent, "pseas")

plot(model_po_random_bg_maxent, "pwarm")
# do others!


# predict our distribution based on our model and covariates
pred_po_random_bg_maxent <- sdm_predict(
  model = model_po_random_bg_maxent,
  covariates = covs
)

# plot it
plot(pred_po_random_bg_maxent)

plot(c(pred_po_random_bg_logistic, pred_po_random_bg_maxent))

plot(c(prob_present, pred_pa_random_logistic,
       pred_po_random_bg_logistic, pred_po_random_bg_maxent))

plot(c(prob_present, pred_pa_random_logistic,
       pred_po_random_bg_maxent, reported_occurrence_rate))


### Model:presence-only with maxnet (R version of maxent)
# with bias layer

#use the presence only and random background again
data_po_random_bg

# extract bias values from presence and background locations
names(bias) <- "bias"
maxent_bias_df <-  model_data_presence_only(
  presences = occurrence_coords,
  absences = random_bg,
  covariates = bias
)


# log them and create vector
maxent_bias <- log(maxent_bias_df$bias)

# model in maxnet

model_po_random_bg_maxent_bias <- maxnet(
  p = data_po_random_bg$presence,
  data = data_po_random_bg %>% select(-presence),
  offset = maxent_bias %>% as.matrix(),
  f = maxnet.formula(
    p = data_po_random_bg$presence,
    data = data_po_random_bg %>% select(-presence),
    classes = "lqp"
  ),
  addsamplestobackground = FALSE # because we have included background
)


# partial response of each variable
# different for maxnet than the glms
plot(model_po_random_bg_maxent_bias, "ttemp")
plot(model_po_random_bg_maxent_bias, "tiso")
plot(model_po_random_bg_maxent_bias, "tseas")
plot(model_po_random_bg_maxent_bias, "twet")
plot(model_po_random_bg_maxent_bias, "pprec")
plot(model_po_random_bg_maxent_bias, "pseas")
plot(model_po_random_bg_maxent_bias, "pwarm")
# do others!


# predict our distribution based on our model and covariates
pred_po_random_bg_maxent_bias <- sdm_predict(
  model = model_po_random_bg_maxent_bias,
  covariates = covs
)

# plot it
plot(pred_po_random_bg_maxent_bias)

plot(c(pred_po_random_bg_maxent, pred_po_random_bg_maxent_bias))

plot(c(prob_present,
  pred_po_random_bg_maxent,
  pred_po_random_bg_maxent_bias,
  reported_occurrence_rate))

### maxent model with less correlated variables
data_po_random_bg_cut <- data_po_random_bg %>% select(ttemp, tiso, tseas, pprec, pwarm)

# extract bias values from presence and background locations
maxent_bias_df <-  model_data_presence_only(
  presences = occurrence_coords,
  absences = random_bg,
  covariates = bias
)

# log them and create vector
maxent_bias <- log(maxent_bias_df$bias)

# model in maxnet

model_po_random_bg_maxent_bias_cut <- maxnet(
  p = data_po_random_bg$presence,
  data = data_po_random_bg_cut,
  offset = maxent_bias %>% as.matrix(),
  f = maxnet.formula(
    p = data_po_random_bg$presence,
    data = data_po_random_bg_cut,
    classes = "lqp"
  ),
  addsamplestobackground = FALSE # because we have included background
)


# partial response of each variable
# different for maxnet than the glms
plot(model_po_random_bg_maxent_bias_cut, "ttemp")
plot(model_po_random_bg_maxent_bias_cut, "tiso")
plot(model_po_random_bg_maxent_bias_cut, "tseas")
plot(model_po_random_bg_maxent_bias_cut, "pprec")
plot(model_po_random_bg_maxent_bias_cut, "pwarm")
# do others!


# predict our distribution based on our model and covariates
pred_po_random_bg_maxent_bias_cut <- sdm_predict(
  model = model_po_random_bg_maxent_bias_cut,
  covariates = covs
)

# plot it
plot(pred_po_random_bg_maxent_bias_cut)

plot(c(pred_po_random_bg_maxent, pred_po_random_bg_maxent_bias, pred_po_random_bg_maxent_bias_cut))

plot(c(prob_present,
       pred_po_random_bg_maxent,
       pred_po_random_bg_maxent_bias,
       pred_po_random_bg_maxent_bias_cut,
       reported_occurrence_rate))



### Model: glm with target group-background

# our data
# presences
occurrence_coords

# this time we will use other species as "absences"
species_df

data_po_tgb_all <- model_data_presence_only(
  presences = occurrence_coords,
  absences = species_df %>%
    filter(type == "focal") %>%
    dplyr::select(x, y),
  covariates = covs
)




# maxent with target group background (and not bias)

model_po_tbg_maxent <- maxnet(
  p = data_po_tgb_all$presence,
  data = data_po_tgb_all %>% select(-presence),
  f = maxnet.formula(
    p = data_po_tgb_all$presence,
    data = data_po_tgb_all %>% select(-presence),
    classes = "lqp"
  ),
  addsamplestobackground = FALSE # because we have included background
)
summary(model_po_tbg_maxent)

# predict our distribution based on our model and covariates
pred_po_tgb_maxent <- sdm_predict(
  model = model_po_tbg_maxent,
  covariates = covs
)

# plot it
plot(pred_po_tgb_maxent)

# now compare that prediction with the truth
plot(c(prob_present,
       pred_po_random_bg_maxent,
       pred_po_random_bg_maxent_bias,
       pred_po_tgb_maxent))


### glm model with less correlated variables

# our data
# presences
occurrence_coords

# this time we will use other species as "absences"
species_df

data_po_tgb_all <- model_data_presence_only(
  presences = occurrence_coords,
  absences = species_df %>%
    filter(type == "focal") %>%
    dplyr::select(x, y),
  covariates = covs
)

data_po_tgb_all_cut <- data_po_tgb_all %>% select(ttemp, tiso, tseas, pprec, pwarm)


# maxent with target group background (and not bias)

model_po_tbg_maxent_cut <- maxnet(
  p = data_po_tgb_all$presence,
  data = data_po_tgb_all_cut,
  f = maxnet.formula(
    p = data_po_tgb_all$presence,
    data = data_po_tgb_all_cut,
    classes = "lqp"
  ),
  addsamplestobackground = FALSE # because we have included background
)
summary(model_po_tbg_maxent_cut)

# predict our distribution based on our model and covariates
pred_po_tgb_maxent_cut <- sdm_predict(
  model = model_po_tbg_maxent_cut,
  covariates = covs
)

# plot it
plot(pred_po_tgb_maxent_cut)

# now compare that prediction with the truth
plot(c(prob_present,
       pred_po_random_bg_maxent,
       pred_po_random_bg_maxent_bias,
       pred_po_random_bg_maxent_bias_cut,
       pred_po_tgb_maxent,
       pred_po_tgb_maxent_cut))



### Model: correlated predictor variables

covs_correlated <- bc_mad[[c(1,5,12,8)]]
names(covs_correlated) <- c("tmean", "tmax", "precip", "twet")
plot(covs_correlated)

pa_random_data

data_pa_random_correlated <- model_data_presence_absence(
  pa_data = pa_random_data,
  covariates = covs_correlated
)

data_pa_random_correlated

# fit a simple model!
model_pa_random_correlated_logistic <- glm(
  presence ~  tmean + tmax + precip + twet,
  data = data_pa_random_correlated,
  family = binomial()
)
summary(model_pa_random_correlated_logistic)


# plot the partial responses for each
# predictor variable (covariate)
partial_response_plot(
  model = model_pa_random_correlated_logistic,
  data = data_pa_random_correlated,
  var = "tmax",
  # scale = "link"
  scale = "response"
)
# now do
# tseas
# trange
partial_response_plot(
  model = model_pa_random_correlated_logistic,
  data = data_pa_random_correlated,
  var = "tmean"
)
partial_response_plot(
  model = model_pa_random_correlated_logistic,
  data = data_pa_random_correlated,
  var = "precip"
)

# predict our distribution based on our model
# and covariates
pred_pa_random_correlated_logistic <- sdm_predict(
  model = model_pa_random_correlated_logistic,
  covariates = covs_correlated
)

# plot it
plot(pred_pa_random_correlated_logistic)


plot(c(prob_present, pred_pa_random_correlated_logistic))


par(mfrow = c(2,3))
plot(prob_present, main="Prob of Presence")
plot(reported_occurrence_rate, main="Rep Occur Rate")
plot(pred_pa_random_logistic, main="PA Logistic")
plot(pred_po_random_bg_logistic, main="PO Logistic - Random BG")
plot(pred_po_random_bg_maxent, main="Maxent - Random BG")
plot(pred_po_random_bg_maxent_bias, main="Maxent - Biased BG")

