library(terra)
library(tidyverse)

# Given a raster and a number of sampling locations to simulate, return a
# SpatVector object of the locations. If weighted = TRUE, then treat the values
# of the raster as the relative number of points to put in each cell. If replace
# = TRUE, then multiple samples can appear in the same raster cell.
random_locations <- function(raster, n_samples, weighted = TRUE, replace = TRUE) {
  
  if (weighted) {
    method <- "weights"
  } else {
    method <- "random"
  }
  
  terra::spatSample(raster,
                    n_samples,
                    method = method,
                    replace = replace,
                    na.rm = TRUE,
                    as.points = TRUE)
  
}

# given a SpatVector object of locations at which to sample, and a SpatRaster
# object of the relative abundance (must be scaled to a maximum value of 1), and
# a maximum average catch size (the average catch size at the location with the
# highest abundance), return the SpatVector object with extra columns for the a
# simulated catch size 'count', and binary variable for whether the species was
# present in the catch
sim_catches <- function(sample_locations,
                        relative_abundance,
                        max_average_catch_size = 5000) {
  # how many samples?
  n_samples <- nrow(sample_locations)
  
  # if you were to trap here repeatedly, what would the average catch size be,
  # in the long term?
  average_catch_size <- relative_abundance * max_average_catch_size
  
  # get the values for this at the sample locations
  expected_catch_size <- terra::extract(average_catch_size,
                                        sample_locations)[, 2]
  
  # given this average catch size, sample a single catch (assuming a Poisson
  # distribution for catch sizes)
  sample_locations$count <- rpois(n_samples, expected_catch_size)
  
  # record a 1 if any were caught, and a 0 otherwise
  sample_locations$presence <- pmin(sample_locations$count, 1)
  
  # return the locations with this info attached
  sample_locations
  
}

# given an unscaled relative abundance raster, scale it to have maximum value of 1 
rescale_abundance <- function(unscaled_abundance) {
  min_value <- global(unscaled_abundance, "min", na.rm = TRUE)[1, 1]
  unscaled_abundance <- unscaled_abundance - min_value + 0.00001
  max_value <- global(unscaled_abundance, "max", na.rm = TRUE)[1, 1]
  unscaled_abundance / max_value
}

# given a SpatRaster object of the relative abundance (must be scaled to a
# maximum value of 1), and a maximum average catch size (the average catch size
# at the location with the highest abundance), return a SpatRaster of the
# probability of the species being present in a random catch at each location
probability_of_presence <- function(relative_abundance,
                                    max_average_catch_size = 5000) {
  
  # if you were to trap here repeatedly, what would the average catch size be,
  # in the long term?
  average_catch_size <- relative_abundance * max_average_catch_size
  
  # what is the probability of detecting one or more mosquitoes in a poisson
  # sample with an average catch of this size
  probability_one_or_more <- 1 - exp(-(average_catch_size))
  
  probability_one_or_more
  
}

# name prediction layer 
sdm_predict <- function(
  model,
  covariates,
  type = NULL,
  layer_name = "predicted_distribution"
){
  
  if(is.null(type)){
    if (inherits(model, "maxnet")) {
      type <- "logistic"
    } else {
      type <- "response"
    }
  }
  
  prediction <- predict(covariates, model, na.rm = TRUE, type = type)
  names(prediction) <- layer_name
  
  return(prediction)
}

# function to take presence only and background data
# and covariate rasters and form into a data frame 
# for modelling
model_data_presence_only <- function(
  presences,
  absences,
  covariates
){
  
  pvals <- terra::extract(covariates, presences)
  avals <- terra::extract(covariates, absences)
  
  rbind(
    pvals %>%
      as_tibble %>%
      dplyr::select(-ID) %>%
      mutate(presence = 1),
    avals %>%
      as_tibble %>%
      dplyr::select(-ID) %>%
      mutate(presence = 0)
  ) %>%
    as_tibble
  
}

# function to take presence absence data
# and covariate rasters and form into a data frame 
# for modelling

model_data_presence_absence <- function(
  pa_data,
  covariates
){
  
  vals <- terra::extract(
    covariates,
    pa_data %>%
      dplyr::select(x, y)
  )
  
  cbind(
    pa_data %>%
      dplyr::select(-x, -y),
    vals %>%
      as_tibble %>%
      dplyr::select(-ID)
  ) %>%
    as_tibble
  
}



# fit a probabilityal response curve
probability_ofal_response <- function (model, data, var, type = c("response", "link"), rng = NULL, nsteps = 25) {
  
  type <- match.arg(type)
  if (missing(var)) {
    var <- names(data)[1]
  }
  else if (is.numeric(var)) {
    stopifnot(var > 0 & var <= ncol(data))
    var <- names(data)[var]
  }
  else {
    stopifnot(var %in% names(data))
  }
  if (is.factor(data[[var]])) {
    steps <- levels(data[[var]])
  }
  else {
    if (is.null(rng)) {
      rng <- range(data[[var]])
    }
    increment <- (rng[2] - rng[1])/(nsteps - 2)
    steps <- seq(rng[1] - increment, rng[2] + increment, 
                 increment)
  }
  res <- rep(NA, length(steps))
  for (i in 1:length(steps)) {
    data[[var]] <- steps[i]
    p <- predict(model, data, type = type)
    res[i] <- mean(p)
  }
  x <- data.frame(steps, res)
  names(x) <- c("var", "p")
  x
}

# function to calculate spacing of x-coords + y-values for partial response plot
partial_response <- function (model, data, var, type = c("response", "link"), rng = NULL, nsteps = 25) {
  
  type <- match.arg(type)
  if (missing(var)) {
    var <- names(data)[1]
  }
  else if (is.numeric(var)) {
    stopifnot(var > 0 & var <= ncol(data))
    var <- names(data)[var]
  }
  else {
    stopifnot(var %in% names(data))
  }
  if (is.factor(data[[var]])) {
    steps <- levels(data[[var]])
  }
  else {
    if (is.null(rng)) {
      rng <- range(data[[var]])
    }
    increment <- (rng[2] - rng[1])/(nsteps - 2)
    steps <- seq(rng[1] - increment, rng[2] + increment, 
                 increment)
  }
  res <- rep(NA, length(steps))
  for (i in 1:length(steps)) {
    data[[var]] <- steps[i]
    p <- predict(model, data, type = type)
    res[i] <- mean(p)
  }
  x <- data.frame(steps, res)
  names(x) <- c("var", "p")
  x
}

# function to plot partial response curve
partial_response_plot <- function(
  model,
  data,
  var,
  scale = c("response", "link")
){
  plot(
    partial_response(
      model = model,
      data = data,
      var = var,
      type = scale
    ),
    type = "l",
    xlab = var # could put this into comments if you just want "var" at the bottom
  )
}

# function to plot partial response plot for group probability of presence
# uses "true" formula to calculate true partial response
partial_group <- function(var, means, bounds, cons, minmax, max_average_catch_size=5000, length_out = 25){
  var_name <- var
  i <- which(bounds$variable == var_name)
  pr_data <- seq(from=bounds$min.min[i],
                       to=bounds$min.max[i],
                       length.out=25)
  gamma <- rep(beta_group["int"], length_out)
  for(term in names(beta_group)){
    if(term == "int") next # skip intercept
    # if name of variable ends in 2
    if(grepl("[0-9]$", term)){
      suffix <- sub(".*([0-9])", "\\1", term)
      suffix <- as.numeric(suffix)
      base_var <- sub("[0-9]$", "", term)
      if(base_var == var_name){
        if(base_var %in% names(cons)){
          gamma <- gamma + beta_group[term] * (pr_data-cons[base_var])^suffix
        }else{
          gamma <- gamma + beta_group[term] * pr_data^suffix
        }
      }else{
        if(base_var %in% names(cons)){
          gamma <- gamma + beta_group[term] * (covs_means[[base_var]]-cons[base_var])^suffix
        }else{
          gamma <- gamma + beta_group[term] * covs_means[[base_var]]^suffix
        }
      }
    }else{
      if(term == var_name){
        gamma <- gamma + beta_group[term] * pr_data
      }else{
        gamma <- gamma + beta_group[term] * covs_means[[term]]
      }
    }
  }
  
  gamma_pr <- exp(gamma)
  scaled <- gamma_pr-minmax[1]+0.00001
  scaled <- scaled/(minmax[2]-minmax[1]+0.00001)
  prob_pres <- probability_of_presence(scaled, max_average_catch_size)
  
  plot(
    x = pr_data,
    y = prob_pres,
    xlab = var_name
    #ylim = c(0, 1)
  )
}

# function to plot partial response plot for species probability of presence
# uses "true" formula to calculate true partial response
partial_spec <- function(var, means, bounds, cons, minmax, max_average_catch_size=5000, length_out = 25, sp){
  var_name <- var
  i <- which(bounds$variable == var_name)
  pr_data <- seq(from=bounds$min.min[i],
                 to=bounds$min.max[i],
                 length.out=25)
  gamma <- rep(beta_group["int"]+species_data[sp,]$int, length_out)
  for(term in names(beta_group)){
    if(term == "int") next # skip intercept
    # if name of variable ends in 2
    if(grepl("[0-9]$", term)){
      suffix <- sub(".*([0-9])", "\\1", term)
      suffix <- as.numeric(suffix)
      base_var <- sub("[0-9]$", "", term)
      if(base_var == var_name){
        if(base_var %in% names(cons)){
          gamma <- gamma + (beta_group[term]+species_data[sp,][[term]]) * (pr_data-cons[base_var])^suffix
        }else{
          gamma <- gamma + (beta_group[term]+species_data[sp,][[term]]) * pr_data^suffix
        }
      }else{
        if(base_var %in% names(cons)){
          gamma <- gamma + (beta_group[term]+species_data[sp,][[term]]) * (covs_means[[base_var]]-cons[base_var])^suffix
        }else{
          gamma <- gamma + (beta_group[term]+species_data[sp,][[term]]) * covs_means[[base_var]]^suffix
        }
      }
    }else{
      if(term == var_name){
        gamma <- gamma + (beta_group[term]+species_data[sp,][[term]]) * pr_data
      }else{
        gamma <- gamma + (beta_group[term]+species_data[sp,][[term]]) * covs_means[[term]]
      }
    }
  }
  
  gamma_pr <- exp(gamma)
  scaled <- gamma_pr-minmax[sp,1]+0.00001
  scaled <- scaled/(minmax[sp,2]-minmax[sp,1]+0.00001)
  prob_pres <- probability_of_presence(scaled, max_average_catch_size)
  
  plot(
    x = pr_data,
    y = prob_pres,
    xlab = var_name
    #ylim = c(0, 1)
  )
}

# function to generate simulated data + convert to data frame
generate_data_tabular <- function(n_samples, bias, prob_pres, n_sp = 10, weighted = FALSE){
  sample_locations <- random_locations(bias,
                                      n_samples,
                                       weighted=weighted)
  pa_coords <- crds(sample_locations)
  pa_df <- tibble(site_id = integer(0),
                  presence = integer(0),
                  species_id = integer(0))
  
  for(i in seq_len(n_sp)) {
    # simulate presence-absence data
    p <- terra::extract(prob_pres, pa_coords)[, i]
    presence <- rbinom(length(p), 1, p)
    
    pa_df <- rbind(pa_df,
                   tibble(
                     site_id = seq_along(p),
                     presence = presence,
                     species_id = letters[i])
    )
    
  }
  
  pa_tabular <- pa_df %>%
    pivot_wider(names_from = species_id,
                values_from = presence) %>%
    left_join(
      bind_cols(site_id = seq_len(nrow(pa_coords)),
                pa_coords),
      
      by = "site_id"
    ) %>%
    relocate(x, y,
             .after = site_id)
  pa_tab <- pa_tabular |> 
    mutate(complex = ifelse(rowSums(across(letters[1:n_sp])) > 0, 1, 0))
  pa_tab
}

# function to convert previous tabular data into format used by model
generate_model_data <- function(n_samples, n_cp, pa_tab, n_sp=10){
  pa_tab_1 <- pa_tab[1:n_cp,] |>
    dplyr::select(site_id, x, y, complex) |>
    rename(pa = complex) |>
    mutate(sp = "x")
  if(n_cp<n_samples){
    pa_tab_2 <- pa_tab[(n_cp+1):n_samples,] |>
      dplyr::select(-complex) |>
      pivot_longer(cols = letters[1:n_sp], names_to = "sp", values_to = "pa")
  }else{
    pa_tab_2 <- NULL
  }
  pa_long <- rbind(pa_tab_1, pa_tab_2) |>
    mutate(not_complex = ifelse(sp == "x", 0, 1))
  covs_vals <- extract(x=covs, y=pa_long |> dplyr::select(x, y))
  covs_vals <- covs_vals |> dplyr::select(-any_of(c("sp","not_complex")))
  pa_model_data <- cbind(pa_long, covs_vals) |> dplyr::select(-ID) |>
    mutate(sp = as.factor(sp))
  pa_model_data
}

# computes one type of metric for calculating mse between 2 rasters
compute_mse <- function(true_prob, pred_prob){
  resid <- (pred_prob-true_prob)^2
  as.numeric(global(resid, fun="mean", na.rm=TRUE))
}

# computes one type of metric to take the inverse of the probit function
# for probability of presence - "unravels" prob pres into true relative abundance
inverse_probit <- function(true_prob, pred_prob){
  clip_range <- function(x){
    pmin(pmax(x, 1e-10), 1-1e-10)
  }
  true_inverse <- app(true_prob, function(x) qnorm(clip_range(x)))
  pred_inverse <- app(pred_prob, function(x) qnorm(clip_range(x)))
  resid <- (pred_inverse-true_inverse)^2
  as.numeric(global(resid, fun="mean", na.rm=TRUE))
}

# compute pearson correlation for pa data
compute_cor <- function(true_prob, pred_prob){
  corr <- layerCor(c(true_prob, pred_prob), "pearson", na.rm=TRUE)$correlation
  corr[1,2]
}

# compute spearman correlation for po data
compute_cor_po <- function(true_prob, pred_prob){
  v1 <- values(true_prob)
  v2 <- values(pred_prob)
  corr <- cor(v1, v2, method="spearman", use="complete.obs")
  corr[1,1]
}

# convert metrics into data frame to plot statistics/graphs
make_long <- function(df, type) {
  df %>%
    pivot_longer(cols = everything(), names_to = "Model", values_to = "Correlation") %>%
    mutate(Type = type)
}
