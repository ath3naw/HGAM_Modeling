rm(list=ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
library(mgcv)
source("R/hgam_simplified/functions.R")

# can add a seed
set.seed(136)

# load in variables
par(mfrow = c(1,1))
covs <- terra::rast("data/grids/covariates.tif")
mad_mask <- terra::rast("data/grids/mad_mask.tif")
bc_mad <- terra::rast("data/grids/bc_mad.tif")
bias <- terra::rast("data/grids/bias.tif")

# defining number of species + max catch size
n_sp <- 10
max_catch_size <- 10  # was 10

# group level model
# can change covariates as needed
beta_group <- c(ttemp = 0.02, ttemp2 = -0.1, 
                #tiso = 0.001, 
                #tseas = -0.001, 
                #twet = 0.08, twet2 = -0.02, 
                pprec = 0.001, pprec2 = -0.000004,
                #pseas = 0.04, pseas2 = -0.0007, pwarm = 0.0002, pwarm2 = -0.000001, 
                int=0.2) # int was 0.03

cons_group <- c(ttemp = 22, 
                #twet = 24, 
                pprec = 1400)#, pseas = 80, pwarm=1000)

# calculating group abundance from values
group_abund <- exp(beta_group["int"]+beta_group["ttemp"]*covs$ttemp+beta_group["ttemp2"]*(covs$ttemp-cons_group["ttemp"])^2+
                     #beta_group["tiso"]*covs$tiso+
                     #beta_group["tseas"]*covs$tseas+
                     #beta_group["twet"]*covs$twet+beta_group["twet2"]*(covs$twet-cons_group["twet"])^2+
                     beta_group["pprec"]*covs$pprec+beta_group["pprec2"]*(covs$pprec-cons_group["pprec"])^2)#+
#beta_group["pseas"]*covs$pseas+beta_group["pseas2"]*(covs$pseas-cons_group["pseas"])^2+
#beta_group["pwarm"]*covs$pwarm+beta_group["pwarm2"]*(covs$pwarm-cons_group["pwarm"])^2)
plot(group_abund)
rel_group_abund <- rescale_abundance(group_abund)
names(rel_group_abund) <- "relative_abundance"
plot(rel_group_abund, main="Relative Abundance")
group_prob_pres <- probability_of_presence(rel_group_abund,
                                           max_catch_size)
plot(group_prob_pres, main="Prob of Presence")
writeRaster(group_prob_pres,
            "data/grids/group_prob_pres_hgam.tif",
            overwrite = TRUE)
# number of simulations
n <- 10
# defining data frames to store results
cor_unbiased <- vector("list", length=n)
cor_allbiased <- vector("list", length=n)
cor_po_unbiased <- vector("list", length=n)
cor_po_allbiased <- vector("list", length=n)

# spearman correlation for everything
cor_unbiased_sp <- vector("list", length=n)
cor_allbiased_sp <- vector("list", length=n)
cor_po_unbiased_sp <- vector("list", length=n)
cor_po_allbiased_sp <- vector("list", length=n)

# initializing variable, all_bias, will overwrite later
all_bias <- mad_mask

# simulating n times
for(x in 1:n){
  # Preparing raster data *******************************************************
  # species-level deviations
  species_data <- data.frame(
    species = factor(paste("sp", 1:n_sp, sep="")),
    ttemp = rnorm(n_sp, 0, 0.015), # was 0.008
    ttemp2 = rnorm(n_sp, 0, 0.07), # was 0.015
    #tiso = rnorm(n_sp, 0, 0.003),# was 0.003
    #tseas = rnorm(n_sp, 0, 0.0025),
    #twet = rnorm(n_sp, 0, 0.03),
    #twet2 = rnorm(n_sp, 0, 0.02),
    pprec = rnorm(n_sp, 0, 0.0008),
    pprec2 = rnorm(n_sp, 0, 0.0000009),
    #pseas = rnorm(n_sp, 0, 0.02),
    #pseas2 = rnorm(n_sp, 0, 0.0005),
    #pwarm = rnorm(n_sp, 0, 0.0006),
    #pwarm2 = rnorm(n_sp, 0, 0.0000008),
    int = rnorm(n_sp, 0, 0.1) # was 0.02
  )
  species_abund <- exp(beta_group["int"]+species_data$int+(beta_group["ttemp"]+species_data$ttemp)*covs$ttemp+
                         (beta_group["ttemp2"]+species_data$ttemp2)*(covs$ttemp-cons_group["ttemp"])^2+
                         #(beta_group["tiso"]+species_data$tiso)*covs$tiso+
                         #(beta_group["tseas"]+species_data$tseas)*covs$tseas+
                         #(beta_group["twet"]+species_data$twet)*covs$twet+(beta_group["twet2"]+species_data$twet2)*(covs$twet-cons_group["twet"])^2+
                         (beta_group["pprec"]+species_data$pprec)*covs$pprec+(beta_group["pprec2"]+species_data$pprec2)*(covs$pprec-cons_group["pprec"])^2)#+
  #(beta_group["pseas"]+species_data$pseas)*covs$pseas+(beta_group["pseas2"]+species_data$pseas2)*(covs$pseas-cons_group["pseas"])^2+
  #(beta_group["pwarm"]+species_data$pwarm)*covs$pwarm+(beta_group["pwarm2"]+species_data$pwarm2)*(covs$pwarm-cons_group["pwarm"])^2)

  rel_species_abund <- rast(lapply(species_abund, rescale_abundance))
  
  prob_pres <- probability_of_presence(rel_species_abund,
                                       max_catch_size)
  plot(prob_pres, main="Prob of Presence")
  writeRaster(prob_pres,
              "data/grids/spec_prob_pres_hgam.tif",
              overwrite = TRUE)
  
  # calculating bounds of covariates
  covs_bounds <- data.frame(
    variable = names(covs),
    min = covs_bounds <- data.frame(
      variable = names(covs),
      min = global(covs, "min", na.rm = TRUE)[, 1],
      max = global(covs, "max", na.rm = TRUE)[, 1]
    )
  )
  
  covs_means <- global(covs, "mean", na.rm=TRUE)[, 1]
  covs_means <- as.data.frame(t(covs_means))
  names(covs_means) <- names(covs)
  
  # calculating "rarest" species - mean of raster is lowest across all species
  means <- global(rel_species_abund, "mean", na.rm = TRUE)[, 1]
  i <- which.min(means)
  sp_bias <- prob_pres[[i]]
  writeRaster(sp_bias,
              "data/grids/sp_bias.tif",
              overwrite = TRUE)
  # bias is travel_bias * rarest_species_bias
  all_bias <- bias*sp_bias
  
  # Simulating data *************************************************************
  # total number of samples - let it be 300 for now
  n_samples <- 300
  # unbiased ###################################################################
  pa_tab <- generate_data_tabular(n_samples, mad_mask, prob_pres=prob_pres)
  write.csv(pa_tab,
            file = "data/tabular/hgam_pa_tab_data_med.csv",
            row.names = FALSE)
  
  ## 2/3 complex
  # defining 2/3 locations to be converted into group data
  n_cp <- round(n_samples*2/3)
  pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)
  
  # save these items
  write.csv(pa_model_data,
            file = "data/tabular/hgam_pa_data_med_nobias_23.csv",
            row.names = FALSE)
  
  # with bias ##################################################################
  pa_tab <- generate_data_tabular(n_samples, all_bias, prob_pres=prob_pres, weighted=TRUE)
  write.csv(pa_tab,
            file = "data/tabular/hgam_pa_tab_data_med_allbiased.csv",
            row.names = FALSE)
  
  plot(all_bias, main="Biased Sample Locations")
  points(pa_tab$x, pa_tab$y, pch = 16)
  
  # number of complex data points
  n_cp <- round(n_samples*2/3)
  pa_model_data <- generate_model_data(n_samples, n_cp, pa_tab)
  
  write.csv(pa_model_data,
            file = "data/tabular/hgam_pa_data_med_allbias_23.csv",
            row.names = FALSE)
  
  # presence-only data **********************************************************
  # unbiased data ###############################################################
  pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_nobias_23.csv")
  occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
  plot(prob_pres[[1]])
  points(occurrence_coords$x, occurrence_coords$y, pch=16)
  
  write.csv(
    x = occurrence_coords,
    file = "data/tabular/presence_only_data_hgam.csv",
    row.names = FALSE
  )
  
  # defining number of background points as 300 (random background)
  n_bg_points <- 300
  random_bg <- random_locations(mad_mask,
                                n_bg_points)
  # adding on simulations to occurrence coords to create po data with random bg
  site_id <- seq(from=max(occurrence_coords$site_id)+1, length.out=nrow(random_bg))
  plot(mad_mask)
  points(random_bg)
  po_tab <- tibble()
  for(letter in c(letters[1:n_sp], "x")){
    po_tab <- rbind(po_tab, bind_cols(
      site_id=site_id,
      as_tibble(crds(random_bg)),
      tibble(
        pa=0,
        sp=rep(letter, length.out=nrow(random_bg)),
        not_complex = ifelse(sp == "x", 0, 1)
      )
    )
    )
  }
  
  # extract covariates at each point
  covs_vals <- extract(x=covs, y=crds(random_bg)) |> select(-any_of(c("not_complex", "sp")))
  covs_vals <- bind_rows(replicate(n_sp+1, covs_vals, simplify=FALSE))
  
  # add to create model data for presence-only points
  po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
    mutate(sp = as.factor(sp))
  
  write.csv(
    x = po_model_data,
    file = "data/tabular/presence_only_data_rbg_hgam.csv",
    row.names = FALSE
  )
  
  # biased data #################################################################
  pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_allbias_23.csv")
  occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
  
  write.csv(
    x = occurrence_coords,
    file = "data/tabular/presence_only_data_allbiased_hgam.csv",
    row.names = FALSE
  )
  
  # add to create model data for presence-only points, use same bg points
  po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
    mutate(sp = as.factor(sp))
  
  write.csv(
    x = po_model_data,
    file = "data/tabular/presence_only_data_allbiased_rbg_hgam.csv",
    row.names = FALSE
  )
  
  # remove group data - use species-only data $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # unbiased data ###############################################################
  pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_nobias_23.csv")
  pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
  write.csv(
    x = pa_model_data,
    file = "data/tabular/hgam_pa_data_med_nobias_23_nocp.csv",
    row.names = FALSE
  )
  
  # biased data #################################################################
  pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_allbias_23.csv")
  pa_model_data <- pa_model_data[pa_model_data$sp != "x",]
  write.csv(
    x = pa_model_data,
    file = "data/tabular/hgam_pa_data_med_allbias_23_nocp.csv",
    row.names = FALSE
  )
  
  # presence-only data **********************************************************
  # species-only data $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # unbiased ####################################################################
  po_model_data <- read_csv("data/tabular/presence_only_data_rbg_hgam.csv")
  po_model_data <- po_model_data[po_model_data$sp != "x",]
  write.csv(
    x = po_model_data,
    file = "data/tabular/presence_only_data_rbg_hgam_nocp.csv",
    row.names = FALSE
  )
  # biased ######################################################################
  po_model_data <- read_csv("data/tabular/presence_only_data_allbiased_rbg_hgam.csv")
  po_model_data <- po_model_data[po_model_data$sp != "x",]
  write.csv(
    x = po_model_data,
    file = "data/tabular/presence_only_data_allbiased_rbg_hgam_nocp.csv",
    row.names = FALSE
  )
  
  # calculating group probability of presence
  group_prob_abs <- app(1-prob_pres, fun=prod, na.rm=TRUE)
  group_prob_pres <- 1-group_prob_abs
  
  # Modeling ********************************************************************
  # presence-absence data! ******************************************************
  pa_model_data <- read_csv("data/tabular/hgam_pa_data_med_nobias_23.csv")
  pa_model_data$sp <- as.factor(pa_model_data$sp)
  # species-only data
  pa_model_data_nocp <- read_csv("data/tabular/hgam_pa_data_med_nobias_23_nocp.csv")
  pa_model_data_nocp$sp <- as.factor(pa_model_data_nocp$sp)
  
  # unbiased ####################################################################
  # model: all data, all smooths
  mos_modGS <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                     t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                        by=not_complex), 
                   data = pa_model_data, 
                   family = "binomial", 
                   method = "REML")
  # model: species-only data, all smooths
  mos_modGS_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                          t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                             by=not_complex), 
                        data = pa_model_data_nocp, 
                        family = "binomial", 
                        method = "REML")
  # model: species-only data, species-only smooths
  mos_modGS_nogp <- gam(pa ~ t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                               by=not_complex), 
                          data = pa_model_data_nocp, 
                          family = "binomial", 
                          method = "REML")
  
  # species
  pred_pa_modGS <- rast(rep(mad_mask, n_sp))
  pred_pa_modGS_nocp <- rast(rep(mad_mask, n_sp))
  pred_pa_modGS_nogp <- rast(rep(mad_mask, n_sp))
  covs$not_complex <- 1
  # data frame to store results
  cor_unbiased[[x]] <- data.frame(CP = 0, NoCP=0, NoGP=0)
  cor_unbiased_sp[[x]] <- data.frame(CP = 0, NoCP=0, NoGP=0)
  # run through all species
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
    pred_pa_modGS_nogp[[i]] <- sdm_predict(
      model = mos_modGS_nogp,
      covariates = covs
    )
    par(mfrow=c(2,2))
    # store results
    cor_unbiased[[x]][i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS[[i]])
    cor_unbiased[[x]][i, 2] <- compute_cor(prob_pres[[i]], pred_pa_modGS_nocp[[i]])
    cor_unbiased[[x]][i, 3] <- compute_cor(prob_pres[[i]], pred_pa_modGS_nogp[[i]])
    cor_unbiased_sp[[x]][i, 1] <- compute_cor_po(prob_pres[[i]], pred_pa_modGS[[i]])
    cor_unbiased_sp[[x]][i, 2] <- compute_cor_po(prob_pres[[i]], pred_pa_modGS_nocp[[i]])
    cor_unbiased_sp[[x]][i, 3] <- compute_cor_po(prob_pres[[i]], pred_pa_modGS_nogp[[i]])
  }
  
  # biased ######################################################################
  pa_model_data_allbiased <- read_csv("data/tabular/hgam_pa_data_med_allbias_23.csv")
  pa_model_data_allbiased$sp <- as.factor(pa_model_data_allbiased$sp)
  # species-only data
  pa_model_data_allbiased_nocp <- read_csv("data/tabular/hgam_pa_data_med_allbias_23_nocp.csv")
  pa_model_data_allbiased_nocp$sp <- as.factor(pa_model_data_allbiased_nocp$sp)
  
  # model: all data, all smooths
  mos_modGS_allbiased <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                               t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                  by=not_complex), 
                             data = pa_model_data_allbiased, 
                             family = "binomial", 
                             method = "REML")
  # model: species-only data, all smooths
  mos_modGS_allbiased_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                    t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                       by=not_complex), 
                                  data = pa_model_data_allbiased_nocp, 
                                  family = "binomial", 
                                  method = "REML")
  # model: species-only data, species-only smooths
  mos_modGS_allbiased_nogp <- gam(pa ~ t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                       by=not_complex), 
                                  data = pa_model_data_allbiased_nocp, 
                                  family = "binomial", 
                                  method = "REML")
  
  # species
  pred_pa_modGS_allbiased <- rast(rep(mad_mask, n_sp))
  pred_pa_modGS_allbiased_nocp <- rast(rep(mad_mask, n_sp))
  pred_pa_modGS_allbiased_nogp <- rast(rep(mad_mask, n_sp))
  covs$not_complex <- 1
  # data frame to store results
  cor_allbiased[[x]] <- data.frame(CP = 0, NoCP=0, NoGP=0)
  cor_allbiased_sp[[x]] <- data.frame(CP = 0, NoCP=0, NoGP=0)
  # go through all species
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
    pred_pa_modGS_allbiased_nogp[[i]] <- sdm_predict(
      model = mos_modGS_allbiased_nogp,
      covariates = covs
    )
    par(mfrow=c(2,2))
    # store results
    cor_allbiased[[x]][i, 1] <- compute_cor(prob_pres[[i]], pred_pa_modGS_allbiased[[i]])
    cor_allbiased[[x]][i, 2] <- compute_cor(prob_pres[[i]], pred_pa_modGS_allbiased_nocp[[i]])
    cor_allbiased[[x]][i, 3] <- compute_cor(prob_pres[[i]], pred_pa_modGS_allbiased_nogp[[i]])
    cor_allbiased_sp[[x]][i, 1] <- compute_cor_po(prob_pres[[i]], pred_pa_modGS_allbiased[[i]])
    cor_allbiased_sp[[x]][i, 2] <- compute_cor_po(prob_pres[[i]], pred_pa_modGS_allbiased_nocp[[i]])
    cor_allbiased_sp[[x]][i, 3] <- compute_cor_po(prob_pres[[i]], pred_pa_modGS_allbiased_nogp[[i]])
  }
  
  # presence-only data! *********************************************************
  po_model_data <- read_csv("data/tabular/presence_only_data_rbg_hgam.csv")
  po_model_data$sp <- as.factor(po_model_data$sp)
  # species-only data
  po_model_data_nocp <- read_csv("data/tabular/presence_only_data_rbg_hgam_nocp.csv")
  po_model_data_nocp$sp <- as.factor(po_model_data_nocp$sp)
  
  # unbiased ####################################################################
  # model: all data, all smooths
  mos_modGS_rbg <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                         t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                            by=not_complex), 
                       data = po_model_data, 
                       family = "binomial", 
                       method = "REML")
  # model: species-only data, all smooths
  mos_modGS_rbg_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                              t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                 by=not_complex), 
                            data = po_model_data_nocp, 
                            family = "binomial", 
                            method = "REML")
  # model: species-only data, species-only smooths
  mos_modGS_rbg_nogp <- gam(pa ~ t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                 by=not_complex), 
                            data = po_model_data_nocp, 
                            family = "binomial", 
                            method = "REML")
  
  # species
  pred_po_modGS <- rast(rep(mad_mask, n_sp))
  pred_po_modGS_nocp <- rast(rep(mad_mask, n_sp))
  pred_po_modGS_nogp <- rast(rep(mad_mask, n_sp))
  covs$not_complex <- 1
  # data frame to store results
  cor_po_unbiased[[x]] <- data.frame(CP = 0, NoCP=0, NoGP=0)
  cor_po_unbiased_sp[[x]] <- data.frame(CP = 0, NoCP=0, NoGP=0)
  # go through all species
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
    pred_po_modGS_nogp[[i]] <- sdm_predict(
      model = mos_modGS_rbg_nogp,
      covariates = covs
    )
    par(mfrow=c(2,2))
    # store results
    cor_po_unbiased[[x]][i, 1] <- compute_cor_po(prob_pres[[i]], pred_po_modGS[[i]])
    cor_po_unbiased[[x]][i, 2] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_nocp[[i]])
    cor_po_unbiased[[x]][i, 3] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_nogp[[i]])
    cor_po_unbiased_sp[[x]][i, 1] <- compute_cor_po(prob_pres[[i]], pred_po_modGS[[i]])
    cor_po_unbiased_sp[[x]][i, 2] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_nocp[[i]])
    cor_po_unbiased_sp[[x]][i, 3] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_nogp[[i]])
  }
  
  # biased po data ##############################################################
  po_model_data_allbiased <- read_csv("data/tabular/presence_only_data_allbiased_rbg_hgam.csv")
  po_model_data_allbiased$sp <- as.factor(po_model_data_allbiased$sp)
  # species-only data
  po_model_data_allbiased_nocp <- read_csv("data/tabular/presence_only_data_allbiased_rbg_hgam_nocp.csv")
  po_model_data_allbiased_nocp$sp <- as.factor(po_model_data_allbiased_nocp$sp)
  
  # model: all data, all smooths
  mos_modGS_allbiased_rbg <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                   t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                      by=not_complex), 
                                 data = po_model_data_allbiased, 
                                 family = "binomial", 
                                 method = "REML")
  # model: species-only data, all smooths
  mos_modGS_allbiased_rbg_nocp <- gam(pa ~ te(ttemp, pprec, bs=c("tp", "tp")) + 
                                        t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                           by=not_complex), 
                                      data = po_model_data_allbiased_nocp, 
                                      family = "binomial", 
                                      method = "REML")
  # model: species-only data, species-only smooths
  mos_modGS_allbiased_rbg_nogp <- gam(pa ~ t2(ttemp, pprec, sp, bs=c("tp", "tp", "re"),
                                           by=not_complex), 
                                      data = po_model_data_allbiased_nocp, 
                                      family = "binomial", 
                                      method = "REML")
  
  # species
  pred_po_modGS_allbiased <- rast(rep(mad_mask, n_sp))
  pred_po_modGS_allbiased_nocp <- rast(rep(mad_mask, n_sp))
  pred_po_modGS_allbiased_nogp <- rast(rep(mad_mask, n_sp))
  covs$not_complex <- 1
  # data frame to store results
  cor_po_allbiased[[x]] <- data.frame(CP = 0, NoCP=0, NoGP=0)
  cor_po_allbiased_sp[[x]] <- data.frame(CP = 0, NoCP=0, NoGP=0)
  # go through all species
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
    pred_po_modGS_allbiased_nogp[[i]] <- sdm_predict(
      model = mos_modGS_allbiased_rbg_nogp,
      covariates = covs
    )
    par(mfrow=c(2,2))
    # store results
    cor_po_allbiased[[x]][i, 1] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_allbiased[[i]])
    cor_po_allbiased[[x]][i, 2] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_allbiased_nocp[[i]])
    cor_po_allbiased[[x]][i, 3] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_allbiased_nogp[[i]])
    cor_po_allbiased_sp[[x]][i, 1] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_allbiased[[i]])
    cor_po_allbiased_sp[[x]][i, 2] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_allbiased_nocp[[i]])
    cor_po_allbiased_sp[[x]][i, 3] <- compute_cor_po(prob_pres[[i]], pred_po_modGS_allbiased_nogp[[i]])
  }
}

# Evaluating metrics ************************************************************
# adjust and convert data frames into valid formats
cor_unbiased <- bind_rows(cor_unbiased, .id="column_label") %>% select(-column_label)
cor_allbiased <- bind_rows(cor_allbiased, .id="column_label") %>% select(-column_label)
cor_po_unbiased <- bind_rows(cor_po_unbiased, .id="column_label") %>% select(-column_label)
cor_po_allbiased <- bind_rows(cor_po_allbiased, .id="column_label") %>% select(-column_label)

write.csv(cor_unbiased,
          file = "data/tabular/cor_unbiased_23.csv",
          row.names = FALSE)
write.csv(cor_allbiased,
          file = "data/tabular/cor_allbiased_23.csv",
          row.names = FALSE)
write.csv(cor_po_unbiased,
          file = "data/tabular/cor_po_unbiased_23.csv",
          row.names = FALSE)
write.csv(cor_po_allbiased,
          file = "data/tabular/cor_po_allbiased_23.csv",
          row.names = FALSE)

cor_unbiased_sp <- bind_rows(cor_unbiased_sp, .id="column_label") %>% select(-column_label)
cor_allbiased_sp <- bind_rows(cor_allbiased_sp, .id="column_label") %>% select(-column_label)
cor_po_unbiased_sp <- bind_rows(cor_po_unbiased_sp, .id="column_label") %>% select(-column_label)
cor_po_allbiased_sp <- bind_rows(cor_po_allbiased_sp, .id="column_label") %>% select(-column_label)

write.csv(cor_unbiased_sp,
          file = "data/tabular/cor_unbiased_sp_23.csv",
          row.names = FALSE)
write.csv(cor_allbiased_sp,
          file = "data/tabular/cor_allbiased_sp_23.csv",
          row.names = FALSE)
write.csv(cor_po_unbiased_sp,
          file = "data/tabular/cor_po_unbiased_sp_23.csv",
          row.names = FALSE)
write.csv(cor_po_allbiased_sp,
          file = "data/tabular/cor_po_allbiased_sp_23.csv",
          row.names = FALSE)

# can start from here if simulations finished
cor_unbiased <- read_csv("data/tabular/cor_unbiased_23.csv")
cor_allbiased <- read_csv("data/tabular/cor_allbiased_23.csv")
cor_po_unbiased <- read_csv("data/tabular/cor_po_unbiased_23.csv")
cor_po_allbiased <- read_csv("data/tabular/cor_po_allbiased_23.csv")

# rename everything
cor_unbiased <- cor_unbiased |> rename("All Data, All Smooths"=CP, "Species-Only Data, All Smooths"=NoCP, "Species-Only Smooths"=NoGP)
cor_allbiased <- cor_allbiased |> rename("All Data, All Smooths"=CP, "Species-Only Data, All Smooths"=NoCP, "Species-Only Smooths"=NoGP)
cor_po_unbiased <- cor_po_unbiased |> rename("All Data, All Smooths"=CP, "Species-Only Data, All Smooths"=NoCP, "Species-Only Smooths"=NoGP)
cor_po_allbiased <- cor_po_allbiased |> rename("All Data, All Smooths"=CP, "Species-Only Data, All Smooths"=NoCP, "Species-Only Smooths"=NoGP)

# compute averages of correlations
cor_unbiased_avg <- cor_unbiased %>%
  colMeans
cor_unbiased_avg
cor_allbiased_avg <- cor_allbiased %>%
  colMeans
cor_allbiased_avg
cor_po_unbiased_avg <- cor_po_unbiased %>%
  colMeans
cor_po_unbiased_avg
cor_po_allbiased_avg <- cor_po_allbiased %>%
  colMeans
cor_po_allbiased_avg

# calculate overall how many of each model was the winner
max_col_unbiased <- max.col(cor_unbiased)
max_unbiased <- table(max_col_unbiased)
names(max_unbiased) <- c("All Data, All Smooths", "Species-Only Data, All Smooths", "Species-Only Smooths")
max_unbiased

max_col_allbiased <- max.col(cor_allbiased)
max_allbiased <- table(max_col_allbiased)
names(max_allbiased) <- c("All Data, All Smooths", "Species-Only Data, All Smooths", "Species-Only Smooths")
max_allbiased

max_col_po_unbiased <- max.col(cor_po_unbiased)
max_po_unbiased <- table(max_col_po_unbiased)
names(max_po_unbiased) <- c("All Data, All Smooths", "Species-Only Data, All Smooths", "Species-Only Smooths")
max_po_unbiased

max_col_po_allbiased <- max.col(cor_po_allbiased)
max_po_allbiased <- table(max_col_po_allbiased)
names(max_po_allbiased) <- c("All Data, All Smooths", "Species-Only Data, All Smooths", "Species-Only Smooths")
max_po_allbiased

# convert into better format for analysis
df1 <- make_long(cor_unbiased, "PA Unbiased")
df2 <- make_long(cor_allbiased, "PA Biased")
df3 <- make_long(cor_po_unbiased, "PO Unbiased")
df4 <- make_long(cor_po_allbiased, "PO Biased")

# compute heatmap of best average correlation and sds
all_data <- bind_rows(df1, df2, df3, df4)
summary_df <- all_data %>%
  group_by(Type, Model) %>%
  summarize(
    Mean = mean(Correlation),
    SD = sd(Correlation),
    .groups = "drop"
  ) %>%
  mutate(Label = paste0(
    sprintf("%.4f", Mean), "\n(",
    sprintf("%.4f", SD), ")"
  ))

summary_df$Model <- factor(summary_df$Model, levels=c("All Data, All Smooths", "Species-Only Data, All Smooths", "Species-Only Smooths"))
# plot heatmap
ggplot(summary_df, aes(x = Model, y = Type, fill = Mean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label), color = "black", size = 4.2, lineheight = 0.9) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_x_discrete(labels = c(
    CP = "All Data, All Smooths",
    NoCP = "Species-Only Data, All Smooths",
    NoGP = "Species-Only Smooths"
  )) +
  labs(
    title = "Model Performance by Type",
    fill = "Mean",
    x = "Model Type",
    y = "Data Type"
  ) +
  labs(
    title = "Model Performance by Type",
    fill = "Mean Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )

# compute table for best performance across each simulation
perf <- rbind(
  'PA Unbiased' = max_unbiased,
  'PA Biased' = max_allbiased,
  'PO Unbiased' = max_po_unbiased,
  'PO Biased' = max_po_allbiased
)
perf
write.csv(
  perf,
  file = "data/tabular/best_model_performance.csv",
  row.names = FALSE
)

col_names <- c("All Data, All Smooths", "Species-Only Data, All Smooths", "Species-Only Smooths")
# data frame for easier analysis
cor_unbiased_long <- cor_unbiased %>%
  mutate(Row = row_number()) %>%
  pivot_longer(cols = col_names, names_to = "Model", values_to = "Value")
cor_allbiased_long <- cor_allbiased %>%
  mutate(Row = row_number()) %>%
  pivot_longer(cols = col_names, names_to = "Model", values_to = "Value")
cor_po_unbiased_long <- cor_po_unbiased %>%
  mutate(Row = row_number()) %>%
  pivot_longer(cols = col_names, names_to = "Model", values_to = "Value")
cor_po_allbiased_long <- cor_po_allbiased %>%
  mutate(Row = row_number()) %>%
  pivot_longer(cols = col_names, names_to = "Model", values_to = "Value")

cor_unbiased_long$Model <- factor(cor_unbiased_long$Model, levels = col_names)
cor_allbiased_long$Model <- factor(cor_allbiased_long$Model, levels = col_names)
cor_po_unbiased_long$Model <- factor(cor_po_unbiased_long$Model, levels = col_names)
cor_po_allbiased_long$Model <- factor(cor_po_allbiased_long$Model, levels = col_names)

# compute means for correlations if you want to add to plot of correlations later
model_means_unbiased <- cor_unbiased_long %>%
  group_by(Model) %>%
  summarize(mean_val = mean(Value), .groups = "drop")
model_means_allbiased <- cor_allbiased_long %>%
  group_by(Model) %>%
  summarize(mean_val = mean(Value), .groups = "drop")
model_means_po_unbiased <- cor_po_unbiased_long %>%
  group_by(Model) %>%
  summarize(mean_val = mean(Value), .groups = "drop")
model_means_po_allbiased <- cor_po_allbiased_long %>%
  group_by(Model) %>%
  summarize(mean_val = mean(Value), .groups = "drop")

summary(cor_unbiased)
# plot all correlation values in different colors
ggplot(cor_unbiased_long, aes(x = Row, y = Value, color = Model)) +
  geom_point(size = 2) +
  # can add code below if you want mean line
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black", 
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "red")
  ) +
  labs(
    title = "Correlation Values by Model (Unbiased PA)",
    x = "Observation",
    y = "Correlation",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggplot(cor_allbiased_long, aes(x = Row, y = Value, color = Model)) +
  geom_point(size = 2) +
  # can add code below if you want mean line
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black",  
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "red")
  ) +
  labs(
    title = "Correlation Values by Model (Biased PA)",
    x = "Observation",
    y = "Correlation",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggplot(cor_po_unbiased_long, aes(x = Row, y = Value, color = Model)) +
  geom_point(size = 2) +
  # can add code below if you want mean line
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black", 
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "red")
  ) +
  labs(
    title = "Correlation Values by Model (Unbiased PO)",
    x = "Observation",
    y = "Correlation",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggplot(cor_po_allbiased_long, aes(x = Row, y = Value, color = Model)) +
  geom_point(size = 2) +
  # can add code below if you want mean line
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black", 
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "red")
  ) +
  labs(
    title = "Correlation Values by Model (Biased PO)",
    x = "Observation",
    y = "Correlation",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

## Boxplots
ggplot(cor_unbiased_long, aes(x=Model, y=Value, color=Model)) +
  geom_boxplot(fill=NA, size=0.5) +
  scale_color_manual(values = c("All Data, All Smooths" = "black", 
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "red")) +
  labs(
    title = "Boxplot of Correlation Values by Model (Unbiased PA)",
    x = "Model",
    y = "Correlation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

ggplot(cor_allbiased_long, aes(x=Model, y=Value, color=Model)) +
  geom_boxplot(fill=NA, size=0.5) +
  scale_color_manual(values = c("All Data, All Smooths" = "black", 
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "red")) +
  labs(
    title = "Boxplot of Correlation Values by Model (Biased PA)",
    x = "Model",
    y = "Correlation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

ggplot(cor_po_unbiased_long, aes(x=Model, y=Value, color=Model)) +
  geom_boxplot(fill=NA, size=0.5) +
  scale_color_manual(values = c("All Data, All Smooths" = "black",
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "red")) +
  labs(
    title = "Boxplot of Correlation Values by Model (Unbiased PO)",
    x = "Model",
    y = "Correlation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

ggplot(cor_po_allbiased_long, aes(x=Model, y=Value, color=Model)) +
  geom_boxplot(fill=NA, size=0.5) +
  scale_color_manual(values = c("All Data, All Smooths" = "black", 
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "red")) +
  labs(
    title = "Boxplot of Correlation Values by Model (Biased PO)",
    x = "Model",
    y = "Correlation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )
colnames(perf) <- c("CP", "NoCP", "NoGP")

# create table for best-performing models
table_max <- data.frame(perf) %>%
  mutate(type = rownames(.)) %>%
  pivot_longer(cols =  c(CP, NoCP, NoGP), names_to = "Model", values_to = "Max") %>%
  mutate(Model=recode(Model,
                      CP = "All Data, All Smooths",
                      NoCP = "Species-Only Data, All Smooths",
                      NoGP = "Species-Only Smooths"
  ))

table_max$Model <- factor(table_max$Model, levels = col_names)
# can plot it as a stacked bar chart
ggplot(table_max, aes(x=type,fill=Model)) + 
  geom_bar(stat="identity",position="fill", aes(y=Max)) +
  labs(
    title = "Proportion of Best-Performing Models By Data Type",
    x="Data Type",
    y="Proportion",
    fill="Model"
  ) +
  theme_minimal() +
  theme(
    plot.title=element_text(hjust=0.5)
  )
