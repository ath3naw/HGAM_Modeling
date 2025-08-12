rm(list=ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
library(mgcv)
source("R/hgam_multiple_layers/functions_multiple.R")

# can add a seed
set.seed(136)

# load in variables
par(mfrow = c(1,1))
covs <- terra::rast("data/grids/covariates.tif")
mad_mask <- terra::rast("data/grids/mad_mask.tif")
bc_mad <- terra::rast("data/grids/bc_mad.tif")
bias <- terra::rast("data/grids/bias.tif")

# define numbers/initializing variables
n_cp <- 2
n_sp <- c(5,7)
n_sp <- data.frame(n_sp)
write.csv(n_sp,
          file = "data/tabular/hgam_multiple_layers/n_sp",
          row.names = FALSE)

# max catch size of mosquitoes - 20
max_catch_size <- 20

# group level model
# can change covariates as needed
beta_group <- c(ttemp = 0.02, ttemp2 = -0.2, 
                #tiso = 0.001, 
                #tseas = -0.001, 
                #twet = 0.08, twet2 = -0.02, 
                pprec = 0.001, pprec2 = -0.000006,
                #pseas = 0.04, pseas2 = -0.0007, pwarm = 0.0002, pwarm2 = -0.000001, 
                int=0.2) # int was 0.03

cons_group <- c(ttemp = 22, 
                #twet = 24, 
                pprec = 1400)#, pseas = 80, pwarm=1000)

group_abund <- exp(beta_group["int"]+beta_group["ttemp"]*covs$ttemp+beta_group["ttemp2"]*(covs$ttemp-cons_group["ttemp"])^2+
                     #beta_group["tiso"]*covs$tiso+
                     #beta_group["tseas"]*covs$tseas+
                     #beta_group["twet"]*covs$twet+beta_group["twet2"]*(covs$twet-cons_group["twet"])^2+
                     beta_group["pprec"]*covs$pprec+beta_group["pprec2"]*(covs$pprec-cons_group["pprec"])^2)#+
#beta_group["pseas"]*covs$pseas+beta_group["pseas2"]*(covs$pseas-cons_group["pseas"])^2+
#beta_group["pwarm"]*covs$pwarm+beta_group["pwarm2"]*(covs$pwarm-cons_group["pwarm"])^2)
plot(group_abund)

# getting probability of presence
rel_group_abund <- rescale_abundance(group_abund)
names(rel_group_abund) <- "relative_abundance"
plot(rel_group_abund, main="Relative Abundance")
group_prob_pres <- probability_of_presence(rel_group_abund,
                                           max_catch_size)
plot(group_prob_pres, main="Prob of Presence")
writeRaster(group_prob_pres,
            "data/grids/hgam_multiple_layers/group_prob_pres_hgam.tif",
            overwrite = TRUE)

# number of simulations
n <- 10

# initialize variables for the correlations
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

# running n number of simulations # on part 10, unbiased presence-absence data
for(x in 1:n){
  # Preparing raster data *******************************************************
  # complex-level deviations
  complex_data <- data.frame(
    complex = factor(paste("cp", 1:n_cp, sep="")),
    ttemp = rnorm(n_cp, 0, 0.03),
    ttemp2 = rnorm(n_cp, 0, 0.1),
    #tiso = rnorm(n_cp, 0, 0.003),
    #tseas = rnorm(n_cp, 0, 0.0025),
    #twet = rnorm(n_cp, 0, 0.03),
    #twet2 = rnorm(n_cp, 0, 0.02),
    pprec = rnorm(n_cp, 0, 0.0008),
    pprec2 = rnorm(n_cp, 0, 0.000002),
    #pseas = rnorm(n_cp, 0, 0.02),
    #pseas2 = rnorm(n_cp, 0, 0.0005),
    #pwarm = rnorm(n_cp, 0, 0.0006),
    #pwarm2 = rnorm(n_cp, 0, 0.0000008),
    int = rnorm(n_cp, 0, 0.3) # was 0.02
  )
  complex_abund <- exp(beta_group["int"]+complex_data$int+
                         (beta_group["ttemp"]+complex_data$ttemp)*covs$ttemp+
                         (beta_group["ttemp2"]+complex_data$ttemp2)*(covs$ttemp-cons_group["ttemp"])^2+
                         #beta_group["tiso"]*covs$tiso+
                         #beta_group["tseas"]*covs$tseas+
                         #beta_group["twet"]*covs$twet+beta_group["twet2"]*(covs$twet-cons_group["twet"])^2+
                         (beta_group["pprec"]+complex_data$pprec)*covs$pprec+
                         (beta_group["pprec2"]+complex_data$pprec2)*(covs$pprec-cons_group["pprec"])^2)#+
  #beta_group["pseas"]*covs$pseas+beta_group["pseas2"]*(covs$pseas-cons_group["pseas"])^2+
  #beta_group["pwarm"]*covs$pwarm+beta_group["pwarm2"]*(covs$pwarm-cons_group["pwarm"])^2)
  rel_complex_abund <- rast(lapply(complex_abund, rescale_abundance))
  prob_pres_cp <- probability_of_presence(rel_complex_abund,
                                          max_catch_size)
  plot(prob_pres_cp, main="Prob of Presence")
  writeRaster(prob_pres_cp,
              "data/grids/hgam_multiple_layers/complex_prob_pres_hgam.tif",
              overwrite = TRUE)
  
  # species-level deviations
  species_data <- data.frame()
  for(i in 1:n_cp){
    species_data <- rbind(species_data, data.frame(
      complex = factor(paste("cp", i, sep="")),
      species = factor(paste("sp", 1:n_sp[i,], sep="")),
      ttemp = rnorm(n_sp[i,], 0, 0.01), # was 0.008
      ttemp2 = rnorm(n_sp[i,], 0, 0.08), # was 0.03
      #tiso = rnorm(n_sp[i,], 0, 0.003),# was 0.003
      #tseas = rnorm(n_sp[i,], 0, 0.0025),
      #twet = rnorm(n_sp[i,], 0, 0.03),
      #twet2 = rnorm(n_sp[i,], 0, 0.02),
      pprec = rnorm(n_sp[i,], 0, 0.0006),# was 0.0004
      pprec2 = rnorm(n_sp[i,], 0, 0.0000008), # was 0.0000008
      #pseas = rnorm(n_sp[i,], 0, 0.02),
      #pseas2 = rnorm(n_sp[i,], 0, 0.0005),
      #pwarm = rnorm(n_sp[i,], 0, 0.0006),
      #pwarm2 = rnorm(n_sp[i,], 0, 0.0000008),
      int = rnorm(n_sp[i,], 0, 0.2) # was 0.02
    )
    )
  }
  species_data <- cbind(species_id = 1:nrow(species_data), species_data)
  
  # creating a data frame to match the species_data to the complex data it is correlated with
  complex_match <- match(species_data$complex, complex_data$complex)
  cpx <- complex_data[complex_match,]
  
  species_abund <- exp(beta_group["int"]+cpx$int+species_data$int+
                         (beta_group["ttemp"]+cpx$ttemp+species_data$ttemp)*covs$ttemp+
                         (beta_group["ttemp2"]+cpx$ttemp2+species_data$ttemp2)*(covs$ttemp-cons_group["ttemp"])^2+
                         #(beta_group["tiso"]+species_data$tiso)*covs$tiso+
                         #(beta_group["tseas"]+species_data$tseas)*covs$tseas+
                         #(beta_group["twet"]+species_data$twet)*covs$twet+(beta_group["twet2"]+species_data$twet2)*(covs$twet-cons_group["twet"])^2+
                         (beta_group["pprec"]+cpx$pprec+species_data$pprec)*covs$pprec+
                         (beta_group["pprec2"]+cpx$pprec2+species_data$pprec2)*(covs$pprec-cons_group["pprec"])^2)#+
  #(beta_group["pseas"]+species_data$pseas)*covs$pseas+(beta_group["pseas2"]+species_data$pseas2)*(covs$pseas-cons_group["pseas"])^2+
  #(beta_group["pwarm"]+species_data$pwarm)*covs$pwarm+(beta_group["pwarm2"]+species_data$pwarm2)*(covs$pwarm-cons_group["pwarm"])^2)
  rel_species_abund <- rast(lapply(species_abund, rescale_abundance))  
  prob_pres_sp <- probability_of_presence(rel_species_abund,
                                       max_catch_size)
  plot(prob_pres_sp, main="Prob of Presence")
  writeRaster(prob_pres_sp,
              "data/grids/hgam_multiple_layers/spec_prob_pres_hgam.tif",
              overwrite = TRUE)
  
  #getting the covs_bounds for the partial-response plots and later use
  covs_bounds <- data.frame(
    variable = names(covs),
    min = covs_bounds <- data.frame(
      variable = names(covs),
      min = global(covs, "min", na.rm = TRUE)[, 1],
      max = global(covs, "max", na.rm = TRUE)[, 1]
    )
  )
  
  # getting means of the covariate rasters
  covs_means <- global(covs, "mean", na.rm=TRUE)[, 1]
  covs_means <- as.data.frame(t(covs_means))
  names(covs_means) <- names(covs)
  
  # find "rarest" species
  means <- global(rel_species_abund, "mean", na.rm = TRUE)[, 1]
  i <- which.min(means)
  sp_bias <- prob_pres_sp[[i]]
  writeRaster(sp_bias,
              "data/grids/sp_bias.tif",
              overwrite = TRUE)
  all_bias <- bias*sp_bias
  
  # Simulating data *************************************************************
  # number of samples similar to leuco data
  n_samples <- 149
  num_species <- nlyr(prob_pres_sp)
  
  pa_tab <- generate_data_tabular(n_samples, mad_mask, prob_pres=prob_pres_sp, n_sp=n_sp)
  write.csv(pa_tab,
            file = "data/tabular/hgam_multiple_layers/hgam_pa_tab_data_med.csv",
            row.names = FALSE)
  
  # picking number of complex points, etc.
  n_group <- 49
  n_complex <- c(49, 73)
  
  pa_model_data <- generate_model_data(n_samples, n_group, n_complex, n_cp, n_sp, pa_tab)
  
  # save these items - unbiased ###################################################
  write.csv(pa_model_data,
            file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias.csv",
            row.names = FALSE)
  
  # no complex $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias.csv")
  pa_model_data <- pa_model_data[!pa_model_data$sp %in% c("complex1", "complex2"),]
  write.csv(
    x = pa_model_data,
    file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias_gp.csv",
    row.names = FALSE
  )
  
  # species-only $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias.csv")
  pa_model_data <- pa_model_data[!pa_model_data$sp %in% c("group", "complex1", "complex2"),]
  write.csv(
    x = pa_model_data,
    file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias_nocp.csv",
    row.names = FALSE
  )
  
  ### with bias, all bias #########################################################
  pa_tab <- generate_data_tabular(n_samples, all_bias, prob_pres=prob_pres_sp, n_sp=n_sp, weighted=TRUE)
  write.csv(pa_tab,
            file = "data/tabular/hgam_multiple_layers/hgam_pa_tab_data_med_allbiased.csv",
            row.names = FALSE)
  
  plot(all_bias, main="Biased Sample Locations")
  points(pa_tab$x, pa_tab$y, pch = 16)
  
  # number of complex data points
  # all data ####################################################################
  pa_model_data <- generate_model_data(n_samples, n_group, n_complex, n_cp, n_sp, pa_tab)
  
  write.csv(pa_model_data,
            file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias.csv",
            row.names = FALSE)
  
  pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias.csv")
  
  # no complex ##################################################################
  pa_model_data <- pa_model_data[!pa_model_data$sp %in% c("complex1", "complex2"),]
  write.csv(
    x = pa_model_data,
    file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias_gp.csv",
    row.names = FALSE
  )
  
  # species only ################################################################
  pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias.csv")
  pa_model_data <- pa_model_data[!pa_model_data$sp %in% c("group", "complex1", "complex2"),]
  write.csv(
    x = pa_model_data,
    file = "data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias_nocp.csv",
    row.names = FALSE
  )
  
  
  ### presence-only data ********************************************************
  ## unbiased data ##############################################################
  pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias.csv")
  occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
  plot(prob_pres_sp[[1]])
  points(occurrence_coords[occurrence_coords$sp==1,]$x, occurrence_coords[occurrence_coords$sp==1,]$y, pch=16)

  write.csv(
    x = occurrence_coords,
    file = "data/tabular/hgam_multiple_layers/presence_only_data_hgam.csv",
    row.names = FALSE
  )
  
  # picking number of background points for accuracy + speed
  n_bg_points <- 175
  # number of groups = 12 species + 2 complexes + 1 group = 15
  num_groups <- 15
  
  # adjusting + adding data for background points
  random_bg <- random_locations(mad_mask,
                                n_bg_points)
  site_id <- seq(from=max(occurrence_coords$site_id)+1, length.out=nrow(random_bg))
  
  # checking background
  plot(mad_mask)
  points(random_bg)
  
  # adjusting data formatting
  complex_names <- paste0("complex", 1:n_cp)
  po_tab <- tibble()
  for(i in c(1:num_species, complex_names, "group")){
    po_tab <- rbind(po_tab, bind_cols(
      site_id=site_id,
      as_tibble(crds(random_bg)),
      tibble(
        pa=0,
        sp=rep(i, length.out=nrow(random_bg)),
        not_complex = ifelse(sp %in% c(complex_names, "group"), 0, 1)
      )
    )
    )
  }
  j <- 0
  for(i in 1:n_cp){
    po_tab <- po_tab |> 
      mutate(!!complex_names[i] := ifelse(sp %in% c(complex_names[i], (1+j):(n_sp[i,]+j)),1 ,0))
    j <- j+n_sp[i,]
  }
  
  # getting covariate values
  covs_vals <- extract(x=covs, y=crds(random_bg)) |> select(-any_of(c("not_complex", "sp", "complex1", "complex2")))
  covs_vals <- bind_rows(replicate(num_groups, covs_vals, simplify=FALSE))
  
  # combining all data into model_data format
  po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
    mutate(sp = as.factor(sp))
  
  write.csv(
    x = po_model_data,
    file = "data/tabular/hgam_multiple_layers/presence_only_data_rbg_hgam.csv",
    row.names = FALSE
  )
  
  # biased ######################################################################
  pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias.csv")
  occurrence_coords <- pa_model_data[pa_model_data$pa==1,]
  
  # presence_only ***************************************************************
  write.csv(
    x = occurrence_coords,
    file = "data/tabular/hgam_multiple_layers/presence_only_data_allbiased_hgam.csv",
    row.names = FALSE
  )
  
  po_model_data <- rbind(occurrence_coords, bind_cols(po_tab, covs_vals)) |>
    mutate(sp = as.factor(sp))
  
  write.csv(
    x = po_model_data,
    file = "data/tabular/hgam_multiple_layers/presence_only_data_allbiased_rbg_hgam.csv",
    row.names = FALSE
  )
  
  # use group and species data, no complex data $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # unbiased po #################################################################
  po_model_data <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_rbg_hgam.csv")
  po_model_data <- po_model_data[!po_model_data$sp %in% c("complex1", "complex2"),]
  write.csv(
    x = po_model_data,
    file = "data/tabular/hgam_multiple_layers/hgam_po_data_med_gp.csv",
    row.names = FALSE
  )
  # biased po ###################################################################
  po_model_data <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_allbiased_rbg_hgam.csv")
  po_model_data <- po_model_data[!po_model_data$sp %in% c("complex1", "complex2"),]
  write.csv(
    x = po_model_data,
    file = "data/tabular/hgam_multiple_layers/hgam_po_data_med_allbias_gp.csv",
    row.names = FALSE
  )
  
  # use species-only data, no group or complex data $$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # unbiased po #################################################################
  po_model_data <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_rbg_hgam.csv")
  po_model_data <- po_model_data[!po_model_data$sp %in% c("group", "complex1", "complex2"),]
  write.csv(
    x = po_model_data,
    file = "data/tabular/hgam_multiple_layers/hgam_po_data_med_nocp.csv",
    row.names = FALSE
  )
  # biased po ###################################################################
  po_model_data <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_allbiased_rbg_hgam.csv")
  po_model_data <- po_model_data[!po_model_data$sp %in% c("group", "complex1", "complex2"),]
  write.csv(
    x = po_model_data,
    file = "data/tabular/hgam_multiple_layers/hgam_po_data_med_allbias_nocp.csv",
    row.names = FALSE
  )
  
  ### Modeling ******************************************************************
  # loading in variables
  prob_pres_sp <- terra::rast("data/grids/hgam_multiple_layers/spec_prob_pres_hgam.tif")
  prob_pres_cp <- terra::rast("data/grids/hgam_multiple_layers/complex_prob_pres_hgam.tif")
  covs <- terra::rast("data/grids/covariates.tif")
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
  
  # generating formula
  smooths <- paste0("te(ttemp, pprec, bs=c('tp', 'tp'), by=complex",1:n_cp,")")
  full_formula <- paste("pa ~ te(ttemp, pprec, bs=c('tp', 'tp')) + ",
                        paste(smooths, collapse = " + "),
                        " + te(ttemp, pprec, sp, bs=c('tp', 'tp', 're'),
                      by=not_complex)")
  full_formula <- as.formula(full_formula)
  full_formula
  part_formula <- as.formula(pa ~ te(ttemp, pprec, sp, bs=c('tp', 'tp', 're'), by=not_complex))
  
  ### presence-absence data! ****************************************************
  pa_model_data <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias.csv")
  pa_model_data$sp <- as.factor(pa_model_data$sp)
  pa_model_data_gp <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias_gp.csv")
  pa_model_data_gp$sp <- as.factor(pa_model_data_gp$sp)
  pa_model_data_nocp <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_nobias_nocp.csv")
  pa_model_data_nocp$sp <- as.factor(pa_model_data_nocp$sp)
  
  ## unbiased ##################################################################
  # model: all data, all smooths
  mos_modGS <- gam(formula = full_formula, 
                   data = pa_model_data,
                   optimizer=c("outer", "bfgs"),
                   family = "binomial", 
                   method = "REML")
  # model: group and species data, all smooths
  mos_modGS_gp <- gam(formula = full_formula, 
                        data = pa_model_data_gp, 
                        optimizer=c("outer", "bfgs"),
                        family = "binomial", 
                        method = "REML")
  # model: species data, all smooths
  mos_modGS_nocp <- gam(formula = full_formula, 
                      data = pa_model_data_nocp, 
                      optimizer=c("outer", "bfgs"),
                      family = "binomial", 
                      method = "REML")
  # model: species data, species smooths
  mos_modGS_nogp <- gam(formula=part_formula, 
                        data = pa_model_data_nocp, 
                        optimizer=c("outer", "bfgs"),
                        family = "binomial", 
                        method = "REML")
  
  # initializing variables for prediction raster
  pred_pa_modGS <- rast(rep(mad_mask, num_species))
  pred_pa_modGS_gp <- rast(rep(mad_mask, num_species))
  pred_pa_modGS_nocp <- rast(rep(mad_mask, num_species))
  pred_pa_modGS_nogp <- rast(rep(mad_mask, num_species))
  covs$not_complex <- 1
  cor_unbiased[[x]] <- data.frame(E = 0, GS=0, S=0, NoG=0) # everything, group + species data, species data, species-only smooth
  cor_unbiased_sp[[x]] <- data.frame(E = 0, GS=0, S=0, NoG=0)
  # looping through all species + predicting
  for(i in 1:num_species){
    covs$sp <- factor(i)
    cp <- species[i, 2]
    for(j in 1:n_cp){
      covs[[paste0("complex", j)]] <- 0
    }
    # Set current complex to 1
    covs[[paste0("complex", cp)]] <- 1
    pred_pa_modGS[[i]] <- sdm_predict(
      model = mos_modGS,
      covariates = covs
    )
    pred_pa_modGS_gp[[i]] <- sdm_predict(
      model = mos_modGS_gp,
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
    par(mfrow=c(2,3))
    plot(prob_pres_sp[[i]], main = "True Prob of Pres")
    plot(pred_pa_modGS[[i]], main = paste("Species", i, "- unbiased CP"), range=c(0,1))
    plot(pred_pa_modGS_gp[[i]], main = paste("Species", i, "- unbiased GP"), range=c(0,1))
    plot(pred_pa_modGS_nocp[[i]], main = paste("Species", i, "- unbiased No CP"), range=c(0,1))
    plot(pred_pa_modGS_nogp[[i]], main = paste("Species", i, "- unbiased No GP"), range=c(0,1))
    
    # computing correlation
    cor_unbiased[[x]][i, 1] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS[[i]])
    cor_unbiased[[x]][i, 2] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS_gp[[i]])
    cor_unbiased[[x]][i, 3] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS_nocp[[i]])
    cor_unbiased[[x]][i, 4] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS_nogp[[i]])
    cor_unbiased_sp[[x]][i, 1] <- compute_cor_po(prob_pres_sp[[i]], pred_pa_modGS[[i]])
    cor_unbiased_sp[[x]][i, 2] <- compute_cor_po(prob_pres_sp[[i]], pred_pa_modGS_gp[[i]])
    cor_unbiased_sp[[x]][i, 3] <- compute_cor_po(prob_pres_sp[[i]], pred_pa_modGS_nocp[[i]])
    cor_unbiased_sp[[x]][i, 4] <- compute_cor_po(prob_pres_sp[[i]], pred_pa_modGS_nogp[[i]])
  }
  

  # biased #####################################################################
  pa_model_data_allbiased <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias.csv")
  pa_model_data_allbiased$sp <- as.factor(pa_model_data_allbiased$sp)
  pa_model_data_allbiased_gp <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias_gp.csv")
  pa_model_data_allbiased_gp$sp <- as.factor(pa_model_data_allbiased_gp$sp)
  pa_model_data_allbiased_nocp <- read_csv("data/tabular/hgam_multiple_layers/hgam_pa_data_med_allbias_nocp.csv")
  pa_model_data_allbiased_nocp$sp <- as.factor(pa_model_data_allbiased_nocp$sp)
  
  # model: all data, all smooths
  mos_modGS_allbiased <- gam(formula = full_formula, 
                             data = pa_model_data_allbiased,
                             optimizer=c("outer", "bfgs"),
                             family = "binomial", 
                             method = "REML")
  # model: group and species data, all smooths
  mos_modGS_allbiased_gp <- gam(formula = full_formula, 
                                data = pa_model_data_allbiased_gp,
                                optimizer=c("outer", "bfgs"),
                                family = "binomial", 
                                method = "REML")
  # model: species data, all smooths
  mos_modGS_allbiased_nocp <- gam(formula = full_formula, 
                                  data = pa_model_data_allbiased_nocp,
                                  optimizer=c("outer", "bfgs"),
                                  family = "binomial", 
                                  method = "REML")
  # model: species data, species smooths
  mos_modGS_allbiased_nogp <- gam(formula = part_formula, 
                                  data = pa_model_data_allbiased_nocp,
                                  optimizer=c("outer", "bfgs"),
                                  family = "binomial", 
                                  method = "REML")
  
  # initializing variables for prediction raster
  pred_pa_modGS_allbiased <- rast(rep(mad_mask, num_species))
  pred_pa_modGS_allbiased_gp <- rast(rep(mad_mask, num_species))
  pred_pa_modGS_allbiased_nocp <- rast(rep(mad_mask, num_species))
  pred_pa_modGS_allbiased_nogp <- rast(rep(mad_mask, num_species))
  covs$not_complex <- 1
  cor_allbiased[[x]] <- data.frame(E = 0, GS=0, S=0, NoG=0)
  cor_allbiased_sp[[x]] <- data.frame(E = 0, GS=0, S=0, NoG=0)
  
  # looping through all species + predicting
  for(i in 1:num_species){
    covs$sp <- factor(i)
    cp <- species[i, 2]
    for(j in 1:n_cp){
      covs[[paste0("complex", j)]] <- 0
    }
    # Set current complex to 1
    covs[[paste0("complex", cp)]] <- 1
    pred_pa_modGS_allbiased[[i]] <- sdm_predict(
      model = mos_modGS_allbiased,
      covariates = covs
    )
    pred_pa_modGS_allbiased_gp[[i]] <- sdm_predict(
      model = mos_modGS_allbiased_gp,
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
    par(mfrow=c(2,3))
    plot(prob_pres_sp[[i]], main = "True Prob of Pres")
    plot(pred_pa_modGS_allbiased[[i]], main = paste("Species", i, "- biased CP"), range=c(0,1))
    plot(pred_pa_modGS_allbiased_gp[[i]], main = paste("Species", i, "- biased GP"), range=c(0,1))
    plot(pred_pa_modGS_allbiased_nocp[[i]], main = paste("Species", i, "- biased No CP"), range=c(0,1))
    plot(pred_pa_modGS_allbiased_nogp[[i]], main = paste("Species", i, "- biased No GP"), range=c(0,1))
    
    # computing correlations
    cor_allbiased[[x]][i, 1] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS_allbiased[[i]])
    cor_allbiased[[x]][i, 2] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS_allbiased_gp[[i]])
    cor_allbiased[[x]][i, 3] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS_allbiased_nocp[[i]])
    cor_allbiased[[x]][i, 4] <- compute_cor(prob_pres_sp[[i]], pred_pa_modGS_allbiased_nogp[[i]])
    cor_allbiased_sp[[x]][i, 1] <- compute_cor_po(prob_pres_sp[[i]], pred_pa_modGS_allbiased[[i]])
    cor_allbiased_sp[[x]][i, 2] <- compute_cor_po(prob_pres_sp[[i]], pred_pa_modGS_allbiased_gp[[i]])
    cor_allbiased_sp[[x]][i, 3] <- compute_cor_po(prob_pres_sp[[i]], pred_pa_modGS_allbiased_nocp[[i]])
    cor_allbiased_sp[[x]][i, 4] <- compute_cor_po(prob_pres_sp[[i]], pred_pa_modGS_allbiased_nogp[[i]])
  }
  
  ### presence-only data! *******************************************************
  po_model_data <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_rbg_hgam.csv")
  po_model_data$sp <- as.factor(po_model_data$sp)
  po_model_data_gp <- read_csv("data/tabular/hgam_multiple_layers/hgam_po_data_med_gp.csv")
  po_model_data_gp$sp <- as.factor(po_model_data_gp$sp)
  po_model_data_nocp <- read_csv("data/tabular/hgam_multiple_layers/hgam_po_data_med_nocp.csv")
  po_model_data_nocp$sp <- as.factor(po_model_data_nocp$sp)
  
  ## unbiased ###################################################################
  # model: all data, all smooths
  mos_modGS_rbg <- gam(formula = full_formula, 
                       data = po_model_data,
                       optimizer=c("outer", "bfgs"),
                       family = "binomial", 
                       method = "REML")
  # model: group and species data, all smooths
  mos_modGS_rbg_gp <- gam(formula = full_formula, 
                            data = po_model_data_gp,
                            optimizer=c("outer", "bfgs"),
                            family = "binomial", 
                            method = "REML")
  # model: species data, all smooths
  mos_modGS_rbg_nocp <- gam(formula = full_formula, 
                            data = po_model_data_nocp,
                            optimizer=c("outer", "bfgs"),
                            family = "binomial", 
                            method = "REML")
  # model: species data, species smooths
  mos_modGS_rbg_nogp <- gam(formula = part_formula, 
                            data = po_model_data_nocp,
                            optimizer=c("outer", "bfgs"),
                            family = "binomial", 
                            method = "REML")
  
  # initializing variables for prediction raster
  pred_po_modGS <- rast(rep(mad_mask, num_species))
  pred_po_modGS_gp <- rast(rep(mad_mask, num_species))
  pred_po_modGS_nocp <- rast(rep(mad_mask, num_species))
  pred_po_modGS_nogp <- rast(rep(mad_mask, num_species))
  covs$not_complex <- 1
  cor_po_unbiased[[x]] <- data.frame(E = 0, GS=0, S=0, NoG=0)
  cor_po_unbiased_sp[[x]] <- data.frame(E = 0, GS=0, S=0, NoG=0)
  # looping through all species + predicting
  for(i in 1:num_species){
    covs$sp <- factor(i)
    cp <- species[i, 2]
    for(j in 1:n_cp){
      covs[[paste0("complex", j)]] <- 0
    }
    # Set current complex to 1
    covs[[paste0("complex", cp)]] <- 1
    pred_po_modGS[[i]] <- sdm_predict(
      model = mos_modGS_rbg,
      covariates = covs
    )
    pred_po_modGS_gp[[i]] <- sdm_predict(
      model = mos_modGS_rbg_gp,
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
    par(mfrow=c(2,3))
    plot(prob_pres_sp[[i]], main = "True Prob of Pres")
    plot(pred_po_modGS[[i]], main = paste("Species", i, "- unbiased CP"))
    plot(pred_po_modGS_gp[[i]], main = paste("Species", i, "- unbiased GP"))
    plot(pred_po_modGS_nocp[[i]], main = paste("Species", i, "- unbiased No CP"))
    plot(pred_po_modGS_nogp[[i]], main = paste("Species", i, "- unbiased No GP"))
    
    # computing correlations
    cor_po_unbiased[[x]][i, 1] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS[[i]])
    cor_po_unbiased[[x]][i, 2] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_gp[[i]])
    cor_po_unbiased[[x]][i, 3] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_nocp[[i]])
    cor_po_unbiased[[x]][i, 4] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_nogp[[i]])
    cor_po_unbiased_sp[[x]][i, 1] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS[[i]])
    cor_po_unbiased_sp[[x]][i, 2] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_gp[[i]])
    cor_po_unbiased_sp[[x]][i, 3] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_nocp[[i]])
    cor_po_unbiased_sp[[x]][i, 4] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_nogp[[i]])
  }
  
  # biased po data #############################################################
  po_model_data_allbiased <- read_csv("data/tabular/hgam_multiple_layers/presence_only_data_allbiased_rbg_hgam.csv")
  po_model_data_allbiased$sp <- as.factor(po_model_data_allbiased$sp)
  po_model_data_allbiased_gp <- read_csv("data/tabular/hgam_multiple_layers/hgam_po_data_med_allbias_gp.csv")
  po_model_data_allbiased_gp$sp <- as.factor(po_model_data_allbiased_gp$sp)
  po_model_data_allbiased_nocp <- read_csv("data/tabular/hgam_multiple_layers/hgam_po_data_med_allbias_nocp.csv")
  po_model_data_allbiased_nocp$sp <- as.factor(po_model_data_allbiased_nocp$sp)
  
  # model: all data, all smooths
  mos_modGS_allbiased_rbg <- gam(formula = full_formula, 
                                 data = po_model_data_allbiased,
                                 optimizer=c("outer", "bfgs"),
                                 family = "binomial", 
                                 method = "REML")
  # model: group and species data, all smooths
  mos_modGS_allbiased_rbg_gp <- gam(formula = full_formula, 
                                      data = po_model_data_allbiased_gp,
                                      optimizer=c("outer", "bfgs"),
                                      family = "binomial", 
                                      method = "REML")
  # model: species data, all smooths
  mos_modGS_allbiased_rbg_nocp <- gam(formula = full_formula, 
                                      data = po_model_data_allbiased_nocp,
                                      optimizer=c("outer", "bfgs"),
                                      family = "binomial", 
                                      method = "REML")
  # model: species data, species smooths
  mos_modGS_allbiased_rbg_nogp <- gam(formula = part_formula, 
                                      data = po_model_data_allbiased_nocp,
                                      optimizer=c("outer", "bfgs"),
                                      family = "binomial", 
                                      method = "REML")
  
  # initializing variables for prediction raster
  pred_po_modGS_allbiased <- rast(rep(mad_mask, num_species))
  pred_po_modGS_allbiased_gp <- rast(rep(mad_mask, num_species))
  pred_po_modGS_allbiased_nocp <- rast(rep(mad_mask, num_species))
  pred_po_modGS_allbiased_nogp <- rast(rep(mad_mask, num_species))
  covs$not_complex <- 1
  cor_po_allbiased[[x]] <- data.frame(E = 0, GS=0, S=0, NoG=0)
  cor_po_allbiased_sp[[x]] <- data.frame(E = 0, GS=0, S=0, NoG=0)
  # looping through all species + predicting
  for(i in 1:num_species){
    covs$sp <- factor(i)
    cp <- species[i, 2]
    for(j in 1:n_cp){
      covs[[paste0("complex", j)]] <- 0
    }
    # Set current complex to 1
    covs[[paste0("complex", cp)]] <- 1
    pred_po_modGS_allbiased[[i]] <- sdm_predict(
      model = mos_modGS_allbiased_rbg,
      covariates = covs
    )
    pred_po_modGS_allbiased_gp[[i]] <- sdm_predict(
      model = mos_modGS_allbiased_rbg_gp,
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
    par(mfrow=c(2,3))
    plot(prob_pres_sp[[i]], main = "True Prob of Pres")
    plot(pred_po_modGS_allbiased[[i]], main = paste("Species", i, "- biased CP"))
    plot(pred_po_modGS_allbiased_gp[[i]], main = paste("Species", i, "- biased GP"))
    plot(pred_po_modGS_allbiased_nocp[[i]], main = paste("Species", i, "- biased No CP"))
    plot(pred_po_modGS_allbiased_nogp[[i]], main = paste("Species", i, "- biased No GP"))
    
    # computing correlations
    cor_po_allbiased[[x]][i, 1] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_allbiased[[i]])
    cor_po_allbiased[[x]][i, 2] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_allbiased_gp[[i]])
    cor_po_allbiased[[x]][i, 3] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_allbiased_nocp[[i]])
    cor_po_allbiased[[x]][i, 4] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_allbiased_nogp[[i]])
    cor_po_allbiased_sp[[x]][i, 1] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_allbiased[[i]])
    cor_po_allbiased_sp[[x]][i, 2] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_allbiased_gp[[i]])
    cor_po_allbiased_sp[[x]][i, 3] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_allbiased_nocp[[i]])
    cor_po_allbiased_sp[[x]][i, 4] <- compute_cor_po(prob_pres_sp[[i]], pred_po_modGS_allbiased_nogp[[i]])
  }
}

# Evaluating metrics ************************************************************
# adjusting data
cor_unbiased <- bind_rows(cor_unbiased, .id="column_label") %>% select(-column_label) %>%
  rename("All Data, All Smooths"=E, "Group + Species Data, All Smooths"=GS, "Species-Only Data, All Smooths"=S,
         "Species-Only Smooths"=NoG)
cor_allbiased <- bind_rows(cor_allbiased, .id="column_label") %>% select(-column_label) %>%
  rename("All Data, All Smooths"=E, "Group + Species Data, All Smooths"=GS, "Species-Only Data, All Smooths"=S,
         "Species-Only Smooths"=NoG)
cor_po_unbiased <- bind_rows(cor_po_unbiased, .id="column_label") %>% select(-column_label) %>%
  rename("All Data, All Smooths"=E, "Group + Species Data, All Smooths"=GS, "Species-Only Data, All Smooths"=S,
         "Species-Only Smooths"=NoG)
cor_po_allbiased <- bind_rows(cor_po_allbiased, .id="column_label") %>% select(-column_label) %>%
  rename("All Data, All Smooths"=E, "Group + Species Data, All Smooths"=GS, "Species-Only Data, All Smooths"=S, 
         "Species-Only Smooths"=NoG)

# saving data
write.csv(cor_unbiased,
          file = "data/tabular/hgam_multiple_layers/cor_unbiased.csv",
          row.names = FALSE)
write.csv(cor_allbiased,
          file = "data/tabular/hgam_multiple_layers/cor_allbiased.csv",
          row.names = FALSE)
write.csv(cor_po_unbiased,
          file = "data/tabular/hgam_multiple_layers/cor_po_unbiased.csv",
          row.names = FALSE)
write.csv(cor_po_allbiased,
          file = "data/tabular/hgam_multiple_layers/cor_po_allbiased.csv",
          row.names = FALSE)


# adjusting data
cor_unbiased_sp <- bind_rows(cor_unbiased_sp, .id="column_label") %>% select(-column_label) %>%
  rename("All Data, All Smooths"=E, "Group + Species Data, All Smooths"=GS, "Species-Only Data, All Smooths"=S,
         "Species-Only Smooths"=NoG)
cor_allbiased_sp <- bind_rows(cor_allbiased_sp, .id="column_label") %>% select(-column_label) %>%
  rename("All Data, All Smooths"=E, "Group + Species Data, All Smooths"=GS, "Species-Only Data, All Smooths"=S,
         "Species-Only Smooths"=NoG)
cor_po_unbiased_sp <- bind_rows(cor_po_unbiased_sp, .id="column_label") %>% select(-column_label) %>%
  rename("All Data, All Smooths"=E, "Group + Species Data, All Smooths"=GS, "Species-Only Data, All Smooths"=S,
         "Species-Only Smooths"=NoG)
cor_po_allbiased_sp <- bind_rows(cor_po_allbiased_sp, .id="column_label") %>% select(-column_label) %>%
  rename("All Data, All Smooths"=E, "Group + Species Data, All Smooths"=GS, "Species-Only Data, All Smooths"=S, 
         "Species-Only Smooths"=NoG)

# saving data
write.csv(cor_unbiased_sp,
          file = "data/tabular/hgam_multiple_layers/cor_unbiased_sp.csv",
          row.names = FALSE)
write.csv(cor_allbiased_sp,
          file = "data/tabular/hgam_multiple_layers/cor_allbiased_sp.csv",
          row.names = FALSE)
write.csv(cor_po_unbiased_sp,
          file = "data/tabular/hgam_multiple_layers/cor_po_unbiased_sp.csv",
          row.names = FALSE)
write.csv(cor_po_allbiased_sp,
          file = "data/tabular/hgam_multiple_layers/cor_po_allbiased_sp.csv",
          row.names = FALSE)


# pearson and spearman metrics #################################################
# can run from here if already saved
cor_unbiased <- read_csv("data/tabular/hgam_multiple_layers/cor_unbiased.csv")
cor_allbiased <- read_csv("data/tabular/hgam_multiple_layers/cor_allbiased.csv")
cor_po_unbiased <- read_csv("data/tabular/hgam_multiple_layers/cor_po_unbiased.csv")
cor_po_allbiased <- read_csv("data/tabular/hgam_multiple_layers/cor_po_allbiased.csv")

# compute averages
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

id_map <- c(
  "1" = "All Data, All Smooths",
  "2" = "Group + Species Data, All Smooths",
  "3" = "Species-Only Data, All Smooths",
  "4" = "Species-Only Smooths"
)

# calculate overall how many of each model was the winner
max_col_unbiased <- factor(max.col(cor_unbiased), levels=names(id_map))
max_unbiased <- table(max_col_unbiased)
names(max_unbiased) <- id_map[names(max_unbiased)]
max_unbiased

max_col_allbiased <- factor(max.col(cor_allbiased), levels=names(id_map))
max_allbiased <- table(max_col_allbiased)
names(max_allbiased) <- id_map[names(max_allbiased)]
max_allbiased

max_col_po_unbiased <- factor(max.col(cor_po_unbiased), levels=names(id_map))
max_po_unbiased <- table(max_col_po_unbiased)
names(max_po_unbiased) <- id_map[names(max_po_unbiased)]
max_po_unbiased

max_col_po_allbiased <- factor(max.col(cor_po_allbiased), levels=names(id_map))
max_po_allbiased <- table(max_col_po_allbiased)
names(max_po_allbiased) <- id_map[names(max_po_allbiased)]
max_po_allbiased

library(ggplot2)

# converting them into format for analyzing
df1 <- make_long(cor_unbiased, "PA Unbiased")
df2 <- make_long(cor_allbiased, "PA Biased")
df3 <- make_long(cor_po_unbiased, "PO Unbiased")
df4 <- make_long(cor_po_allbiased, "PO Biased")

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

summary_df$Model <- factor(summary_df$Model, levels=c("All Data, All Smooths", "Group + Species Data, All Smooths", "Species-Only Data, All Smooths", "Species-Only Smooths"))

# creating a heatplot of means of correlations
ggplot(summary_df, aes(x = Model, y = Type, fill = Mean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label), color = "black", size = 4.2, lineheight = 0.9) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_x_discrete(labels = c(
    E = "All Data, All Smooths",
    GS = "Group + Species Data, All Smooths",
    S = "Species-Only Data, All Smooths",
    NoG = "Species-Only Smooths"
  )) +
  labs(
    title = "Model Performance by Type - Pearson/Spearman",
    fill = "Mean",
    x = "Model Type",
    y = "Data Type"
  ) +
  labs(
    title = "Model Performance by Type - Pearson/Spearman",
    fill = "Mean Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )

# performance variable for how many times each model does best
perf <- rbind(
  'PA Unbiased' = max_unbiased,
  'PA Biased' = max_allbiased,
  'PO Unbiased' = max_po_unbiased,
  'PO Biased' = max_po_allbiased
)
perf
write.csv(
  perf,
  file = "data/tabular/hgam_multiple_layers/best_model_performance.csv",
  row.names = FALSE
)

col_names <- c("All Data, All Smooths", "Group + Species Data, All Smooths", "Species-Only Data, All Smooths", "Species-Only Smooths")

# converting to better format for analyzing
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

# can calculate the mean and plot the line on the plot if you want
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

# plotting all correlation values
ggplot(cor_unbiased_long, aes(x = Row, y = Value, color = Model)) +
  geom_point(size = 2) +
  # add this code if you want to see the mean on the plots as well
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")
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
  # add this code if you want to see the mean on the plots as well
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")
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
  # add this code if you want to see the mean on the plots as well
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")
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
  # add this code if you want to see the mean on the plots as well
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")
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


# plotting boxplots for variations of correlations
ggplot(cor_unbiased_long, aes(x=Model, y=Value, color=Model)) +
  geom_boxplot(fill=NA, size=0.5) +
  scale_color_manual(values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")) +
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
  scale_color_manual(values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")) +
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
  scale_color_manual(values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")) +
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
  scale_color_manual(values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")) +
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

colnames(perf) <- c("E", "GS", "S", "NoG")


# getting a table of best-performing models
table_max <- data.frame(perf) %>%
  mutate(type = rownames(.)) %>%
  pivot_longer(cols = c(E, GS, S, NoG), names_to = "Model", values_to = "Max") %>%
  mutate(Model=recode(Model,
                      E = "All Data, All Smooths",
                      GS = "Group + Species Data, All Smooths",
                      S = "Species-Only Data, All Smooths",
                      NoG = "Species-Only Smooths"
  ))


table_max$Model <- factor(table_max$Model, levels = col_names)

# plotting stacked bar plot
ggplot(table_max, aes(x=type,fill=Model)) + 
  geom_bar(stat="identity",position="fill", aes(y=Max)) +
  labs(
    title = "Proportion of Best-Performing Models By Data Type - Pearson/Spearman",
    x="Data Type",
    y="Proportion",
    fill="Model"
  ) +
  theme_minimal() +
  theme(
    plot.title=element_text(hjust=0.5)
  )


# spearman correlation metrics ##################################################
# can run from here if already saved
cor_unbiased <- read_csv("data/tabular/hgam_multiple_layers/cor_unbiased_sp.csv")
cor_allbiased <- read_csv("data/tabular/hgam_multiple_layers/cor_allbiased_sp.csv")
cor_po_unbiased <- read_csv("data/tabular/hgam_multiple_layers/cor_po_unbiased_sp.csv")
cor_po_allbiased <- read_csv("data/tabular/hgam_multiple_layers/cor_po_allbiased_sp.csv")

# compute averages
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
max_col_unbiased <- factor(max.col(cor_unbiased), levels=names(id_map))
max_unbiased <- table(max_col_unbiased)
names(max_unbiased) <- id_map[names(max_unbiased)]
max_unbiased

max_col_allbiased <- factor(max.col(cor_allbiased), levels=names(id_map))
max_allbiased <- table(max_col_allbiased)
names(max_allbiased) <- id_map[names(max_allbiased)]
max_allbiased

max_col_po_unbiased <- factor(max.col(cor_po_unbiased), levels=names(id_map))
max_po_unbiased <- table(max_col_po_unbiased)
names(max_po_unbiased) <- id_map[names(max_po_unbiased)]
max_po_unbiased

max_col_po_allbiased <- factor(max.col(cor_po_allbiased), levels=names(id_map))
max_po_allbiased <- table(max_col_po_allbiased)
names(max_po_allbiased) <- id_map[names(max_po_allbiased)]
max_po_allbiased

library(ggplot2)

# converting them into format for analyzing
df1 <- make_long(cor_unbiased, "PA Unbiased")
df2 <- make_long(cor_allbiased, "PA Biased")
df3 <- make_long(cor_po_unbiased, "PO Unbiased")
df4 <- make_long(cor_po_allbiased, "PO Biased")

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

summary_df$Model <- factor(summary_df$Model, levels=c("All Data, All Smooths", "Group + Species Data, All Smooths", "Species-Only Data, All Smooths", "Species-Only Smooths"))

# creating a heatplot of means of correlations
ggplot(summary_df, aes(x = Model, y = Type, fill = Mean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label), color = "black", size = 4.2, lineheight = 0.9) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_x_discrete(labels = c(
    E = "All Data, All Smooths",
    GS = "Group + Species Data, All Smooths",
    S = "Species-Only Data, All Smooths",
    NoG = "Species-Only Smooths"
  )) +
  labs(
    title = "Model Performance by Type - Spearman",
    fill = "Mean",
    x = "Model Type",
    y = "Data Type"
  ) +
  labs(
    title = "Model Performance by Type - Spearman",
    fill = "Mean Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )

# performance variable for how many times each model does best
perf <- rbind(
  'PA Unbiased' = max_unbiased,
  'PA Biased' = max_allbiased,
  'PO Unbiased' = max_po_unbiased,
  'PO Biased' = max_po_allbiased
)
perf
write.csv(
  perf,
  file = "data/tabular/hgam_multiple_layers/best_model_performance_sp.csv",
  row.names = FALSE
)

col_names <- c("All Data, All Smooths", "Group + Species Data, All Smooths", "Species-Only Data, All Smooths", "Species-Only Smooths")

# converting to better format for analyzing
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

# can calculate the mean and plot the line on the plot if you want
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

# plotting all correlation values
ggplot(cor_unbiased_long, aes(x = Row, y = Value, color = Model)) +
  geom_point(size = 2) +
  # add this code if you want to see the mean on the plots as well
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")
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
  # add this code if you want to see the mean on the plots as well
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")
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
  # add this code if you want to see the mean on the plots as well
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")
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
  # add this code if you want to see the mean on the plots as well
  # geom_hline(data=model_means, aes(yintercept=mean_val, color=Model),
  #           linetype="dashed", size=1) +
  scale_color_manual(
    values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
               "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")
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


# plotting boxplots for variations of correlations
ggplot(cor_unbiased_long, aes(x=Model, y=Value, color=Model)) +
  geom_boxplot(fill=NA, size=0.5) +
  scale_color_manual(values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")) +
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
  scale_color_manual(values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")) +
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
  scale_color_manual(values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")) +
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
  scale_color_manual(values = c("All Data, All Smooths" = "black", "Group + Species Data, All Smooths" = "red", 
                                "Species-Only Data, All Smooths" = "blue", "Species-Only Smooths" = "green")) +
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

colnames(perf) <- c("E", "GS", "S", "NoG")


# getting a table of best-performing models
table_max <- data.frame(perf) %>%
  mutate(type = rownames(.)) %>%
  pivot_longer(cols = c(E, GS, S, NoG), names_to = "Model", values_to = "Max") %>%
  mutate(Model=recode(Model,
                      E = "All Data, All Smooths",
                      GS = "Group + Species Data, All Smooths",
                      S = "Species-Only Data, All Smooths",
                      NoG = "Species-Only Smooths"
  ))


table_max$Model <- factor(table_max$Model, levels = col_names)

# plotting stacked bar plot
ggplot(table_max, aes(x=type,fill=Model)) + 
  geom_bar(stat="identity",position="fill", aes(y=Max)) +
  labs(
    title = "Proportion of Best-Performing Models By Data Type - Spearman",
    x="Data Type",
    y="Proportion",
    fill="Model"
  ) +
  theme_minimal() +
  theme(
    plot.title=element_text(hjust=0.5)
  )

