rm(list=ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
source("R/hgam_multiple_layers/functions_multiple.R")

# load in variables and reset format
par(mfrow = c(1,1))
covs <- terra::rast("data/grids/covariates.tif")
twn_mask <- terra::rast("data/grids/mad_mask.tif")
bc_twn <- terra::rast("data/grids/bc_mad.tif")

# defining number of complex + species
n_cp <- 2
# 5 in 1st complex and 7 in 2nd complex
n_sp <- c(5,7)
n_sp <- data.frame(n_sp)
write.csv(n_sp,
          file = "data/tabular/hgam_multiple_layers/n_sp",
          row.names = FALSE)

# define max_catch_size of mosquitoes
max_catch_size <- 20

# group level model
# can add more covariates if needed
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

# calculating the group probability of presence
group_prob_pres <- probability_of_presence(rel_group_abund,
                                           max_catch_size)
plot(group_prob_pres, main="Prob of Presence")
writeRaster(group_prob_pres,
            "data/grids/hgam_multiple_layers/group_prob_pres_hgam.tif",
            overwrite = TRUE)

# complex distributions
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
plot(complex_abund)
rel_complex_abund <- rast(lapply(complex_abund, rescale_abundance))

plot(rel_complex_abund, main="Relative Abundance")
prob_pres_cp <- probability_of_presence(rel_complex_abund,
                                        max_catch_size)
plot(prob_pres_cp, main="Prob of Presence")
writeRaster(prob_pres_cp,
            "data/grids/hgam_multiple_layers/complex_prob_pres_hgam.tif",
            overwrite = TRUE)

# adding a table for which complex corresponds to the species
complex_match <- match(species_data$complex, complex_data$complex)
cpx <- complex_data[complex_match,]

# adding species probability of presence + abundance
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
plot(species_abund)

rel_species_abund <- rast(lapply(species_abund, rescale_abundance))

plot(rel_species_abund, main="Relative Abundance")
prob_pres <- probability_of_presence(rel_species_abund,
                                     max_catch_size)
plot(prob_pres, main="Prob of Presence")
writeRaster(prob_pres,
            "data/grids/hgam_multiple_layers/spec_prob_pres_hgam.tif",
            overwrite = TRUE)

# calculating covariate bounds and means for partial response plots
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

# calculating and min and max for group, complex, and species data -> overall min and max for scaling
min_value_group <- global(group_abund, "min", na.rm = TRUE)[1, 1]
max_value_group <- global(group_abund, "max", na.rm = TRUE)[1, 1]
min_value_cp <- global(complex_abund, "min", na.rm = TRUE)[, 1]
max_value_cp <- global(complex_abund, "max", na.rm = TRUE)[, 1]
min_value_sp <- global(species_abund, "min", na.rm = TRUE)[, 1]
max_value_sp <- global(species_abund, "max", na.rm = TRUE)[, 1]

# creating data frames for values
minmax_group <- cbind(min_value_group, max_value_group)
minmax_complex <- cbind(min_value_cp, max_value_cp)
minmax_spec <- cbind(min_value_sp, max_value_sp)

# calculating partial response plots, group
partial_group("ttemp", covs_means, covs_bounds, cons_group, minmax_group, max_catch_size)
partial_group("pprec", covs_means, covs_bounds, cons_group, minmax_group, max_catch_size)

# calculating partial response plots, complex
par(mfrow=c(2,2))
partial_complex("ttemp", covs_means, covs_bounds, cons_group, minmax_complex, max_catch_size, cp=1)
partial_complex("ttemp", covs_means, covs_bounds, cons_group, minmax_complex, max_catch_size, cp=2)

partial_complex("pprec", covs_means, covs_bounds, cons_group, minmax_complex, max_catch_size, cp=1)
partial_complex("pprec", covs_means, covs_bounds, cons_group, minmax_complex, max_catch_size, cp=2)

# calculating partial response plots, species
par(mfrow=c(2,3))
partial_spec("ttemp", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=1, sp=1)
partial_spec("ttemp", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=1, sp=2)
partial_spec("ttemp", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=1, sp=3)

partial_spec("pprec", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=1, sp=1)
partial_spec("pprec", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=1, sp=2)
partial_spec("pprec", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=1, sp=3)

partial_spec("ttemp", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=2, sp=1)
partial_spec("ttemp", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=2, sp=2)
partial_spec("ttemp", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=2, sp=3)

partial_spec("pprec", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=2, sp=1)
partial_spec("pprec", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=2, sp=2)
partial_spec("pprec", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, cp=2, sp=3)

