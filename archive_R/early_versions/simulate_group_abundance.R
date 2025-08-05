# ****************************** TESTING CODE ***********************************
# Early version
rm(list=ls())
library(tidyverse)
library(terra)
library(geodata)
library(gratia)
library(dplyr)
source("R/hgam_simplified/functions.R")

# read in variables
par(mfrow = c(1,1))
covs <- terra::rast("data/grids/covariates.tif")
mad_mask <- terra::rast("data/grids/mad_mask.tif")
bc_mad <- terra::rast("data/grids/bc_mad.tif")
bias <- terra::rast("data/grids/bias.tif")

# establishing number of species + max catch size
n_sp <- 10
max_catch_size <- 10  # was 10

# group level model
#ttemp was 0.02, ttemp2=-0.05, ttemp test was ttemp=0, ttemp2 = -0.2/0.3, tiso=0.001
beta_group <- c(ttemp = 0.02, ttemp2 = -0.1, tiso = 0.001, tseas = -0.001, 
                twet = 0.08, twet2 = -0.02, pprec = 0.001, pprec2 = -0.000001,#
                #pseas = 0.04, pseas2 = -0.0007, pwarm = 0.0002, pwarm2 = -0.000001, 
                int=0.2) # int was 0.03

cons_group <- c(ttemp = 22, twet = 24, pprec = 1400)#, pseas = 80, pwarm=1000)

# species-level deviations
species_data <- data.frame(
  species = factor(paste("sp", 1:n_sp, sep="")),
  ttemp = rnorm(n_sp, 0, 0.008), # was 0.008
  ttemp2 = rnorm(n_sp, 0, 0.15), # was 0.015
  tiso = rnorm(n_sp, 0, 0.003),# was 0.003
  tseas = rnorm(n_sp, 0, 0.0025),
  twet = rnorm(n_sp, 0, 0.03),
  twet2 = rnorm(n_sp, 0, 0.02),
  pprec = rnorm(n_sp, 0, 0.0008),
  pprec2 = rnorm(n_sp, 0, 0.0000007),
  #pseas = rnorm(n_sp, 0, 0.02),
  #pseas2 = rnorm(n_sp, 0, 0.0005),
  #pwarm = rnorm(n_sp, 0, 0.0006),
  #pwarm2 = rnorm(n_sp, 0, 0.0000008),
  int = rnorm(n_sp, 0, 0.4) # was 0.02
)

group_abund <- exp(beta_group["int"]+beta_group["ttemp"]*covs$ttemp+beta_group["ttemp2"]*(covs$ttemp-cons_group["ttemp"])^2+
                     beta_group["tiso"]*covs$tiso+beta_group["tseas"]*covs$tseas+beta_group["twet"]*covs$twet+
                     beta_group["twet2"]*(covs$twet-cons_group["twet"])^2+
                     beta_group["pprec"]*covs$pprec+beta_group["pprec2"]*(covs$pprec-cons_group["pprec"])^2)#+
                     #beta_group["pseas"]*covs$pseas+beta_group["pseas2"]*(covs$pseas-cons_group["pseas"])^2+
                     #beta_group["pwarm"]*covs$pwarm+beta_group["pwarm2"]*(covs$pwarm-cons_group["pwarm"])^2)
plot(group_abund)
rel_group_abund <- rescale_abundance(group_abund)
names(rel_group_abund) <- "relative_abundance"
plot(rel_group_abund, main="Relative Abundance")
# calculating probability of presence
group_prob_pres <- probability_of_presence(rel_group_abund,
                                           max_catch_size)
plot(group_prob_pres, main="Prob of Presence")
writeRaster(group_prob_pres,
            "data/grids/group_prob_pres_hgam.tif",
            overwrite = TRUE)

# calculating species abundance
species_abund <- exp(beta_group["int"]+species_data$int+(beta_group["ttemp"]+species_data$ttemp)*covs$ttemp+
                       (beta_group["ttemp2"]+species_data$ttemp2)*(covs$ttemp-cons_group["ttemp"])^2+
                       (beta_group["tiso"]+species_data$tiso)*covs$tiso+(beta_group["tseas"]+species_data$tseas)*covs$tseas+
                       (beta_group["twet"]+species_data$twet)*covs$twet+(beta_group["twet2"]+species_data$twet2)*(covs$twet-cons_group["twet"])^2+
  (beta_group["pprec"]+species_data$pprec)*covs$pprec+(beta_group["pprec2"]+species_data$pprec2)*(covs$pprec-cons_group["pprec"])^2)#+
  #(beta_group["pseas"]+species_data$pseas)*covs$pseas+(beta_group["pseas2"]+species_data$pseas2)*(covs$pseas-cons_group["pseas"])^2+
  #(beta_group["pwarm"]+species_data$pwarm)*covs$pwarm+(beta_group["pwarm2"]+species_data$pwarm2)*(covs$pwarm-cons_group["pwarm"])^2)
plot(species_abund)

rel_species_abund <- rast(lapply(species_abund, rescale_abundance))

plot(rel_species_abund, main="Relative Abundance")
prob_pres <- probability_of_presence(rel_species_abund,
                                        max_catch_size)
# calculating probability of presence
plot(prob_pres, main="Prob of Presence")
writeRaster(prob_pres,
            "data/grids/spec_prob_pres_hgam.tif",
            overwrite = TRUE)

# calculating bounds + means for covariates for partial response plots
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

# finding max and min of abundance for scaling
min_value_group <- global(group_abund, "min", na.rm = TRUE)[1, 1]
max_value_group <- global(group_abund, "max", na.rm = TRUE)[1, 1]
min_value <- global(species_abund, "min", na.rm = TRUE)[, 1]
max_value <- global(species_abund, "max", na.rm = TRUE)[, 1]
minmax_group <- cbind(min_value_group, max_value_group)
minmax_spec <- cbind(min_value, max_value)

# calculating partial response plots, group
partial_group("ttemp", covs_means, covs_bounds, cons_group, minmax_group, max_catch_size)
partial_group("tiso", covs_means, covs_bounds, cons_group, minmax_group, max_catch_size)
partial_group("tseas", covs_means, covs_bounds, cons_group, minmax_group, max_catch_size)
partial_group("twet", covs_means, covs_bounds, cons_group, minmax_group, max_catch_size)
partial_group("pprec", covs_means, covs_bounds, cons_group, minmax_group, max_catch_size)
partial_group("pseas", covs_means, covs_bounds, cons_group, minmax_group, max_catch_size)
partial_group("pwarm", covs_means, covs_bounds, cons_group, minmax_group, max_catch_size)

# calculating partial response plots, species
partial_spec("ttemp", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, sp=1)
partial_spec("ttemp", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, sp=2)
partial_spec("ttemp", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, sp=3)
partial_spec("ttemp", covs_means, covs_bounds, cons_group, minmax_spec, max_catch_size, sp=4)


## Now for simulation of the data **********************************************
# have a medium amount of data, 300
n_samples = 300
sample_locations_random <- random_locations(mad_mask,
                                            n_samples,
                                            weighted = FALSE)
plot(mad_mask)
points(sample_locations_random)
pa_coords <- crds(sample_locations_random)

write.csv(pa_coords,
          file = "data/tabular/hgam_coords.csv",
          row.names = FALSE)


pa_df <- tibble(site_id = integer(0),
                      presence = integer(0),
                      species_id = integer(0))
# converting to good format for data frame of simulations
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

# checking presence and absence data
plot(prob_pres[[1]], range=c(0,1))
points(sample_locations_random, pch=21, bg=pa_tabular$a==1)
plot(prob_pres[[2]], range=c(0,1))
points(sample_locations_random, pch=21, bg=pa_tabular$b==1)

# number of locations converted to group
n_cp <- 200 # before was 100

# creating group coords and putting into data frame
pa_tab <- pa_tabular |> 
  mutate(complex = ifelse(rowSums(across(c(a, b, c, d, e, f, g, h, i, j))) > 0, 1, 0))
pa_tab_1 <- pa_tab[1:n_cp,] |>
  select(site_id, x, y, complex) |>
  rename(pa = complex) |>
  mutate(sp = "x")
pa_tab_2 <- pa_tab[(n_cp+1):n_samples,] |>
  select(-complex) |>
  pivot_longer(cols = c(a,b,c,d,e,f,g,h,i,j), names_to = "sp", values_to = "pa")
pa_long <- rbind(pa_tab_1, pa_tab_2) |>
mutate(not_complex = ifelse(sp == "x", 0, 1))
covs_vals <- extract(x=covs, y=pa_long |> select(x, y))

# combining it all into data frame for model
pa_model_data <- cbind(pa_long, covs_vals) |> select(-ID) |>
  mutate(sp = as.factor(sp))

# save these items
write.csv(pa_model_data,
          file = "data/tabular/hgam_pa_data.csv",
          row.names = FALSE)



