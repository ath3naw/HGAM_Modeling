################################################################################
# Master document for scripts and explanation of code
# Description: this script explains and runs the full pipeline for 1 simulation
# of prepping species distributions, simulating data, and modeling data, as well
# as different experiments and results for the data
################################################################################

# Each of the scripts calls
# source("R/hgam_multiple_layers/functions_multiple.R") # functions that simulations use


# Running 1 simulation *********************************************************

# Step 1: Preparing species distributions and saving raster data ###############
source("R/hgam_multiple_layers/prepare_raster_data_mult_layers_hgam.R")

# Step 2: Simulating data and putting it into modeling format
source("R/hgam_multiple_layers/simulate_data_mult_layers_hgam.R") ##############

# Step 3: Choosing the different experiment(s) for modeling ####################

# - General: examining unbiased vs biased predictions vs truth
# plots the true probability of presence and predicted probabilities of presence
# from the unbiased and biased data (bias=travel_time^2*rarest species) next to
# each other for comparison
source("R/hgam_multiple_layers/model_data_mult_layers_hgam.R")

# - Comparing HGAM With Complex and Group Data and HGAM Without Complex or Group Data:
# creates hgam models of data including group and complex data and without group
# data or complex data for measuring performance of misspecified group models.
# Compares models using unbiased presence-absence, biased presence-absence,
# unbiased presence-only, and biased presence-only data for 2/3 complex data 
# with 300 sampling locations.
source("R/hgam_multiple_layers/comparing_complex_nocomplex_mult_layers_hgam.R")

################################################################################

# OR, to run n simulations (a batch of simulations for more consistent results)
# comparing performance of hgam with group, complex, and species data, hgam with
# group and species data, hgam with only species data, and gam with only species
# data for unbiased presence-absence, biased presence-absence,
# unbiased presence-only, and biased presence-only data.
# Uses 2/3 complex with 300 sampling locations.
source("R/hgam_multiple_layers/simulate_batches_and_mult_layers_hgam.R")


