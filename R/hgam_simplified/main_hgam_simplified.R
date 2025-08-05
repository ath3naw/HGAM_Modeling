################################################################################
# Master document for scripts and explanation of code
# Description: this script explains and runs the full pipeline for 1 simulation
# of prepping species distributions, simulating data, and modeling data, as well
# as different experiments and results for the data
################################################################################

# Each of the scripts calls
# source("R/hgam_simplified/functions.R") # functions that simulations use


# Running 1 simulation *********************************************************

# Step 1: Preparing species distributions and saving raster data ###############
source("R/hgam_simplified/prepare_raster_data_hgam.R")

# Step 2: Simulating data and putting it into modeling format
source("R/hgam_simplified/simulate_data_hgam.R") ###############################

# Step 3: Choosing the different experiment(s) for modeling ####################

# - General: examining unbiased vs biased predictions vs truth
# plots the true probability of presence and predicted probabilities of presence
# from the unbiased, travel-biased, and species-biased data next to each other
# for comparison
source("R/hgam_simplified/model_data_hgam.R")

# - Comparing Data of Varying Quality, Unbiased: -------------------------------
# creates hgam models of unbiased presence-absence data with 5/6 complex, 
# 2/3 complex, 1/3 complex, and 1/6 complex and compares predictions next to 
# each other and the truth. Uses data with 300 sampling locations.
source("R/hgam_simplified/model_data_varying_quality_hgam.R")

# - Comparing Data With Varying Numbers, Unbiased: -----------------------------
# creates hgam models of unbiased presence-absence data with 900 sampling
# locations, 300 sampling locations, and 100 sampling locations and compares
# predictions next to each other and the truth. Uses 2/3 complex data.
source("R/hgam_simplified/model_data_varying_numbers_hgam.R")

# - Comparing HGAM With Group Data and HGAM Without Group Data:
# creates hgam models of data including group data and without group data for
# measuring performance of a misspecified group model.
# Compares models using unbiased presence-absence, biased presence-absence,
# unbiased presence-only, and biased presence-only data, as well as performance
# across varying data quality with 300 sampling locations.
source("R/hgam_simplified/comparing_complex_nocomplex_data_hgam.R")

################################################################################

# OR, to run n simulations (a batch of simulations for more consistent results)
# comparing performance of hgam with group data, hgam without group data, and
# gam without group data for unbiased presence-absence, biased presence-absence,
# unbiased presence-only, and biased presence-only data.
# Uses 2/3 complex with 300 sampling locations.
source("R/hgam_simplified/simulate_batches_hgam.R")


