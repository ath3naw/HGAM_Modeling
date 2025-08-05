################################################################################
# Master document for scripts and explanation of code
# Description: this script explains and runs the full pipeline for cleaning
# leucosphyrus data and modeling it on a Southeast Asia map
################################################################################

# Each of the scripts calls
# source("R/leucosphyrus_model/leuco_functions.R") # functions that simulations use

# Download bioclim data for '/leuco_prepare_raster_data.R' with 
# https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_bio.zip

# Step 1: Preparing covariate and raster maps ##################################
source("R/leucosphyrus_model/leuco_prepare_raster_data.R")

# Step 2: Cleaning MalariaAtlas Data (optional) ################################
# converting po data from MalariaAtlas library to model data format
source("R/leucosphyrus_model/leuco_data_cleaning_po.R")

# Step 3: Checking and cleaning data ###########################################
# checking to make sure data are in corresponding countries + converting to
# model data format
source("R/leucosphyrus_model/leuco_checking_data.R")

# Step 4: Modeling #############################################################
# using data to fit models and plot predictions
source("R/leucosphyrus_model/leuco_data_modeling")
