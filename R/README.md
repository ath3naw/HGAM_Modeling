################################################################################
# R Code for Hierarchical Generalized Additive Model Performance

## Overview
The files in this folder are the R scripts for the simulations of the HGAM
model. There are 3 folders:
1. 'hgam_simplified/' - all R scripts for a 2-layer HGAM model with 1 group
distribution and species distributions varying off of the group distribution. 
Default is 10 species, although can vary. Organized into stages of preparing
distribution rasters, simulating data, and modeling/predicting results.

2. 'hgam_multiple_layers/' - all R scripts for a 3-layer HGAM model with 1 group
distribution, some complex distributions, and some species distributions
varying off of each corresponding complex and group distribution. Default is
2 complexes with 5 and 7 species, respectively, although can vary. Organized
into stages of preparing distribution rasters, simulating data, and 
modeling/predicting results.

3. 'leucosphyrus_model/' - all R scripts for cleaning and modeling collected
leucosphyrus data using HGAM model

################################################################################
