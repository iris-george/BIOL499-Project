########## BIOL 499 PROJECT MAIN FILE ##########
########## 
##########
# This main file calls and runs all subsequent R files in this analysis.  
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-05-21
##########
##########


# Set-Up =======================================================================
library(here)


# Fish Dataframe Creation ======================================================

# This script reads in cleaned survey data from SVC, transect, and roving 
# surveys and creates a constant format between the three types before joining
# them into a single dataframe with all fish observations, grouped by species
# and size bin for each session. 

source(here("./src/fish_dataframes.R"))


# Model Dataframes Creation ====================================================

# This script joins survey metadata, habitat trait, and species' trait values 
# onto the cleaned fish dataframe before splitting it into the two dataframes
# used in further analyses. One dataframe examines fish observations within 
# SVC and transect surveys, while the other explores observations in SVC and 
# roving surveys. The script additionally calculates log-transformed density 
# differences between the survey types for each fish observation as well as 
# average depths between the surveys. 

source(here("./src/model_dataframes.R"))


# Trait Analyses ===============================================================

# This script runs analyses on the dataframes comparing log density differences 
# between survey types. An ANOVA is run to explore the effects of species' order
# on density differences before running linear mixed effects models which 
# incorporate all habitat, trait, and survey characteristics present in the 
# dataframes. Included are tests of predictor collinearity and model fit. 

source(here("./src/trait_analyses.R"))


# Covariate Boxplots ===========================================================

# This script creates and saves boxplots of significant predictors from the 
# linear mixed effects models that are of particular interest for interpretation 
# in the thesis.

source(here("./src/covariate_boxplots.R"))


# Density Barplots =============================================================

# This script creates and saves barplots of the density differences between 
# survey types averaged across families (for SVC compared to transect surveys) 
# and species (for SVC compared to roving surveys). 

source(here("./src/density_barplots.R"))


# Lionfish Analyses ============================================================

# This script runs additional analyses on lionfish observations across the three
# survey types. It utilizes an ANOVA to compare the differences in average 
# lionfish density across the survey types with and without high outliers
# present in the SVC data and utilizes boxplots to visualize the results.

source(here("./src/lionfish_analyses.R"))