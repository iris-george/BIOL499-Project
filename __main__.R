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