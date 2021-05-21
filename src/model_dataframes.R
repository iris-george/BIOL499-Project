########## BIOL 499 PROJECT MODEL DATAFRAME CREATION ##########
########## 
##########
# This file combines fish observation data from three survey types (SVC, 
# transect, and roving) with associated survey metadata, habitat traits, and 
# species traits before calculating density and average depth across 
# observations and log transforming density for further analyses.
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-05-21
##########
##########

# Set-Up =======================================================================

# packages
library(here)
library(plyr)
library(tidyverse)

# data
fish_data <- read_csv(here("./dataframes/fish_dataframe.csv"))
SVC <- read_csv(here("./clean_data/SVC_data.csv"))
prey_meta <- read_csv(here("./clean_data/prey_metadata.csv"))
pred_meta <- read_csv(here("./clean_data/pred_metadata.csv"))
traits <- read_csv(here("./clean_data/fish_traits.csv"))
vert_relief <- read_csv(here("./clean_data/vertical_relief.csv"))


#### Joining Survey Metadata ===================================================

# The following joins metadata associated with each survey type (SVC, transect 
# and roving) to the full fish dataframe. 


# Joining SVC Metadata: 

# select wanted metadata columns: session, site, date, diver, habitat, 
# cylinder_area, max_depth, octocoral, stony coral
SVC_meta <- SVC[,c(1,3,4,12,16,17,33,34)] 

# aggregate rows by session
SVC_meta <- SVC_meta %>% group_by(session, site, SVC_date, SVC_habitat) %>% 
  summarise_each(funs(mean))

# rename area column
SVC_meta <- SVC_meta %>% rename(SVC_area = SVC_cylinder_area) 

# join meta data to fish data
SVC_full <- join(fish_data, SVC_meta, by = NULL, type = "left", match = "first")


# Joining Transect Metadata:

# select wanted metadata columns: session, site, date, transect_area, depth
prey_meta <- prey_meta[,c(1,4,5,7,15)] 

# rename columns
prey_meta <- prey_meta %>% rename(prey_depth = prey_depth_m) 
prey_meta <- prey_meta %>% rename(prey_area = prey_tran_area) 

# want to aggregate depth by mean and area by sum: splitting up

# transform depth and area columns from character to numeric
prey_meta <- transform(prey_meta, prey_depth = as.numeric(prey_depth), 
                       prey_area = as.numeric(prey_area)) 

# remove area column from full transect meta
prey_depth <- prey_meta[,c(1:4)] 

# aggregate depth rows by session
prey_depth <- aggregate(.~session+site+prey_date, prey_depth, mean) 

# remove depth column from full transect meta
prey_area <- prey_meta[,c(1:3,5)]

# aggregate area rows by session
prey_area <- aggregate(.~session+site+prey_date, prey_area, sum) 

# join transect depth and area to make full transect metadata 
prey_meta <- join(prey_depth, prey_area, by = NULL, type = "full", 
                  match = "all") 

# join transect metadata to fish and SVC dataframe
SVCprey_full <- join(SVC_full, prey_meta, by = NULL, type = "left", 
                     match = "first") 


# Joining Roving Metadata:

# select wanted metadata columns: session, site, date, transect_area, depth
pred_meta <- pred_meta[,c(1,4,8,17,21)] 

# rename columns
pred_meta <- pred_meta %>% rename(pred_depth = pred_depth_ft)
pred_meta <- pred_meta %>% rename(pred_area = pred_trans_area) 

# want to aggregate depth by mean and area by sum: splitting up

# remove area column
pred_depth <- pred_meta[,c(1:4)] 

# aggregate depth rows by session
pred_depth <- aggregate(.~session+site+pred_date, pred_depth, mean) 

# remove depth column
pred_area <- pred_meta[,c(1:3,5)] 

# aggregate area rows by session
pred_area <- aggregate(.~session+site+pred_date, pred_area, sum) 

# join roving depth and area to create full roving metadata
pred_meta <- join(pred_depth, pred_area, by = NULL, type = "full", 
                  match = "all") 

# join roving metadata to fish, SVC, and transect dataframe
fish_meta <- join(SVCprey_full, pred_meta, by = NULL, type = "left", 
                  match = "first") 

# Joining Vertical Relief Data =================================================

# The following section aggregates vertical relief measures to a mean value per
# survey site, then joins these measures to the fish and survey metadata 
# dataframe. 

# select site and vert relief columns
vert_relief <- vert_relief[,c(1,7)] 

# aggregate to site mean
vert_relief <- aggregate(relief_cm~site, vert_relief, mean) 

# join vertical relief measure to each site
fish_meta <- join(fish_meta, vert_relief, by = NULL, type = "left", 
                  match = "first") 

# Joining Species' Trait Data ==================================================

# The following joins adult fish traits used in further analyses to the survey
# metadata and fish dataframe.

# filter for adult lifestage
fish_traits <- filter(traits, lifestage == "adult") 

# select relevant columns: latin names, predator presence, nocturnal, 
# max_length, position, behaviour, colouration, and cryptic_behaviour
fish_traits <- fish_traits[,c(1:4,7,18,36,38,39,58,60)] 

# rename columns
fish_traits <- fish_traits %>% rename(colouration = colouration_cat1)
fish_traits <- fish_traits %>% rename(species = common_name)

# join fish trait data to meta and fish dataframe
full_dataframe <- join(fish_meta, fish_traits, by = NULL, type = "full", match = "all")

# remove NA values
full_dataframe <- na.omit(full_dataframe) 


#### Calculate Density ####

# Density Calculations
full_dataframe$SVC_density <- full_dataframe$SVC_abundance/full_dataframe$SVC_area # SVC density calculation
full_dataframe$prey_density <- full_dataframe$prey_abundance/full_dataframe$prey_area # prey density calculation
full_dataframe$pred_density <- full_dataframe$pred_abundance/full_dataframe$pred_area # predator density calcualtion 

# Density Differences
full_dataframe$SVC_prey_difference <- full_dataframe$SVC_density - full_dataframe$prey_density
full_dataframe$SVC_pred_difference <- full_dataframe$SVC_density - full_dataframe$pred_density 
full_dataframe$prey_pred_difference <- full_dataframe$prey_density - full_dataframe$pred_density 


#### Dataframe Edits ####

# Re-Order Columns
full_dataframe <- full_dataframe[,c(2,1,9,12:13,20,21:23,3,4,24:30,8,11,10,5,31,14:16,6,32,17:19,7,33,34:36)]
full_dataframe <- full_dataframe %>% rename(habitat = SVC_habitat) 

# Re-Name Columns
full_dataframe <- full_dataframe %>% rename(family = Family)
full_dataframe <- full_dataframe %>% rename(binomial = Binomial)
full_dataframe <- full_dataframe %>% rename(species_order = Order)


#### SVC vs. Prey Dataframe ####
SVCprey <- full_dataframe[,c(1:28,34)] # select SVC and prey columns
SVCprey$total_density <- SVCprey$SVC_density+SVCprey$prey_density
SVCprey_data <- SVCprey[SVCprey$total_density !=0,] # removing rows where SVC and prey densities = 0 (indicates that no species of that size group were observed on said session in either survey)


#### SVC vs. Predator Dataframe ####

SVCpred <- full_dataframe[,c(1:23,29:33,35)] # select SVC and predator columns
SVCpred <- filter(SVCpred, predator_presence == 1) # filter for species recorded on predator surveys
SVCpred$total_density <- SVCpred$SVC_density+SVCpred$pred_density
SVCpred_data <- SVCpred[SVCpred$total_density !=0,] # removing rows where SVC and predator densities = 0 (indicates that no species of that size group were observed on said session in either survey)


#### Density Log Transformation ####

# density differences did not meet normality assumptions --> a log transformation of raw densities before taking the difference improved normality

# SVC vs. Prey:
log_SVCdensity <- log(SVCprey_data$SVC_density + 0.001) 
log_preydensity <- log(SVCprey_data$prey_density + 0.001) 
hist(log_SVCdensity-log_preydensity)
SVCprey_data$log_difference <- log_SVCdensity-log_preydensity

# SVC vs. Predator:
log_SVCdensity2 <- log(SVCpred_data$SVC_density + 0.001)
log_preddensity <- log(SVCpred_data$pred_density + 0.001)
hist(log_SVCdensity2-log_preddensity)
SVCpred_data$log_difference <- log_SVCdensity2-log_preddensity


#### Average Depth Calculation ####

# SVC vs. Prey:
SVCprey_data$average_depth <- (SVCprey_data$SVC_max_depth + SVCprey_data$prey_depth)/2

# SVC vs. Predator:
SVCpred_data$average_depth <- (SVCpred_data$SVC_max_depth + SVCpred_data$pred_depth)/2
write.csv(SVCpred_data, "SVCpred_dataframe.csv")
