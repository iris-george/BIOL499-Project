########## BIOL 499 PROJECT FISH DATAFRAME CREATION ##########
########## 
##########
# This file compiles all individual fish observations from SVC, transect, and 
# roving surveys into a matching format with associated size bins and 
# abundances.
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-05-21
##########
##########

# Set-Up =======================================================================

# Packages: 
library(here)
library(plyr)
library(tidyverse)
library(data.table) 

# Data:
SVC <- read_csv(here("./clean_data/SVC_data.csv"))
prey_fish <- read_csv(here("./clean_data/prey_fish_data.csv"))
pred_fish <- read_csv(here("./clean_data/pred_fish_data.csv"))

# SVC Lengths ==================================================================

# The following data estimates individual fish lengths from the SVC data using a 
# triangle distribution. This accounts for the abundnace of a species observed, 
# the maximum length of the group, the minimum length of the group, and their 
# mean length. 

# calculate length of species with abundance = 1
c2 <- SVC %>%
  filter(SVC_abundance == 1) %>%
  mutate(
    L1 = NA,
    A1 = NA,
    L2 = NA,
    A2 = NA,
    L3 = SVC_mean_tl,
    A3 = SVC_abundance,
    L4 = NA,
    A4 = NA,
    L5 = NA,
    A5 = NA)

# calculate lengths of species with abundance = 2
c3 <-SVC %>%
  filter(SVC_abundance == 2) %>%
  mutate(
    L1 = SVC_min_tl,
    A1 = 1,
    L2 = NA,
    A2 = NA,
    L3 = NA,
    A3 = NA,
    L4 = NA,
    A4 = NA,
    L5 = SVC_max_tl,
    A5 = 1)

# calculate lengths of species with abundance = 3
c4 <- SVC %>%
  filter(SVC_abundance == 3) %>%
  mutate(
    L1 = SVC_min_tl,
    A1 = 1,
    L2 = NA,
    A2 = NA,
    L3 = SVC_mean_tl,
    A3 = 1,
    L4 = NA,
    A4 = NA,
    L5 = SVC_max_tl,
    A5 = 1)

# calculate lengths of species with abundance = 4
c5 <- SVC %>%
  filter(SVC_abundance == 4) %>%
  mutate(
    L1 = SVC_min_tl,
    A1 = 1,
    L2 = NA,
    A2 = NA,
    L3 = SVC_mean_tl,
    A3 = 2,
    L4 = NA,
    A4 = NA,
    L5 = SVC_max_tl,
    A5 = 1)

# calculate lengths of species with abundance >= 5
c6 <- SVC %>%
  filter(SVC_abundance >= 5 &
           SVC_abundance <= 99) %>%
  mutate(
    L1 = SVC_min_tl,
    A1 = 1,
    L3 = SVC_mean_tl,
    A3 = (.5 * SVC_abundance),
    L5 = SVC_max_tl,
    A5 = 1,
    L2 = (L3 + L1) / 2,
    A2 = (SVC_abundance - (A1 + A3 + A5)) / 2,
    L4 = (L3 + L5) / 2,
    A4 = A2)

# calculate lengths of species with abundance > 99
c7 <- SVC %>%
  filter(SVC_abundance > 99) %>%
  mutate(
    L1 = SVC_min_tl,
    A1 = (.01 * SVC_abundance),
    L3 = SVC_mean_tl,
    A3 = (.5 * SVC_abundance),
    L5 = SVC_max_tl,
    A5 = (.01 * SVC_abundance),
    L2 = (L3 + L1) / 2,
    A2 = (SVC_abundance - (A1 + A3 + A5)) / 2,
    L4 = (L3 + L5) / 2,
    A4 = A2)

# bind SVC lengths
SVC_lengths <- combine(list(c2, c3, c4, c5, c6, c7))

# export SVC length data
write_csv(SVC_lengths, here("./clean_data/SVC_lengths.csv"))


# SVC Dataframe ================================================================

# The following section transforms the SVC length data into a format matching 
# that of the transect and roving survey data. 

# select session, species, length, and abundance columns
SVC_fish <- SVC_lengths[,c(1, 37, 42:51)] 

# take from wide to long format, pairing length and abundance columns
SVC_melt <- melt(setDT(SVC_fish), measure = patterns("L", "A"), 
                 variable.name = "var", value.name = c("length", "abundance")) 

# remove NA values
SVC_melt <- na.omit(SVC_melt) 

# order by session
SVC_melt <- SVC_melt[order(session),] 

# rename columns
SVC_melt <- SVC_melt %>% rename(SVC_tl = length) 
SVC_melt <- SVC_melt %>% rename(SVC_abundance = abundance) 


# Size Bins ====================================================================

# The following groups every fish length into one of six size bins: bin 1 <= 5,
# 5 < bin 2 <= 10, 10 < bin 3 <= 15, 15 < bin 4 <= 20, 20 < bin 5 <= 30, 
# bin 6 > 30. 

# SVC size bin column
SVC_melt$size_bin = ifelse(SVC_melt$SVC_tl <= 5, 1, ifelse(SVC_melt$SVC_tl > 5 
    & SVC_melt$SVC_tl <= 10, 2, ifelse(SVC_melt$SVC_tl > 10 & 
    SVC_melt$SVC_tl <= 15, 3, ifelse(SVC_melt$SVC_tl > 15 & 
    SVC_melt$SVC_tl <= 20, 4, ifelse(SVC_melt$SVC_tl > 20 & 
    SVC_melt$SVC_tl <=30, 5, ifelse(SVC_melt$SVC_tl > 30, 6, NA)))))) 

# selected session, species, size_bin, and abundance columns from SVC
SVC_bins <- SVC_melt[,c(1,2,5,6)] 

# transect size bin column
prey_fish$size_bin = ifelse(prey_fish$prey_tl <= 5, 1, 
   ifelse(prey_fish$prey_tl > 5 & prey_fish$prey_tl <= 10, 2, 
   ifelse(prey_fish$prey_tl > 10 & prey_fish$prey_tl <= 15, 3, 
   ifelse(prey_fish$prey_tl > 15 & prey_fish$prey_tl <= 20, 4, 
   ifelse(prey_fish$prey_tl > 20 & prey_fish$prey_tl <=30, 5, 
   ifelse(prey_fish$prey_tl > 30, 6, NA))))))

# selecting session, species, and size_bin columns from transect
prey_bins <- prey_fish[,c(1,3,5)] 

# roving size bin column
pred_fish$size_bin = ifelse(pred_fish$pred_tl <= 5, 1, 
    ifelse(pred_fish$pred_tl > 5 & pred_fish$pred_tl <= 10, 2, 
    ifelse(pred_fish$pred_tl > 10 & pred_fish$pred_tl <= 15, 3, 
    ifelse(pred_fish$pred_tl > 15 & pred_fish$pred_tl <= 20, 4, 
    ifelse(pred_fish$pred_tl > 20 & pred_fish$pred_tl <=30, 5, 
    ifelse(pred_fish$pred_tl > 30, 6, NA))))))

# selecting session, species, and size_bin columns from roving
pred_bins <- pred_fish[,c(1,3,14)] 


# Adding Abundance =============================================================

# The following combines all matching species' size bins within every session
# and creates an additional column with the number of individuals in each size
# bin. 

# SVC abundance column 
SVC_abun <- SVC_bins[, lapply(.SD, sum), by=list(session, species, size_bin)] 

# transect abundance column
prey_abun <- prey_bins %>% group_by(session, species, size_bin) %>% tally()

# rename transect abundance column
prey_abun <- prey_abun %>% rename(prey_abundance = n)

# roving abundance column
pred_abun <- pred_bins %>% group_by(session, species, size_bin) %>% tally() 

# rename transect abundance column
pred_abun <- pred_abun %>% rename(pred_abundance = n) 


# Joining Survey Fish Dataframes ===============================================

# The following joins all three survey dataframes (SVC, transect, and roving) 
# with their size bin and abundance columns into a single dataframe with all 
# individual fish observations. 

# join transect and roving datasets
prey_pred <- full_join(prey_abun, pred_abun, by = NULL, copy = FALSE) 

# join SVC to transect and roving dataset
fish_data <- full_join(SVC_abun, prey_pred, by = NULL, copy = FALSE) 

# replace NAs with values of 0
fish_data[is.na(fish_data)] <- 0

# order by session
fish_data <- fish_data[order(session),]

# export fish dataframe
write_csv(fish_data, here("./dataframes/fish_dataframe.csv"))