########## BIOL 499 PROJECT DENSITY BARPLOTS ##########
########## 
##########
# This file creates barplots of the density differences between survey types. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-05-25
##########
##########


# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(doBy)
library(ggplot2)
library(here)

# data
prey_fish <- read_csv(here("./clean_data/prey_fish_data.csv"))
pred_fish <- read_csv(here("./clean_data/pred_fish_data.csv"))
prey_meta <- read_csv(here("./clean_data/prey_metadata.csv"))
pred_meta <- read_csv(here("./clean_data/pred_metadata.csv"))
SVC_lengths <- read_csv(here("./clean_data/SVC_lengths.csv"))
traits <- read_csv(here("./clean_data/fish_traits.csv"))


# Data Edits ===================================================================

# The following removes unwanted species.

# remove "sp." 
pred_edit <- pred_fish[pred_fish$species !="grouper sp.",]
pred_edit <- pred_fish[pred_fish$species !="soapfish sp.",]

# remove blank species
pred_edit <- pred_fish[pred_fish$species !="",]


# Aggregate Species ============================================================

# The following aggregates species by session and calculates their abundance.

# select transect species and session columns
prey_species <- prey_fish[,c(1,3)]

# aggregate species by session
prey_species <- prey_fish %>% group_by(session, species) %>% tally()

# rename abundance column
prey_species <- prey_species %>% rename(prey_abundance = n)

# select roving species and session columns
pred_species <- pred_edit[,c(1,3)]

# aggregate species by session
pred_species <- pred_species %>% group_by(session, species) %>% tally()

# rename abundance column
pred_species <- pred_species %>% rename(pred_abundance = n)


# Join Area ====================================================================

# The following joins the average survey area to each fish observation.

# select transect session and area columns
prey_area <- prey_meta[,c(1,15)]

# aggregate area by session
prey_area <- aggregate(.~session, prey_area, sum)

# join area to fish data
prey_density <- join(prey_species, prey_area, by = NULL, type = "full", 
                     match = "all")

# select roving session and area columns
pred_area <- pred_meta[,c(1,21)]

# aggregate area by session
pred_area <- aggregate(.~session, pred_area, sum)

# join area to fish data
pred_density <- join(pred_species, pred_area, by = NULL, type = "full",
                     match = "all")

# select SVC session, species, abundance, and area columns
SVC_density <- SVC_lengths[,c(1,37,38,12)]

# rename area column
SVC_density <- SVC_density %>% rename(SVC_area = SVC_cylinder_area)


# Density Calculation ==========================================================

# The following calculates densities for each observation of a species within a 
# session between the three survey types. 

# transect density calculation
prey_density$prey_density <- 
  (prey_density$prey_abundance)/(prey_density$prey_tran_area)

# roving density calculation 
pred_density$pred_density <- 
  (pred_density$pred_abundance)/(pred_density$pred_trans_area)

# SVC density calculation 
SVC_density$SVC_density <- 
  (SVC_density$SVC_abundance)/(SVC_density$SVC_area)


# Join Dataframes ==============================================================

# The following joins together SVC and transect survey data and SVC and roving
# survey data.

# join SVC to transect data
SVCprey_fishdens <- join(prey_density, SVC_density, by = NULL, type = "full", 
                         match = "all")

# select session, species, and density columns
SVCprey_fishdens <- SVCprey_fishdens[,c(1,2,5,8)]

# replace NA values with 0
SVCprey_fishdens[is.na(SVCprey_fishdens)] = 0

# join SVC to roving data
SVCpred_fishdens <- join(pred_density, SVC_density, by = NULL, type = "full", 
                         match = "all")

# select session, species, and density columns
SVCpred_fishdens <- SVCpred_fishdens[,c(1,2,5,8)]

# replace NA values with 0
SVCpred_fishdens[is.na(SVCpred_fishdens)] = 0


# SVC vs. Roving Survey Dataframe Edits ========================================

# The following performs final edits on the dataframe comparing SVC to roving 
# surveys in order to create the bargraph comparing their observed densitites 
# across species. 

# select binomial, common name, and predator presence columns
predator_presence <- traits[,c(3,4,7)]

# rename columns
predator_presence <- predator_presence %>% rename(species = common_name)
predator_presence <- predator_presence %>% rename(binomial = Binomial)

# join predator presence to SVC and roving survey dataframe
SVCpred_fishdens <- join(SVCpred_fishdens, predator_presence, by = NULL, 
                         type = "full", match = "all")

# filter for species present in predator/roving surveys
SVCpred_fishdens <- SVCpred_fishdens %>% filter(predator_presence == 1)

# remove binomial and predator presence columns
SVCpred_fishdens <- SVCpred_fishdens[,1:4]


# Density Differences Calculations =============================================

# The following calculates the density differences between SVC and transect 
# surveys and SVC and roving surveys.

# SVC vs. transect survey density difference calculation
SVCprey_fishdens$density_difference <- 
  SVCprey_fishdens$SVC_density-SVCprey_fishdens$prey_density

# SVC vs. roving survey density difference calculation
SVCpred_fishdens$density_difference <- 
  SVCpred_fishdens$SVC_density-SVCpred_fishdens$pred_density


# Aggregate by Species and Family ==============================================

# The following aggregates density differences between species and sessions into
# family averages when comparing SVC and transect data and species averages when
# comparing SVC and roving surveys. 

# select species and density difference columns from SVC vs. transect data
SVCprey_bar <- SVCprey_fishdens[,c(2,5)]

# select family and common name from fish trait data
family <- traits[,c(2,4)]

# rename columns
family <- family %>% rename(species = common_name)
family <- family %>% rename(family = Family)

# join family to SVC vs. transect survey data
SVCprey_bar <- join(SVCprey_bar, family, by = NULL, type = "full", 
                    match = "all")

# remove NA values
SVCprey_bar <- na.omit(SVCprey_bar)

# select family and density difference columns from SVC vs. transect data
SVCprey_family <- SVCprey_bar[,c(3,2)]

# aggregate by family
SVCprey_family <- summaryBy(density_difference~family, data=SVCprey_family, 
                            FUN=c(mean,sd))
# rename columns
SVCprey_family <- SVCprey_family %>% 
  rename(avg_density_dif = density_difference.mean)
SVCprey_family <- SVCprey_family %>% 
  rename(sd_density_dif = density_difference.sd)

# replace NA values with 0
SVCprey_family[is.na(SVCprey_family)] <- 0

# select species and density difference columns from SVC vs. roving data
SVCpred_bar <- SVCpred_fishdens[,c(2,5)]

# aggregate by species 
SVCpred_bar <- summaryBy(density_difference~species, data=SVCpred_bar, 
                         FUN=c(mean,sd))

# rename columns
SVCpred_bar <- SVCpred_bar %>% rename(avg_density_dif = density_difference.mean)
SVCpred_bar <- SVCpred_bar %>% rename(sd_density_dif = density_difference.sd)

# replace NA values with 0
SVCpred_bar[is.na(SVCpred_bar)] <- 0

# remove species = 0
SVCpred_bar <- SVCpred_bar[SVCpred_bar$species !=0,]


# Remove Single-Order Species ==================================================

# The following removes single-order species from the dataframes as these 
# species were not used in analyses.

# remove silversides and trumpetfish from SVC vs. transect data
SVCprey_family <- SVCprey_family[c(1:2,5:38),]

# remove trumpetfish from SVC vs. roving data
SVCpred_bar <- SVCpred_bar[c(1:23),]


# Density Difference Barplots ==================================================

# The following produces barplots of the density differences between survey 
# types. Differences are averaged across family between SVC and transect surveys
# and across species between SVC and roving surveys. 

# SVC vs. transect survey barplot
SVCprey_barplot <- ggplot(data=SVCprey_family, 
                          aes(x=family, y=avg_density_dif)) +
  geom_bar(stat="identity", fill="blue") +
  ylim(-0.3, 0.15) +
  theme_classic() + xlab("Family") + 
  ylab(bquote("Mean Density Difference" (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) 
SVCprey_barplot + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black")
ggsave(here('./visuals/SVCprey_barplot.png'), SVCprey_barplot)

# SVC vs. roving survey barplot
SVCpred_barplot <- ggplot(data=SVCpred_bar, aes(x=species, y=avg_density_dif)) +
  geom_bar(stat="identity", fill="blue") +
  theme_classic() + 
  xlab("Species") + 
  ylab(bquote("Mean Density Difference" (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  ylim(-0.03, 0.13)
SVCpred_barplot + coord_flip() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") 
ggsave(here('./visuals/SVCpred_barplot.png'), SVCpred_barplot)


# Density Difference Barplots with Error Bars ==================================

# The following produces barplots of the density differences between survey 
# types including error bars representing the standard deviation of the average 
# differences. Differences are averaged across family between SVC and transect 
# surveys and across species between SVC and roving surveys. 

# SVC vs. transect survey barplot
SVCprey_error_barplot <- ggplot(data=SVCprey_family_nosilversides, 
                                aes(x=family, y=avg_density_dif)) +
  geom_bar(stat="identity", fill="blue") +
  theme_classic() + xlab("Family") + 
  ylab(bquote("Mean Density Difference" (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) 
SVCprey_error_barplot + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_errorbar(aes(x=family, ymin=avg_density_dif-sd_density_dif, 
                ymax=avg_density_dif+sd_density_dif), width = 0.2, 
                colour = "black", alpha = 0.9, size = 1.3)
ggsave(here('./visuals/SVCprey_error_barplot.png'), SVCprey_error_barplot)

# SVC vs. roving survey barplot
SVCpred_error_barplot <- ggplot(data=SVCpred_bar, 
                                aes(x=species, y=avg_density_dif)) +
  geom_bar(stat="identity", fill="blue") +
  theme_classic() + 
  xlab("Species") + 
  ylab(bquote("Density Difference" (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20))
SVCpred_error_barplot + coord_flip() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  geom_errorbar(aes(x=species, ymin=avg_density_dif-sd_density_dif, 
                ymax=avg_density_dif+sd_density_dif), width = 0.2, 
                colour = "black", alpha = 0.9, size = 1.3)
ggsave(here('./visuals/SVCpred_error_barplot.png'), SVCpred_error_barplot)