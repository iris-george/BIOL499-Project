########## BIOL 499 PROJECT VISUALS ##########
########## 
##########
# This file creates boxplot and barplot visuals based off of the results of 
# linear mixed effects models comparing fish densities across SVC, transect, and
# roving surveys. 
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
library(ggplot2)
library(here)

# data
SVCprey_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))
prey_fish <- read_csv(here("./clean_data/prey_fish_data.csv"))
pred_fish <- read_csv(here("./clean_data/pred_fish_data.csv"))
prey_meta <- read_csv(here("./clean_data/prey_metadata.csv"))
pred_meta <- read_csv(here("./clean_data/pred_metadata.csv"))
SVC_lengths <- read_csv(here("./clean_data/SVC_lengths.csv"))


# Covariate Boxplots: SVC vs. Transect Surveys =================================

# The following creates boxplots of significant covariates from the linear 
# mixed effects model comparing SVC to transect surveys.

# water column position boxplot
ggplot(SVCprey_data, aes(x = position, y = log_difference, fill = position)) +
  geom_boxplot() +
  theme_classic() + xlab("Water Column Position") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# aggregation behaviour x colouration interaction boxplot
ggplot(SVCprey_data, aes(behavior, log_difference, fill = colouration)) + 
  geom_boxplot() + 
  theme_classic() + scale_fill_manual(name = "Colouration", 
       labels = c("Conspicuous", "Cryptic", "None"), 
       values = c("yellow1", "gray65", "white")) + 
  xlab("Aggregation Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")


# Covariate Boxplots: SVC vs. Roving Surveys ===================================

# The following creates boxplots of significant covariates from the linear 
# mixed effects model comparing SVC to roving surveys.

# aggregation behaviour boxplot
ggplot(SVCpred_data, aes(x = behavior, y = log_difference, fill = behavior)) +
  geom_boxplot() +
  theme_classic() + xlab("Aggregation Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# cryptic behaviour boxplot 
SVCpred_data$behaviour <- ifelse(SVCpred_data$cryptic_behaviour == 1, 
                                 "cryptic behaviour", "none")
ggplot(SVCpred_data, aes(x = behaviour, y = log_difference, fill = behaviour)) +
  geom_boxplot() +
  theme_classic() + xlab("Cryptic Behaviour Presence") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")


#### Density Difference Barplots ####

# Data Edits:
pred_edit <- pred_fish[pred_fish$species !="grouper sp.",]
pred_edit <- pred_fish[pred_fish$species !="soapfish sp.",]
pred_edit <- pred_fish[pred_fish$species !="",]

# Aggregate Species:
prey_species <- prey_fish[,c(1,3)]
prey_species <- prey_fish %>% group_by(session, species) %>% tally()
prey_species <- prey_species %>% rename(prey_abundance = n)

pred_species <- pred_edit[,c(1,3)]
pred_species <- pred_species %>% group_by(session, species) %>% tally()
pred_species <- pred_species %>% rename(pred_abundance = n)

# Join Area:
prey_area <- prey_meta[,c(1,5)]
prey_area <- aggregate(.~session, prey_area, mean)
prey_density <- join(prey_species, prey_area, by = NULL, type = "full", match = "all")

pred_area <- pred_meta[,c(1,5)]
pred_area <- aggregate(.~session, pred_area, mean)
pred_density <- join(pred_species, pred_area, by = NULL, type = "full", match = "all")

SVC_density <- SVC_lengths[,c(1,37,38,12)]
SVC_density <- SVC_density %>% rename(SVC_area = SVC_cylinder_area)

# Calculate Densities:
prey_density$prey_density <- (prey_density$prey_abundance)/(prey_density$prey_area)
pred_density$pred_density <- (pred_density$pred_abundance)/(pred_density$pred_area)
SVC_density$SVC_density <- (SVC_density$SVC_abundance)/(SVC_density$SVC_area)

# Join Dataframes:
SVCprey_fishdens <- join(prey_density, SVC_density, by = NULL, type = "full", match = "all")
SVCprey_fishdens <- SVCprey_fishdens[,c(1,2,5,8)]
SVCprey_fishdens[is.na(SVCprey_fishdens)] = 0

SVCpred_fishdens <- join(pred_density, SVC_density, by = NULL, type = "full", match = "all")
SVCpred_fishdens <- SVCpred_fishdens[,c(1,2,5,8)]
SVCpred_fishdens[is.na(SVCpred_fishdens)] = 0

# SVC vs. Predator Dataframe Edits:
predator_presence <- traits[,c(3,4,7)]
predator_presence <- predator_presence %>% rename(species = common_name)
predator_presence <- predator_presence %>% rename(binomial = Binomial)
SVCpred_fishdens <- join(SVCpred_fishdens, predator_presence, by = NULL, type = "full", match = "all")
SVCpred_fishdens <- SVCpred_fishdens %>% filter(predator_presence == 1)
SVCpred_fishdens <- SVCpred_fishdens[,1:4]

# Density Differences:
SVCprey_fishdens$density_difference <- SVCprey_fishdens$SVC_density-SVCprey_fishdens$prey_density
SVCpred_fishdens$density_difference <- SVCpred_fishdens$SVC_density-SVCpred_fishdens$pred_density

# Aggregate by Species and Family:
SVCprey_bar <- SVCprey_fishdens[,c(2,5)]
family <- traits[,c(2,4)]
family <- family %>% rename(species = common_name)
family <- family %>% rename(family = Family)
SVCprey_bar <- join(SVCprey_bar, family, by = NULL, type = "full", match = "all")
SVCprey_bar <- na.omit(SVCprey_bar)

SVCprey_family <- SVCprey_bar[,c(3,2)]
SVCprey_family <- summaryBy(density_difference~family, data=SVCprey_family, FUN=c(mean,sd))
SVCprey_family <- SVCprey_family %>% rename(avg_density_dif = density_difference.mean)
SVCprey_family <- SVCprey_family %>% rename(sd_density_dif = density_difference.sd)
SVCprey_family[is.na(SVCprey_family)] <- 0

SVCpred_bar <- SVCpred_fishdens[,c(2,5)]
SVCpred_bar <- summaryBy(density_difference~species, data=SVCpred_bar, FUN=c(mean,sd))
SVCpred_bar <- SVCpred_bar %>% rename(avg_density_dif = density_difference.mean)
SVCpred_bar <- SVCpred_bar %>% rename(sd_density_dif = density_difference.sd)
SVCpred_bar[is.na(SVCpred_bar)] <- 0
SVCpred_bar <- SVCpred_bar[SVCpred_bar$species !=0,]

# Remove Silversides:
SVCprey_family_nosilversides <- SVCprey_family[c(1:2,4:38),]

# Barplot:
SVCprey_nosilversides_barplot <- ggplot(data=SVCprey_family_nosilversides, aes(x=family, y=avg_density_dif)) +
  geom_bar(stat="identity", fill="blue") +
  ylim(-0.4, 0.2) +
  theme_classic() + xlab("Family") + 
  ylab(bquote("Mean Density Difference" (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) 
SVCprey_nosilversides_barplot + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black")

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

# Barplot with Error Bars:
SVCprey_error_barplot <- ggplot(data=SVCprey_family_nosilversides, aes(x=family, y=avg_density_dif)) +
  geom_bar(stat="identity", fill="blue") +
  theme_classic() + xlab("Family") + 
  ylab(bquote("Mean Density Difference" (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) 
SVCprey_error_barplot + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_errorbar(aes(x=family, ymin=avg_density_dif-sd_density_dif, ymax=avg_density_dif+sd_density_dif), width = 0.2, colour = "black", alpha = 0.9, size = 1.3)

SVCpred_error_barplot <- ggplot(data=SVCpred_bar, aes(x=species, y=avg_density_dif)) +
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
  geom_errorbar(aes(x=species, ymin=avg_density_dif-sd_density_dif, ymax=avg_density_dif+sd_density_dif), width = 0.2, colour = "black", alpha = 0.9, size = 1.3)

