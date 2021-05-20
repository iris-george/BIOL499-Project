########## Reef Fish Detectability Across Survey Types ##########


#### Set-Up ####
setwd("/Users/irisgeorge/Documents/Green Lab/Lionfish/BIOL 499 Summary")

# Data:
SVC <- read.csv("SVC_data.csv")
prey_meta <- read.csv("prey_metadata.csv")
prey_fish <- read.csv("prey_fish_data.csv")
pred_meta <- read.csv("pred_metadata.csv")
pred_fish <- read.csv("pred_fish_data.csv")
traits <- read.csv("fish_traits.csv")
vert_relief <- read.csv("vertical_relief.csv")
lionfish <- read.csv("lionfish_anova_data.csv")

# Packages:
library(plyr)
library(tidyverse) 
library(data.table) 
library(nlme)
library(car)
library(ggplot2)
library(doBy)


#### SVC Lengths ####

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

# Bind SVC Lengths:
SVC_lengths <- combine(list(c2, c3, c4, c5, c6, c7))


#### Size Bins ####

# SVC
SVC_fish <- SVC_lengths[,c(1, 37, 42:51)] # selecting session, species, length, and abundance columns (from triangle distribution)
SVC_melt <- melt(setDT(SVC_fish), measure = patterns("L", "A"), variable.name = "var", value.name = c("length", "abundance")) # took from wide to long, pairing length and abundance columns
SVC_melt <- na.omit(SVC_melt) # took out all NA values
SVC_melt <- SVC_melt[order(session),] # ordered by session
SVC_melt <- SVC_melt %>% rename(SVC_tl = length) # renaming length column to match prey and predator
SVC_melt <- SVC_melt %>% rename(SVC_abundance = abundance) # renaming abundance column to match prey and predator (with unique identifier)

SVC_melt$size_bin = ifelse(SVC_melt$SVC_tl <= 5, 1, ifelse(SVC_melt$SVC_tl > 5 & SVC_melt$SVC_tl <= 10, 2, ifelse(SVC_melt$SVC_tl > 10 & SVC_melt$SVC_tl <= 15, 3, ifelse(SVC_melt$SVC_tl > 15 & SVC_melt$SVC_tl <= 20, 4, ifelse(SVC_melt$SVC_tl > 20 & SVC_melt$SVC_tl <=30, 5, ifelse(SVC_melt$SVC_tl > 30, 6, NA)))))) # created size bins in new column
SVC_bins <- SVC_melt 
SVC_bins <- SVC_bins[,c(1,2,5,6)] # selected session, species, size_bin, and abundance columns

# Prey
prey_fish$size_bin = ifelse(prey_fish$prey_tl <= 5, 1, ifelse(prey_fish$prey_tl > 5 & prey_fish$prey_tl <= 10, 2, ifelse(prey_fish$prey_tl > 10 & prey_fish$prey_tl <= 15, 3, ifelse(prey_fish$prey_tl > 15 & prey_fish$prey_tl <= 20, 4, ifelse(prey_fish$prey_tl > 20 & prey_fish$prey_tl <=30, 5, ifelse(prey_fish$prey_tl > 30, 6, NA)))))) # creates bins in new column
prey_bins <- prey_fish[,c(1,3,5)] # selecting session, species, and size_bin columns

# Predator
pred_fish$size_bin = ifelse(pred_fish$pred_tl <= 5, 1, ifelse(pred_fish$pred_tl > 5 & pred_fish$pred_tl <= 10, 2, ifelse(pred_fish$pred_tl > 10 & pred_fish$pred_tl <= 15, 3, ifelse(pred_fish$pred_tl > 15 & pred_fish$pred_tl <= 20, 4, ifelse(pred_fish$pred_tl > 20 & pred_fish$pred_tl <=30, 5, ifelse(pred_fish$pred_tl > 30, 6, NA)))))) # creates bins in new column
pred_bins <- pred_fish[,c(1,3,14)] # selecting session, species, and size_bin columns


#### Adding Abundance ####

# Prey
prey_abun <- prey_bins %>% group_by(session, species, size_bin) %>% tally() # combined rows with matching session, species, and size_bin and counted number of rows combined in separate column, n
prey_abun <- prey_abun %>% rename(prey_abundance = n) # renamed n column

# Predator
pred_abun <- pred_bins %>% group_by(session, species, size_bin) %>% tally() # combined rows with matching session, species, and size_bin and counted number of rows combined in separate column, n
pred_abun <- pred_abun %>% rename(pred_abundance = n) # renamed n column

# SVC
SVC_abun <- SVC_bins[, lapply(.SD, sum), by=list(session, species, size_bin)] # combined rows with matching session, species, and size_bin and summed the abundance values for combined rows 


#### Joining Survey Fish Dataframes ####

prey_pred <- full_join(prey_abun, pred_abun, by = NULL, copy = FALSE) # join prey and predator datasets
fish_data <- full_join(SVC_abun, prey_pred, by = NULL, copy = FALSE) # join SVC to prey and predator
fish_data[is.na(fish_data)] <- 0 # replace NA's with 0's 
fish_data <- fish_data[order(session),] # ordered by session


#### Joining Survey Metadata ####

# SVC
SVC_meta <- SVC[,c(1,3,4,12,16,17,33,34)] # select wanted metadata columns: session, site, date, diver, habitat, cylinder_area, max_depth, octocoral, stony
SVC_meta <- aggregate(.~session+site+SVC_date+SVC_habitat, SVC_meta, mean) # aggregated rows by session; combined character variables and took mean of numeric
SVC_meta <- SVC_meta %>% group_by(session, site, SVC_date, SVC_habitat) %>% summarise_each(funs(mean))
SVC_meta <- SVC_meta %>% rename(SVC_area = SVC_cylinder_area) # rename
SVC_full <- join(fish_data, SVC_meta, by = NULL, type = "left", match = "first") # joined SVC metadata to size data

# Prey
prey_meta <- prey_meta[,c(1,4,5,7,15)] # select wanted metadata columns: session, site, date, transect_area, depth
prey_meta <- prey_meta %>% rename(prey_depth = prey_depth_m) # rename
prey_meta <- prey_meta %>% rename(prey_area = prey_tran_area) # rename
prey_meta <- transform(prey_meta, prey_depth = as.numeric(prey_depth), prey_area = as.numeric(prey_area)) # transformed depth and area columns from character to numeric
# want to aggregate depth by mean and area by sum: splitting up
prey_depth <- prey_meta[,c(1:4)] # taking out area
prey_depth <- aggregate(.~session+site+prey_date, prey_depth, mean) # aggregated rows by session; combined character variables and took mean of depth
prey_area <- prey_meta[,c(1:3,5)] # taking out depth
prey_area <- aggregate(.~session+site+prey_date, prey_area, sum) # aggregated rows by session; combined character variables and summed areas
prey_meta <- join(prey_depth, prey_area, by = NULL, type = "full", match = "all") # joining prey depth and prey area to make full prey metadata 
SVCprey_full <- join(SVC_full, prey_meta, by = NULL, type = "left", match = "first") # joined prey metadata to size and SVC data

# Predator
pred_meta <- pred_meta[,c(1,4,8,17,21)] # select wanted metadata columns: session, site, date, transect_area, depth
pred_meta <- pred_meta %>% rename(pred_depth = pred_depth_ft) # rename
pred_meta <- pred_meta %>% rename(pred_area = pred_trans_area) # rename
# want to aggregate depth by mean and area by sum: splitting up
pred_depth <- pred_meta[,c(1:4)] # taking out area
pred_depth <- aggregate(.~session+site+pred_date, pred_depth, mean) # aggregated rows by session; combined character variables and took mean of depth
pred_area <- pred_meta[,c(1:3,5)] # taking out depth
pred_area <- aggregate(.~session+site+pred_date, pred_area, sum) # aggregated rows by session; combined character variables and summed areas
pred_meta <- join(pred_depth, pred_area, by = NULL, type = "full", match = "all") # joining predator depth and predatro area to make full predator metadata 
fish_meta <- join(SVCprey_full, pred_meta, by = NULL, type = "left", match = "first") # joined prey metadata to size and SVC data

# Vertical Relief
vert_relief <- vert_relief[,c(1,7)] # select site and vert relief columns
vert_relief <- aggregate(relief_cm~site, vert_relief, mean) # gives mean relief per site
fish_meta <- join(fish_meta, vert_relief, by = NULL, type = "left", match = "first") # assigned vert_relief measure to each site

# Traits
fish_traits <- filter(traits, lifestage == "adult") # filter only adults
fish_traits <- fish_traits[,c(1:4,7,18,36,38,39,58,60)] # select latin names, predator presence, nocturnal, max_length, position, behaviour, colouration, and cryptic_behaviour
fish_traits <- fish_traits %>% rename(colouration = colouration_cat1)
fish_traits <- fish_traits %>% rename(species = common_name)
full_dataframe <- join(fish_meta, fish_traits, by = NULL, type = "full", match = "all")
full_dataframe <- na.omit(full_dataframe) # took out all NA values


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

#### Order ANOVAs ####

# SVC vs. Prey:
SVCprey_order_anova <- aov(SVC_prey_difference ~ as.factor(species_order), data = SVCprey_data)
summary(SVCprey_order_anova)
boxplot(SVCprey_data$SVC_prey_difference ~ SVCprey_data$species_order, xlab = "Order", ylab = "Density Difference (SVC - Prey)")
TukeyHSD(SVCprey_order_anova)

# SVC vs. Predator:
SVCpred_orders_anova <- aov(SVC_pred_difference ~ as.factor(species_order), data = SVCpred_data)
summary(SVCpred_orders_anova)
boxplot(SVCpred_data$SVC_pred_difference ~ SVCpred_data$species_order, xlab = "Order", ylab = "Density Difference (SVC - Predator)")
TukeyHSD(SVCpred_orders_anova)


#### Linear Mixed Effects Model: SVC vs. Prey ####

# Remove Single-Species Orders:
SVCprey_model_data <- SVCprey_data[SVCprey_data$species_order !="Syngnathiformes",] # remove trumpetfish
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$species_order !="Atheriniformes",] # remove silversides

# Full Model:
SVCprey_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration*behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) # model structure: density difference as response, site and order as random effects, using colouration x behaviour interaction
summary(SVCprey_model) # model summary
vif(SVCprey_model) # no GVIF > 5

# Random Effects Plot:
plot(ranef(SVCprey_model))

# Residuals Plots:
res_SVCprey_model = residuals(SVCprey_model)
plot(res_SVCprey_model) # residuals plot
qqnorm(res_SVCprey_model) 
qqline(res_SVCprey_model) # qq plot
plot(SVCprey_model) 


#### Linear Mixed Effects Model: SVC vs. Predator ####

# Remove Single-Species Orders:
SVCpred_model_data <- SVCpred_data[SVCpred_data$species_order !="Syngnathiformes",] # remove trumpetfish

# Full Model:
SVCpred_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) # model structure: density difference as response, site and order as random effects, no colouration x behaviour interaction (not significant)
summary(SVCpred_model) # model summary
vif(SVCpred_model) # habitat and average_depth GVIF > 5 (and colouration)

# Depth Model:
SVCpred_depth_model <- lme(log_difference~octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) # model structure: density difference as response, site and order as random effects, no colouration x behaviour interaction (not significant)
summary(SVCpred_depth_model) # model summary; AIC = 2187.758
vif(SVCpred_depth_model) # colouration GVIF > 5 

# Habitat Model:
SVCpred_hab_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration+behavior+cryptic_behaviour, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) # model structure: density difference as response, site and order as random effects, no colouration x behaviour interaction (not significant)
summary(SVCpred_hab_model) # model summary; AIC = 2179.73 
vif(SVCpred_hab_model) # habitat and average_depth GVIF > 5 (and colouration)
# habitat model performs better than depth model based on AIC

# Random Effects Plot:
plot(ranef(SVCpred_hab_model))

# Residuals Plots:
res_SVCpred_hab_model = residuals(SVCpred_hab_model)
plot(res_SVCpred_hab_model) # residuals plot
qqnorm(res_SVCpred_hab_model) 
qqline(res_SVCpred_hab_model) # qq plot
plot(SVCpred_hab_model) 


#### Lionfish ANOVA ####

# ANOVA:
lionfish_anova <- aov(density ~ survey, data = lionfish)
summary(lionfish_anova)
boxplot(lionfish$density~lionfish$survey)
TukeyHSD(lionfish_anova)

# Boxplot: 
ggplot(lionfish, aes(x = survey, y = density, fill = survey)) +
  geom_boxplot() +
  theme_classic() + xlab("Survey Type") + 
  ylab(bquote("Density " (lionfish/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") 

# Remove SVC Outlier:
lionfish_nooutlier <- subset(lionfish, lionfish$density < 0.025)
lionfish_anova_nooutliers <- aov(density ~ survey, data = lionfish_nooutlier)
summary(lionfish_anova_nooutliers)
boxplot(lionfish_nooutlier$density~lionfish_nooutlier$survey)
TukeyHSD(lionfish_anova_nooutliers)


#### Covariate Boxplots ####

# SVC vs. Prey Model:
position_subset <- subset(SVCprey_data, position !="NA")
ggplot(position_subset, aes(x = position, y = log_difference, fill = position)) +
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

boxplot(SVCprey_data$log_difference~SVCprey_data$colouration) # cryptic colouration value = 1.659633, no colouration = -0.009229 (insig)

boxplot(SVCprey_data$log_difference~SVCprey_data$behavior) # shoaling = 0.714334, solitary = 0.823100

interaction_subset <- subset(SVCprey_data, behavior !="NA")
ggplot(interaction_subset, aes(behavior, log_difference, fill = colouration)) + geom_boxplot() + theme_classic() + scale_fill_manual(name = "Colouration", labels = c("Conspicuous", "Cryptic", "None"), values = c("yellow1", "gray65", "white")) + xlab("Aggregation Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# SVC vs. Predator Model: 
behaviour_subset <- subset(SVCpred_data, behavior !="NA")
ggplot(behaviour_subset, aes(x = behavior, y = log_difference, fill = behavior)) +
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

crypticbehaviour_subset <- subset(SVCpred_data, cryptic_behaviour !="NA")
crypticbehaviour_subset$behaviour <- ifelse(crypticbehaviour_subset$cryptic_behaviour == 1, "cryptic behaviour", "none")
ggplot(crypticbehaviour_subset, aes(x = behaviour, y = log_difference, fill = behaviour)) +
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










