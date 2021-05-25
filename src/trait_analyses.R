########## BIOL 499 PROJECT TRAIT ANALYSES ##########
########## 
##########
# This file utilizes linear mixed effects models to compare fish log density 
# differences between SVC and transect surveys and SVC and roving surveys as 
# well as an ANOVA to explore the effect of species' order on density 
# difference.
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
library(nlme)
library(car)
library(here)

# data
SVCprey_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))


# Species' Order ANOVA =========================================================

# The following performs an ANOVA to explore the effect of species' order on the
# density difference between survey types.

# SVC vs. transect survey order ANOVA
SVCprey_order_anova <- aov(SVC_prey_difference ~ as.factor(species_order), 
                           data = SVCprey_data)
summary(SVCprey_order_anova)

# SVC vs. transect survey Tukey test
TukeyHSD(SVCprey_order_anova)

# SVC vs. transect survey order boxplot
boxplot(SVCprey_data$SVC_prey_difference ~ SVCprey_data$species_order, 
        xlab = "Order", ylab = "Density Difference (SVC - Prey)")

# SVC vs. roving survey order ANOVA
SVCpred_orders_anova <- aov(SVC_pred_difference ~ as.factor(species_order), 
                            data = SVCpred_data)
summary(SVCpred_orders_anova)

# SVC vs roving survey Tukey test
TukeyHSD(SVCpred_orders_anova)

# SVC vs. roving survey order boxplot
boxplot(SVCpred_data$SVC_pred_difference ~ SVCpred_data$species_order, 
        xlab = "Order", ylab = "Density Difference (SVC - Predator)")


# Linear Mixed Effects Model: SVC vs. Transect Survey ==========================

# The following section removes species' which are the only members of their 
# respective orders in the dataframe before running a linear mixed effects model
# on the log density differences for fish observations between SVC and transect
# surveys. Collinearity is explored by examining covariate variance inflation 
# factors (VIFs). Model fit is explored by creating a random effects plot, 
# residual plot, qq plot, and model plot. 

# remove trumpetfish
SVCprey_model_data <- SVCprey_data[SVCprey_data$species_order 
                                   !="Syngnathiformes",] 

# remove silversides
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$species_order 
                                         !="Atheriniformes",]

# linear mixed effects model 
SVCprey_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+
                       nocturnal+position+max_length+colouration*behavior+
                       cryptic_behaviour+average_depth, 
                     random = list(~1|site, ~1|species_order), 
                     na.action = na.omit, SVCprey_model_data) 
# model structure: density difference as response, site and order as random 
# effects, using colouration x behaviour interaction

# save model output
saveRDS(SVCprey_model, here("./outputs/SVCprey_lme.rds"))

# model summary 
summary(SVCprey_model) 

# VIFs
vif(SVCprey_model)

# random effects plot
plot(ranef(SVCprey_model))

# residuals plot
res_SVCprey_model = residuals(SVCprey_model)
plot(res_SVCprey_model)

# qq plot
qqnorm(res_SVCprey_model) 
qqline(res_SVCprey_model) 

# model plot 
plot(SVCprey_model) 


# Linear Mixed Effects Model: SVC vs. Roving Survey ============================

# The following section removes species' which are the only members of their 
# respective orders in the dataframe before running a linear mixed effects model
# on the log density differences for fish observations between SVC and roving
# surveys. Collinearity is explored by examining covariate variance inflation 
# factors (VIFs). Habitat and depth were found to be collinear predictors based 
# on their VIFs >5, so individual models were run using each predictor. The 
# model using habitat over depth was found to perform better based on AIC values 
# so it was chosen to be used in further analyses. Model fit is explored by 
# creating a random effects plot, residual plot, qq plot, and model plot. 

# remove trumpetfish
SVCpred_model_data <- SVCpred_data[SVCpred_data$species_order 
                                   !="Syngnathiformes",] 

# linear mixed effects model
SVCpred_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+
                       nocturnal+position+max_length+colouration+behavior+
                       cryptic_behaviour+average_depth, 
                     random = list(~1|site, ~1|species_order), 
                     na.action = na.omit, SVCpred_model_data) 
# model structure: density difference as response, site and order as random 
# effects, no colouration x behaviour interaction (not significant). 

# save model output
saveRDS(SVCpred_model, here("./outputs/SVCpred_lme.rds"))

# model summary
summary(SVCpred_model) 

# VIFs
vif(SVCpred_model)

# lme with depth
SVCpred_depth_model <- lme(log_difference~octocoral+stony+relief_cm+size_bin+
                             nocturnal+position+max_length+colouration+behavior+
                             cryptic_behaviour+average_depth, 
                           random = list(~1|site, ~1|species_order), 
                           na.action = na.omit, SVCpred_model_data) 
# model structure: density difference as response, site and order as random 
# effects, no colouration x behaviour interaction (not significant)

# save depth model output
saveRDS(SVCpred_depth_model, here("./outputs/SVCprey_depth_lme.rds"))

# depth model summary
summary(SVCpred_depth_model) 

# depth lme VIFs
vif(SVCpred_depth_model) 

# lme with habitat
SVCpred_hab_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                           size_bin+nocturnal+position+max_length+colouration+
                           behavior+cryptic_behaviour, 
                         random = list(~1|site, ~1|species_order), 
                         na.action = na.omit, SVCpred_model_data) 
# model structure: density difference as response, site and order as random 
# effects, no colouration x behaviour interaction (not significant)

# save habitat model output
saveRDS(SVCpred_hab_model, here("./outputs/SVCprey_habitat_lme.rds"))

# habitat model summary
summary(SVCpred_hab_model) 

# habitat lme VIFs
vif(SVCpred_hab_model) 

# random effects plot
plot(ranef(SVCpred_hab_model))

# residuals plot
res_SVCpred_hab_model = residuals(SVCpred_hab_model)
plot(res_SVCpred_hab_model) 

# qq plot
qqnorm(res_SVCpred_hab_model) 
qqline(res_SVCpred_hab_model) 

# model plot
plot(SVCpred_hab_model) 