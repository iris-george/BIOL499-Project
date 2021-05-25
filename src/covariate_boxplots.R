########## BIOL 499 PROJECT COVARITATE BOXPLOTS ##########
########## 
##########
# This file creates boxplot visuals based off of the results of linear mixed 
# effects models comparing fish densities across SVC, transect, and roving 
# surveys. 
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


# Covariate Boxplots: SVC vs. Transect Surveys =================================

# The following creates boxplots of significant covariates from the linear 
# mixed effects model comparing SVC to transect surveys.

# water column position boxplot
SVCprey_position_box <- ggplot(SVCprey_data, aes(x = position, 
                        y = log_difference, fill = position)) +
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
SVCprey_position_box
ggsave(here('./visuals/SVCprey_position_box.png'), SVCprey_position_box)

# aggregation behaviour x colouration interaction boxplot
SVCprey_bevcol_box <- ggplot(SVCprey_data, aes(behavior, log_difference, 
                                               fill = colouration)) + 
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
ggsave(here('./visuals/SVCprey_behaviour_colouration_box.png'), 
       SVCprey_bevcol_box)


# Covariate Boxplots: SVC vs. Roving Surveys ===================================

# The following creates boxplots of significant covariates from the linear 
# mixed effects model comparing SVC to roving surveys.

# aggregation behaviour boxplot
SVCpred_behaviour_box <- ggplot(SVCpred_data, aes(x = behavior, 
                         y = log_difference, fill = behavior)) +
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
ggsave(here('./visuals/SVCpred_behaviour_box.png'), SVCpred_behaviour_box)

# cryptic behaviour boxplot 
SVCpred_data$behaviour <- ifelse(SVCpred_data$cryptic_behaviour == 1, 
                                 "cryptic behaviour", "none")
SVCpred_cryptic_box <- ggplot(SVCpred_data, aes(x = behaviour, 
                       y = log_difference, fill = behaviour)) +
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
ggsave(here('./visuals/SVCpred_cryptic_box.png'), SVCpred_cryptic_box)