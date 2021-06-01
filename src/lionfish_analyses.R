########## BIOL 499 PROJECT LIONFISH ANALYSES ##########
########## 
##########
# This file explores lionfish observations collected from SVC, transect, and 
# roving surveys and explores the differences in density and presence/absence
# between the survey types using an ANOVA and visually using boxplots. 
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
library(doBy)
library(here)

# data
lionfish <- read_csv(here("./dataframes/lionfish_anova_data.csv"))

# change survey type names
lionfish$survey <- ifelse(lionfish$survey == "prey", "transect", 
                          ifelse(lionfish$survey == "predator", "roving", 
                          ifelse(lionfish$survey == "SVC", "SVC", NA)))


# Lionfish ANOVA ===============================================================

# The following performs an ANOVA to explore the differences in lionfish density
# across three survey types: SVC, transect, and roving surveys. A boxplot is 
# made to visually interpret the results, and it is re-run to examine the 
# potential effects of high outliers from SVC surveys. 

# ANOVA
lionfish_anova <- aov(density ~ survey, data = lionfish)
summary(lionfish_anova)

# Tukey test
TukeyHSD(lionfish_anova)


# Lionfish Density Boxplot ===================================================== 

# The following produces a boxplot which shows the difference in average 
# lionfish density reported in SVC, transect, and roving surveys.

# boxplots
ggplot(lionfish, aes(x = survey, y = density, fill = survey)) +
  geom_boxplot() +
  theme_classic() + xlab("Survey Type") + 
  ylab(bquote("Density " (lionfish/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "YlGnBu") 


# Remove High Outliers =========================================================

# The following re-runs the ANOVA on lionfish density across the three survey 
# types to examine the potential effects of high outliers from SVC surveys on 
# the results. 

# remove SVC outliers 
lionfish_nooutlier <- subset(lionfish, lionfish$density < 0.025)

# re-run ANOVA
lionfish_anova_nooutliers <- aov(density ~ survey, data = lionfish_nooutlier)
summary(lionfish_anova_nooutliers)

# Tukey test
TukeyHSD(lionfish_anova_nooutliers)

# boxplot without outliers
ggplot(lionfish_nooutlier, aes(x = survey, y = density, fill = survey)) +
  geom_boxplot() +
  theme_classic() + xlab("Survey Type") + 
  ylab(bquote("Density " (lionfish/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") 


# Presence/Absence Exploration =================================================

# The following re-runs the ANOVA on lionfish presence/absence as opposed to 
# density across the three survey types. 

# create presence/absence column
lionfish$pres_abs <- ifelse(lionfish$density > 0, 1, 0)

# ANOVA
lionfish_pres_abs_ANOVA <- aov(pres_abs ~ survey, data = lionfish)
summary(lionfish_pres_abs_ANOVA)

# Tukey test
TukeyHSD(lionfish_pres_abs_ANOVA)

# boxplot
ggplot(lionfish, aes(x = survey, y = pres_abs, fill = survey)) +
  geom_boxplot() +
  theme_classic() + xlab("Survey Type") + 
  ylab(bquote("Density " (lionfish/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "YlGnBu") 

# make barplot dataframe - aggregate presence/absence into sum
SVCprey_family <- summaryBy(density_difference~family, data=SVCprey_family, 
                            FUN=c(mean,sd))
lionfish_bar <- summaryBy(pres_abs~survey, data = lionfish, FUN = sum)

# barplot
ggplot(data = lionfish_bar, aes(x = survey, y = pres_abs.sum)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_classic() + xlab("Survey Type") + 
  ylab(bquote("Number of Sessions with Lionfish Present")) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) 