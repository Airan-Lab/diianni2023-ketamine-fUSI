
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)

################
# ANTERIOR SLICE
peak_values_ant <- read_csv('../data/peak_cbv_table_ketamine_anterior.csv')

# exclude regions that do not show effect in functional maps
drops <- c("PrL","IL","S1","Ins")
peak_values_ant <- peak_values_ant[ , !(names(peak_values_ant) %in% drops)]

# exclude NAL+VEH group from this analysis
peak_values_ant <- peak_values_ant[ -c(which(peak_values_ant$Treatment == "NAL+VEH")), ]

# tidy dataset
peak_values_ant <- peak_values_ant %>% 
  pivot_longer(Cg1:NAcSh, names_to = "Region", values_to = "Peak") 

peak_values_ant <- peak_values_ant %>%
  convert_as_factor(Treatment, Region)
################

################
# POSTERIOR SLICE
peak_values_post <- read_csv('../data/peak_cbv_table_ketamine_posterior.csv')

# exclude regions that do not show effect in functional maps
drops <- c("LPtA","MPtA","Hipp","VM","VPM","VPL")
peak_values_post <- peak_values_post[ , !(names(peak_values_post) %in% drops)]

# exclude NAL+VEH group from this analysis
peak_values_post <- peak_values_post[ -c(which(peak_values_post$Treatment == "NAL+VEH")), ]

# tidy dataset
peak_values_post <- peak_values_post %>% 
  pivot_longer(RSG:LPLR, names_to = "Region", values_to = "Peak")

peak_values_post <- peak_values_post %>%
  convert_as_factor(Treatment, Region)
################

# merge data frames for 
peak_values <- rbind(peak_values_ant, peak_values_post)

# peak_values <- peak_values %>%
#   convert_as_factor(Treatment, Region)

# two-way mixed anova at each level of Region
res_aov2_reg <- peak_values %>%
  group_by(Region) %>%
  anova_test(dv = Peak, wid = Animal_ID,  between = Sex, within = Treatment)
res_aov2_reg <- get_anova_table(res_aov2_reg)

# write anova results to .txt file
write.csv(res_aov2_reg, "res_peak_twoway_anova_region.csv", row.names=FALSE)