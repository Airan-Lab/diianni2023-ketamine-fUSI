
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)

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

# two-way mixed anova at each level of Sex
res_aov2_sex_ant <- peak_values_ant %>%
  group_by(Sex) %>%
  anova_test(dv = Peak, wid = Animal_ID, within = c(Region,Treatment))
res_aov2_sex_ant <- get_anova_table(res_aov2_sex_ant)

# write anova results to .txt file
write.csv(res_aov2_sex_ant, "res_peak_anova_twoway_sex_anterior.csv", row.names=FALSE)

# Pairwise comparisons between Treatment levels
res_pwc_treatment <- peak_values_ant %>%
  group_by(Region,Sex) %>%
  pairwise_t_test(Peak ~ Treatment,
                  p.adjust.method = "none", paired = T)
res_pwc_treatment$p.adj <- p.adjust(res_pwc_treatment$p, method = "fdr")
res_pwc_treatment <- add_significance(res_pwc_treatment,"p.adj")

# save pwc treatment results
res_pwc_treatment <- res_pwc_treatment[ , !(names(res_pwc_treatment) %in% ".y.")]
write.csv(res_pwc_treatment,"res_peak_pwc_treatment_anterior.csv")

# Treatment effect size
res_eff_size_treatment <-peak_values_ant %>% 
  group_by(Region,Sex) %>%
  cohens_d(Peak ~ Treatment, paired = T, hedges.correction = T)
res_eff_size_treatment

# save treatment effect size
res_eff_size_treatment <- res_eff_size_treatment[ , !(names(res_eff_size_treatment) %in% ".y.")]
res_eff_size_treatment <- relocate(res_eff_size_treatment,"Region")
res_eff_size_treatment <- relocate(res_eff_size_treatment,"Sex")
write.csv(res_eff_size_treatment,"res_peak_eff_size_treatment_anterior.csv")
