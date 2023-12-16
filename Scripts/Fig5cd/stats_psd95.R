
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)

# load data
data_psd95 <- read_csv('../data/psd95.csv')

######################
# psd95

# three-way anova
res_aov3_psd <- anova_test(
  data = data_psd95, dv = PSD95, wid = Animal_ID,
  between = c(KET,NTX,Sex)
)
res_aov3_psd <- get_anova_table(res_aov3_psd)

# write anova results to .txt file
write.csv(res_aov3_psd, "res_anova_threeway_psd.csv", row.names=FALSE)

# Pairwise comparisons between Treatment levels
res_pwc_treat_psd <- data_psd95 %>%
  group_by(Sex) %>%
  pairwise_t_test(PSD95 ~ Treatment,
                  p.adjust.method = "fdr", paired = F)
res_pwc_treat_psd

# save pwc treatment results
res_pwc_treat_psd <- res_pwc_treat_psd[ , !(names(res_pwc_treat_psd) %in% ".y.")]
write.csv(res_pwc_treat_psd,"res_psd_pwc_treatment.csv")

# Treatment effect size
res_eff_size_treat_psd <- data_psd95 %>%
  group_by(Sex) %>%
  cohens_d(PSD95 ~ Treatment, paired = F, hedges.correction = T)
res_eff_size_treat_psd

# save treatment effect size
res_eff_size_treat_psd <- res_eff_size_treat_psd[ , !(names(res_eff_size_treat_psd) %in% ".y.")]
res_eff_size_treat_psd <- relocate(res_eff_size_treat_psd,"Sex")
write.csv(res_eff_size_treat_psd,"res_psd_eff_size_treatment.csv")

######################

######################
# dapi

# three-way anova
res_aov3_dapi <- anova_test(
  data = data_psd95, dv = DAPI, wid = Animal_ID,
  between = c(KET,NTX,Sex)
)
res_aov3_dapi <- get_anova_table(res_aov3_dapi)

# write anova results to .txt file
write.csv(res_aov3_dapi, "res_anova_threeway_dapi.csv", row.names=FALSE)

# Pairwise comparisons between Treatment levels
res_pwc_treat_dapi <- data_psd95 %>%
  group_by(Sex) %>%
  pairwise_t_test(DAPI ~ Treatment,
                  p.adjust.method = "fdr", paired = F)
res_pwc_treat_dapi

# save pwc treatment results
res_pwc_treat_dapi <- res_pwc_treat_dapi[ , !(names(res_pwc_treat_dapi) %in% ".y.")]
write.csv(res_pwc_treat_dapi,"res_dapi_pwc_treatment.csv")

# Treatment effect size
res_eff_size_treat_dapi <- data_psd95 %>%
  group_by(Sex) %>%
  cohens_d(DAPI ~ Treatment, paired = F, hedges.correction = T)
res_eff_size_treat_dapi

# save treatment effect size
res_eff_size_treat_dapi <- res_eff_size_treat_dapi[ , !(names(res_eff_size_treat_dapi) %in% ".y.")]
res_eff_size_treat_dapi <- relocate(res_eff_size_treat_dapi,"Sex")
write.csv(res_eff_size_treat_dapi,"res_dapi_eff_size_treatment.csv")
