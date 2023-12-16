
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)

ARG <- read_csv('../data/autoradiography.csv')

######### NAc

# two-way anova
res_aov2_nac <- anova_test(
  data = ARG, dv = NAc, wid = Animal_ID,
  between = c(Treatment,Sex)
)
res_aov2_nac <- get_anova_table(res_aov2_nac)

# write anova results to .txt file
write.csv(res_aov2_nac, "res_nac_anova_twoway.csv", row.names=FALSE)

# Pairwise comparison - treatment
res_pwc_nac_treatment <- ARG %>%
  group_by(Sex) %>%
  pairwise_t_test(NAc ~ Treatment,
                  p.adjust.method = "fdr", paired = F)
res_pwc_nac_treatment

# save pwc treatment results
res_pwc_nac_treatment <- res_pwc_nac_treatment[ , !(names(res_pwc_nac_treatment) %in% ".y.")]
write.csv(res_pwc_nac_treatment,"res_nac_pwc_treatment.csv")

# Effect size - treatment
res_eff_size_nac_treatment <- ARG %>%
  group_by(Sex) %>%
  cohens_d(NAc ~ Treatment, paired = F, hedges.correction = T)
res_eff_size_nac_treatment

# save treatment effect size
res_eff_size_nac_treatment <- res_eff_size_nac_treatment[ , !(names(res_eff_size_nac_treatment) %in% ".y.")]
res_eff_size_nac_treatment <- relocate(res_eff_size_nac_treatment,"Sex")
write.csv(res_eff_size_nac_treatment,"res_nac_eff_size_treatment.csv")

# Pairwise comparison - Sex
res_pwc_nac_sex <- ARG %>%
  group_by(Treatment) %>%
  pairwise_t_test(NAc ~ Sex,
                  p.adjust.method = "fdr", paired = F)
res_pwc_nac_sex

# save pwc treatment results
res_pwc_nac_sex <- res_pwc_nac_sex[ , !(names(res_pwc_nac_sex) %in% ".y.")]
write.csv(res_pwc_nac_sex,"res_nac_pwc_sex.csv")

# Effect size - Sex
res_eff_size_nac_sex <- ARG %>%
  group_by(Treatment) %>%
  cohens_d(NAc ~ Sex, paired = F, hedges.correction = T)
res_eff_size_nac_sex

# save treatment effect size
res_eff_size_nac_sex <- res_eff_size_nac_sex[ , !(names(res_eff_size_nac_sex) %in% ".y.")]
res_eff_size_nac_sex <- relocate(res_eff_size_nac_sex,"Treatment")
write.csv(res_eff_size_nac_sex,"res_nac_eff_size_sex.csv")


######### CPu

# two-way anova
res_aov2_cpu <- anova_test(
  data = ARG, dv = CPu, wid = Animal_ID,
  between = c(Treatment,Sex)
)
res_aov2_cpu <- get_anova_table(res_aov2_cpu)

# write anova results to .txt file
write.csv(res_aov2_cpu, "res_cpu_anova_twoway.csv", row.names=FALSE)

# Pairwise comparison - treatment
res_pwc_cpu_treatment <- ARG %>%
  group_by(Sex) %>%
  pairwise_t_test(CPu ~ Treatment,
                  p.adjust.method = "fdr", paired = F)
res_pwc_cpu_treatment

# save pwc treatment results
res_pwc_cpu_treatment <- res_pwc_cpu_treatment[ , !(names(res_pwc_cpu_treatment) %in% ".y.")]
write.csv(res_pwc_cpu_treatment,"res_cpu_pwc_treatment.csv")

# Effect size - Treatment
res_eff_size_cpu_treatment <- ARG %>%
  group_by(Sex) %>%
  cohens_d(CPu ~ Treatment, paired = F, hedges.correction = T)
res_eff_size_cpu_treatment

# save treatment effect size
res_eff_size_cpu_treatment <- res_eff_size_cpu_treatment[ , !(names(res_eff_size_cpu_treatment) %in% ".y.")]
res_eff_size_cpu_treatment <- relocate(res_eff_size_cpu_treatment,"Sex")
write.csv(res_eff_size_cpu_treatment,"res_cpu_eff_size_treatment.csv")

# Pairwise comparison - SEx
res_pwc_cpu_sex <- ARG %>%
  group_by(Treatment) %>%
  pairwise_t_test(CPu ~ Sex,
                  p.adjust.method = "fdr", paired = F)
res_pwc_cpu_sex

# save pwc treatment results
res_pwc_cpu_sex <- res_pwc_cpu_sex[ , !(names(res_pwc_cpu_sex) %in% ".y.")]
write.csv(res_pwc_cpu_sex,"res_cpu_pwc_sex.csv")

# Effect size - Sex
res_eff_size_cpu_sex <- ARG %>%
  group_by(Treatment) %>%
  cohens_d(CPu ~ Sex, paired = F, hedges.correction = T)
res_eff_size_cpu_sex

# save treatment effect size
res_eff_size_cpu_sex <- res_eff_size_cpu_sex[ , !(names(res_eff_size_cpu_sex) %in% ".y.")]
res_eff_size_cpu_sex <- relocate(res_eff_size_cpu_sex,"Treatment")
write.csv(res_eff_size_cpu_sex,"res_cpu_eff_size_sex.csv")


######### Cg1/PrL (PFC)

# two-way anova
res_aov2_pfc <- anova_test(
  data = ARG, dv = PFC, wid = Animal_ID,
  between = c(Treatment,Sex)
)
res_aov2_pfc <- get_anova_table(res_aov2_pfc)

# write anova results to .txt file
write.csv(res_aov2_pfc, "res_pfc_anova_twoway.csv", row.names=FALSE)

# Pairwise comparison - Treatment
res_pwc_pfc_treatment <- ARG %>%
  group_by(Sex) %>%
  pairwise_t_test(PFC ~ Treatment,
                  p.adjust.method = "fdr", paired = F)
res_pwc_pfc_treatment

# save pwc treatment results
res_pwc_pfc_treatment <- res_pwc_pfc_treatment[ , !(names(res_pwc_pfc_treatment) %in% ".y.")]
write.csv(res_pwc_pfc_treatment,"res_pfc_pwc_treatment.csv")

# Effect size - Treatment
res_eff_size_pfc_treatment <- ARG %>%
  group_by(Sex) %>%
  cohens_d(PFC ~ Treatment, paired = F, hedges.correction = T)
res_eff_size_pfc_treatment

# save treatment effect size
res_eff_size_pfc_treatment <- res_eff_size_pfc_treatment[ , !(names(res_eff_size_pfc_treatment) %in% ".y.")]
res_eff_size_pfc_treatment <- relocate(res_eff_size_pfc_treatment,"Sex")
write.csv(res_eff_size_pfc_treatment,"res_pfc_eff_size_treatment.csv")

# Pairwise comparison - Sex
res_pwc_pfc_sex <- ARG %>%
  group_by(Treatment) %>%
  pairwise_t_test(PFC ~ Sex,
                  p.adjust.method = "fdr", paired = F)
res_pwc_pfc_sex

# save pwc treatment results
res_pwc_pfc_sex <- res_pwc_pfc_sex[ , !(names(res_pwc_pfc_sex) %in% ".y.")]
write.csv(res_pwc_pfc_sex,"res_pfc_pwc_sex.csv")

# Effect size
res_eff_size_pfc_sex <- ARG %>%
  group_by(Treatment) %>%
  cohens_d(PFC ~ Sex, paired = F, hedges.correction = T)
res_eff_size_pfc_sex

# save treatment effect size
res_eff_size_pfc_sex <- res_eff_size_pfc_sex[ , !(names(res_eff_size_pfc_sex) %in% ".y.")]
res_eff_size_pfc_sex <- relocate(res_eff_size_pfc_sex,"Treatment")
write.csv(res_eff_size_pfc_sex,"res_pfc_eff_size_sex.csv")