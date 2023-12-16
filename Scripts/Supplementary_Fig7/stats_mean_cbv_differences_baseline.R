
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)

# load mean cbv baseline anterior
mean_cbv_diff_ant <- read_csv('../data/mean_cbv_differences_baseline_anterior.csv')

# exclude regions that are not significant in functional maps
drops <- c("PrL","IL","S1","Ins")
mean_cbv_diff_ant <- mean_cbv_diff_ant[ , !(names(mean_cbv_diff_ant) %in% drops)]

# load mean cbv baseline posterior
mean_cbv_diff_post <- read_csv('../data/mean_cbv_differences_baseline_posterior.csv')

# exclude regions that are not significant in functional maps
drops <- c("LPtA","MPtA","Hipp","VM","VPM","VPL")
mean_cbv_diff_post <- mean_cbv_diff_post[ , !(names(mean_cbv_diff_post) %in% drops)]

# tidy anterior dataset
mean_cbv_diff_ant <- mean_cbv_diff_ant %>% 
  pivot_longer(Cg1:NAcSh, names_to = "Region", values_to = "MeanCBV") 

mean_cbv_diff_ant <- mean_cbv_diff_ant %>%
  convert_as_factor(Region)

# tidy posterior dataset
mean_cbv_diff_post <- mean_cbv_diff_post %>% 
  pivot_longer(RSG:LPLR, names_to = "Region", values_to = "MeanCBV") 

mean_cbv_diff_post <- mean_cbv_diff_post %>%
  convert_as_factor(Region)

mean_cbv_diff_post$Animal_ID = mean_cbv_diff_post$Animal_ID+18

# concatenate the two datasets
mean_cbv_diff <- rbind(mean_cbv_diff_ant, mean_cbv_diff_post)

# One-way anova - SEX
res_mean_cbv_sex <- mean_cbv_diff %>%
  anova_test(dv = MeanCBV, wid = Animal_ID,
             between = Sex)
res_mean_cbv_sex <- get_anova_table(res_mean_cbv_sex)

# write anova results to .txt file
write.csv(res_mean_cbv_sex, "res_mean_cbv_anova_oneway_sex.csv", row.names=FALSE)

# Pairwise comparisons - SEX
res_pwc_sex <- mean_cbv_diff %>%
  group_by(Region) %>%
  pairwise_t_test(MeanCBV ~ Sex, p.adjust.method = "none", paired = F)
res_pwc_sex$p.adj <- p.adjust(res_pwc_sex$p, method = "fdr")
res_pwc_sex <- add_significance(res_pwc_sex,"p.adj")

# save pwc results
res_pwc_sex <- res_pwc_sex[ , !(names(res_pwc_sex) %in% ".y.")]
write.csv(res_pwc_sex,"res_mean_cbv_pwc_sex.csv")

# effect size - SEX
res_eff_size_sex <- mean_cbv_diff %>%
  group_by(Region) %>%
  cohens_d(MeanCBV ~ Sex, paired = F, hedges.correction = T)
res_eff_size_sex

# save effect size
res_eff_size_sex <- res_eff_size_sex[ , !(names(res_eff_size_sex) %in% ".y.")]
res_eff_size_sex <- relocate(res_eff_size_sex,"Region")
write.csv(res_eff_size_sex,"res_mean_cbv_eff_size_sex.csv")

# Pairwise comparisons - REGION - anterior
res_pwc_region_ant <- mean_cbv_diff_ant %>%
  group_by(Sex) %>%
  pairwise_t_test(MeanCBV ~ Region, p.adjust.method = "none", paired = T)
res_pwc_region_ant$p.adj <- p.adjust(res_pwc_region_ant$p, method = "fdr")
res_pwc_region_ant <- add_significance(res_pwc_region_ant,"p.adj")

# save pwc results
res_pwc_region_ant <- res_pwc_region_ant[ , !(names(res_pwc_region_ant) %in% ".y.")]
write.csv(res_pwc_region_ant,"res_mean_cbv_pwc_region_anterior.csv")

# Pairwise comparisons - REGION - posterior
res_pwc_region_post <- mean_cbv_diff_post %>%
  group_by(Sex) %>%
  pairwise_t_test(MeanCBV ~ Region, p.adjust.method = "none", paired = T)
res_pwc_region_post$p.adj <- p.adjust(res_pwc_region_post$p, method = "fdr")
res_pwc_region_post <- add_significance(res_pwc_region_post,"p.adj")

# save pwc results
res_pwc_region_post <- res_pwc_region_post[ , !(names(res_pwc_region_post) %in% ".y.")]
write.csv(res_pwc_region_post,"res_mean_cbv_pwc_region_posterior.csv")

# Bar plot
bp1 <- ggbarplot(
  mean_cbv_diff, x = "Region", y = "MeanCBV", add = "mean_se",
  color = "Sex", palette = "jco",
  position = position_dodge(0.8)
)
bp1

# Add p-values onto the bar plots
res_pwc_sex <- res_pwc_sex %>%
  add_xy_position(fun = "mean_sd", x = "Region", dodge = 0.8)
bp1 + stat_pvalue_manual(
  res_pwc_sex,  label = "p.adj.signif", tip.length = 0.01
)