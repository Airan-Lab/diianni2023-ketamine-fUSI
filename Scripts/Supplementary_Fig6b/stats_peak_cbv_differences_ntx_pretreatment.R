
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)

# load peak data - anterior slice
peak_values_ant <- read_csv('../data/peak_cbv_differences_naltrexone_anterior.csv')

# exclude regions that are not significant in functional maps
drops <- c("PrL","IL","S1","Ins")
peak_values_ant <- peak_values_ant[ , !(names(peak_values_ant) %in% drops)]

# load peak data - posterior slice
peak_values_post <- read_csv('../data/peak_cbv_differences_naltrexone_posterior.csv')

# exclude regions that are not significant in functional maps
drops <- c("LPtA","MPtA","Hipp","VM","VPM","VPL")
peak_values_post <- peak_values_post[ , !(names(peak_values_post) %in% drops)]

# tidy anterior dataset
peak_values_ant <- peak_values_ant %>% 
  pivot_longer(Cg1:NAcSh, names_to = "Region", values_to = "Peak") 

peak_values_ant <- peak_values_ant %>%
  convert_as_factor(Region)

# tidy posterior dataset
peak_values_post <- peak_values_post %>% 
  pivot_longer(RSG:LPLR, names_to = "Region", values_to = "Peak") 

peak_values_post <- peak_values_post %>%
  convert_as_factor(Region)

peak_values_post$Animal_ID = peak_values_post$Animal_ID+18

# concatenate the two datasets
peak_values <- rbind(peak_values_ant, peak_values_post)

# One-way anova (sex)
res_aov_peak_sex <- anova_test(
  data = peak_values, dv = Peak, wid = Animal_ID,
  between = Sex
)
res_aov_peak_sex <- get_anova_table(res_aov_peak_sex)

# write anova results to .txt file
write.csv(res_aov_peak_sex, "res_peakdiff_anova_oneway_sex.csv", row.names=FALSE)

# Pairwise comparisons
res_pwc_sex <- peak_values %>%
  group_by(Region) %>%
  pairwise_t_test(Peak ~ Sex, p.adjust.method = "none", paired = F)
res_pwc_sex$p.adj <- p.adjust(res_pwc_sex$p, method = "fdr")
res_pwc_sex <- add_significance(res_pwc_sex,"p.adj")

# save pwc results
res_pwc_sex <- res_pwc_sex[ , !(names(res_pwc_sex) %in% ".y.")]
write.csv(res_pwc_sex,"res_peakdiff_pwc_sex.csv")

# effect size
res_eff_size_sex <- peak_values %>% 
  group_by(Region) %>%
  cohens_d(Peak ~ Sex, paired = F, hedges.correction = T)
res_eff_size_sex

# save effect size
res_eff_size_sex <- res_eff_size_sex[ , !(names(res_eff_size_sex) %in% ".y.")]
res_eff_size_sex <- relocate(res_eff_size_sex,"Region")
write.csv(res_eff_size_sex,"res_peakdiff_eff_size_sex.csv")

# Bar plot
bp1 <- ggbarplot(
  peak_values, x = "Region", y = "Peak", add = "mean_se",
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