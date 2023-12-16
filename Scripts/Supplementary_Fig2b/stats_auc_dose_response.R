
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)

auc_val <- read_csv('../data/auc_cbv_table_dose_response.csv')

# tidy dataset
auc_val <- auc_val %>% 
  pivot_longer(Cg1:NAc, names_to = "Region", values_to = "AUC") 

auc_val <- auc_val %>%
  convert_as_factor(Dose, Region)

# Two-way mixed anova
res_aov2 <- anova_test(
  data = auc_val, dv = AUC, wid = Animal_ID,
  between = Dose, within = c(Region)
)
res_aov2 <- get_anova_table(res_aov2)

# write anova results to file
write.csv(res_aov2, "res_auc_anova_twoway.csv", row.names=FALSE)

# Pairwise comparisons between dose levels - stratified by region
res_pwc_dose <- auc_val %>%
  group_by(Region) %>%
  pairwise_t_test(AUC ~ Dose, 
                  p.adjust.method = "fdr", paired = F)
res_pwc_dose

# save pwc dose results
res_pwc_dose <- res_pwc_dose[ , !(names(res_pwc_dose) %in% ".y.")]
write.csv(res_pwc_dose,"res_auc_pwc_dose.csv")

# Dose effect size
res_eff_size_dose <- auc_val %>% 
  group_by(Region) %>%
  cohens_d(AUC ~ Dose, paired = F, hedges.correction = T)
res_eff_size_dose

# save dose effect size
res_eff_size_dose <- res_eff_size_dose[ , !(names(res_eff_size_dose) %in% ".y.")]
res_eff_size_dose <- relocate(res_eff_size_dose,"Region")
write.csv(res_eff_size_dose,"res_auc_eff_size_dose.csv")

# Bar plot
bp1 <- ggbarplot(
  auc_val, x = "Region", y = "AUC", add = "mean_se",
  color = "Dose", palette = "jco",
  position = position_dodge(0.8)
)
bp1

# Add p-values onto the bar plots
res_pwc_dose <- res_pwc_dose %>%
  add_xy_position(fun = "mean_sd", x = "Region", dodge = 0.8)
bp1 + stat_pvalue_manual(
  res_pwc_dose,  label = "p.adj.signif", tip.length = 0.01
)