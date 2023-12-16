
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)

# load data
peak_values <- read_csv('../data/peak_cbv_table_ketamine_anterior_gonadectomized.csv')

# # exclude regions to match analysis in non-orchiectomized animals
# drops <- c("PrL","IL","S1","Ins")
# peak_values <- peak_values[ , !(names(peak_values) %in% drops)]

# tidy dataset
peak_values <- peak_values %>% 
  pivot_longer(Cg1:NAcSh, names_to = "Region", values_to = "Peak")

peak_values <- peak_values %>%
  convert_as_factor(Treatment, Region)

# peak_values <- subset (peak_values, select = -c(Sex))

# Two-way mixed anova
res_aov2 <- anova_test(
  data = peak_values, dv = Peak, wid = Animal_ID,
  within = c(Treatment,Region)
)
res_aov2 <- get_anova_table(res_aov2)

# write anova results to .txt file
write.csv(res_aov2, "res_peak_anova_twoway.csv", row.names=FALSE)

# Pairwise comparisons between Treatment levels
res_pwc_treatment <- peak_values %>%
  group_by(Region) %>%
  pairwise_t_test(Peak ~ Treatment,
                  p.adjust.method = "none", paired = T)
res_pwc_treatment$p.adj <- p.adjust(res_pwc_treatment$p, method = "fdr")
res_pwc_treatment <- add_significance(res_pwc_treatment,"p.adj")

# save pwc treatment results
res_pwc_treatment <- res_pwc_treatment[ , !(names(res_pwc_treatment) %in% ".y.")]
write.csv(res_pwc_treatment,"res_peak_pwc_treatment.csv")

# Treatment effect size
res_eff_size_treatment <-peak_values %>% 
  group_by(Region) %>%
  cohens_d(Peak ~ Treatment, paired = T, hedges.correction = T)
res_eff_size_treatment

# save treatment effect size
res_eff_size_treatment <- res_eff_size_treatment[ , !(names(res_eff_size_treatment) %in% ".y.")]
res_eff_size_treatment <- relocate(res_eff_size_treatment,"Region")
write.csv(res_eff_size_treatment,"res_peak_eff_size_treatment.csv")

# Bar plot
bp1 <- ggbarplot(
  peak_values, x = "Region", y = "Peak", add = "mean_se",
  color = "Treatment", palette = "jco",
  position = position_dodge(0.8)
)
bp1

# Add p-values onto the bar plots
res_pwc_treatment <- res_pwc_treatment %>%
  add_xy_position(fun = "mean_sd", x = "Region", dodge = 0.8)
bp1 + stat_pvalue_manual(
  res_pwc_treatment,  label = "p.adj.signif", tip.length = 0.01
)
