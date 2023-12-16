
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)

# ketamine peak cbv table
peak_values_ket <- read_csv('../data/peak_cbv_table_ketamine_anterior.csv')

# exclude NAL+VEH group
peak_values_ket <- peak_values_ket[ -c(which(peak_values_ket$Treatment == "NAL+VEH")), ]

# exclude females
peak_values_ket <- peak_values_ket[ -c(which(peak_values_ket$Sex == "F")), ]

# mk-801 peak cbv table
peak_values_mk801 <- read_csv('../data/peak_cbv_table_mk801_010.csv')

# merge tables
peak_values <- rbind(peak_values_ket, peak_values_mk801)

# regions compared in this plot
keep <- c("Cg1","NAcC")
peak_values <- peak_values[ , (names(peak_values) %in% c("Animal_ID","Treatment","Sex",keep))]

# tidy dataset
peak_values2 <- peak_values %>% 
  pivot_longer(Cg1:NAcC, names_to = "Region", values_to = "Peak") 

peak_values2 <- peak_values2 %>%
  convert_as_factor(Treatment, Region)

comp <- list(c("NAL+KET", "VEH+KET"), c("NAL+MK-801", "VEH+MK-801"))

# Pairwise comparisons
res_pwc_treatment <- peak_values2 %>%
  group_by(Region) %>%
  pairwise_t_test(Peak ~ Treatment,
                  comparisons = list(c("NAL+KET", "VEH+KET"), 
                                     c("NAL+MK801", "VEH+MK801")),
                  p.adjust.method = "none", paired = T)

res_pwc_treatment$p.adj <- p.adjust(res_pwc_treatment$p, method = "fdr")
res_pwc_treatment <- add_significance(res_pwc_treatment,"p.adj")

# save pwc treatment results
res_pwc_treatment <- res_pwc_treatment[ , !(names(res_pwc_treatment) %in% ".y.")]
write.csv(res_pwc_treatment,"res_peak_pwc_treatment.csv")

# Treatment effect size
res_eff_size_treatment <-peak_values2 %>% 
  group_by(Region) %>%
  cohens_d(Peak ~ Treatment, 
           comparisons = list(c("NAL+KET", "VEH+KET"), 
                              c("NAL+MK801", "VEH+MK801")),
           paired = T, hedges.correction = T)
res_eff_size_treatment

# save treatment effect size
res_eff_size_treatment <- res_eff_size_treatment[ , !(names(res_eff_size_treatment) %in% ".y.")]
res_eff_size_treatment <- relocate(res_eff_size_treatment,"Region")
write.csv(res_eff_size_treatment,"res_peak_eff_size_treatment.csv")

# Bar plot
bp1 <- ggbarplot(
  peak_values2, x = "Region", y = "Peak", add = "mean_se",
  color = "Treatment", palette = "jco",
  position = position_dodge(0.8),
)
bp1

# Add p-values onto the bar plots
res_pwc_treatment <- res_pwc_treatment %>%
  add_xy_position(fun = "mean_sd", x = "Region", dodge = 0.8)
bp1 + stat_pvalue_manual(
  res_pwc_treatment,  label = "p.adj.signif", tip.length = 0.01
)
