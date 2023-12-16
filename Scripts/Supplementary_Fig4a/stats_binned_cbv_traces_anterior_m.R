
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)

peak_values <- read_csv('../data/binned_cbv_traces_table_ketamine_anterior.csv')

# tidy dataset
peak_values <- peak_values %>% 
  pivot_longer("-840":"2760", names_to = "Time", values_to = "CBV")
peak_values$Time <- as.numeric(peak_values$Time)

peak_values_m <- peak_values[ which(peak_values$Sex == "M"),]
peak_values_m <- peak_values_m[ which(peak_values_m$Time >= -360),]

# Pairwise comparisons between Treatments
res_pwc_treatment_m <- peak_values_m %>%
  group_by(Time,Region) %>%
  pairwise_t_test(CBV ~ Treatment, p.adjust.method = "fdr", paired = T)

# save pwc treatment results
res_pwc_treatment_m <- res_pwc_treatment_m[ , !(names(res_pwc_treatment_m) %in% ".y.")]
write.csv(res_pwc_treatment_m,"res_binned_cbv_pwc_treatment_m.csv")


