
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)

peak_values <- read_csv('../data/binned_cbv_traces_table_dose_response.csv')

# tidy dataset
peak_values <- peak_values %>% 
  pivot_longer("-600":"2760", names_to = "Time", values_to = "CBV")
peak_values$Time <- as.numeric(peak_values$Time)

peak_values <- peak_values %>%
  convert_as_factor(Dose, Region, Time)

# Pairwise comparisons between Doses
res_pwc_dose <- peak_values %>%
  group_by(Time,Region) %>%
  pairwise_t_test(CBV ~ Dose, p.adjust.method = "fdr", paired = F)
res_pwc_dose

# save pwc treatment results
res_pwc_dose <- res_pwc_dose[ , !(names(res_pwc_dose) %in% ".y.")]
write.csv(res_pwc_dose,"res_binned_cbv_pwc_dose.csv")


