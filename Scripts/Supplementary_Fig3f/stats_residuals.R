
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)

##############
# 10 mg/kg

# ecog residuals
res_ecog <- read_csv('../data/residuals_ecog_table_10mgkg.csv')

# tidy dataset
res_ecog <- res_ecog %>% 
  pivot_longer(deltatheta:gamma, names_to = "Data", values_to = "Res") 

res_ecog <- res_ecog %>%
  convert_as_factor(Data)

# cbv residuals
res_cbv <- read_csv('../data/residuals_fusi_table_10mgkg.csv')

# tidy dataset
res_cbv <- res_cbv %>% 
  pivot_longer(Cg1, names_to = "Data", values_to = "Res") 

res_cbv <- res_cbv %>%
  convert_as_factor(Data)

res_all_10 <- bind_rows(res_cbv,res_ecog)

# Pairwise comparisons - beta1
res_pwc_residuals_10 <- res_all_10 %>%
  pairwise_t_test(Res ~ Data,
                  comparisons = list(c("Cg1","alpha"),c("Cg1","beta"),
                                     c("Cg1","gamma"),c("Cg1","deltatheta")),
                  p.adjust.method = "fdr", paired = F)
res_pwc_residuals_10

# save pwc treatment results
res_pwc_residuals_10 <- res_pwc_residuals_10[ , !(names(res_pwc_residuals_10) %in% ".y.")]
write.csv(res_pwc_residuals_10,"res_pwc_residuals_10mgkg.csv")

##############
# 1 mg/kg

# ecog residuals
res_ecog <- read_csv('../data/residuals_ecog_table_1mgkg.csv')

# tidy dataset
res_ecog <- res_ecog %>% 
  pivot_longer(deltatheta:gamma, names_to = "Data", values_to = "Res") 

res_ecog <- res_ecog %>%
  convert_as_factor(Data)

# cbv residuals
res_cbv <- read_csv('../data/residuals_fusi_table_1mgkg.csv')

# tidy dataset
res_cbv <- res_cbv %>% 
  pivot_longer(Cg1, names_to = "Data", values_to = "Res") 

res_cbv <- res_cbv %>%
  convert_as_factor(Data)

res_all_1 <- bind_rows(res_cbv,res_ecog)

# Pairwise comparisons - beta1
res_pwc_residuals_1 <- res_all_1 %>%
  pairwise_t_test(Res ~ Data,
                  comparisons = list(c("Cg1","alpha"),c("Cg1","beta"),
                                     c("Cg1","gamma"),c("Cg1","deltatheta")),
                  p.adjust.method = "fdr", paired = F)
res_pwc_residuals_1

# save pwc treatment results
res_pwc_residuals_1 <- res_pwc_residuals_1[ , !(names(res_pwc_residuals_1) %in% ".y.")]
write.csv(res_pwc_residuals_1,"res_pwc_residuals_1mgkg.csv")
