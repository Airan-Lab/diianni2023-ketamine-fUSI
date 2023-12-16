
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)

##############
# ECOG DATASET
beta_ecog <- read_csv('../data/beta_ecog_table.csv')

# tidy dataset
beta_ecog2 <- beta_ecog %>% 
  pivot_longer(deltatheta:gamma, names_to = "Data", values_to = "Beta") 

beta_ecog2 <- beta_ecog2 %>%
  convert_as_factor(Data)
##############

##############
# CBV DATASET
beta_cbv <- read_csv('../data/beta_fusi_table.csv')

# tidy dataset
beta_cbv2 <- beta_cbv %>% 
  pivot_longer(Cg1, names_to = "Data", values_to = "Beta") 

beta_ecog2 <- beta_ecog2 %>%
  convert_as_factor(Data)
##############

beta_all <- bind_rows(beta_cbv2,beta_ecog2)

beta1 <- beta_all[ which(beta_all$Beta_Val == 'b1'),]
beta2 <- beta_all[ which(beta_all$Beta_Val == 'b2'),]
beta4 <- beta_all[ which(beta_all$Beta_Val == 'b4'),]

# Pairwise comparisons - beta1
res_pwc_beta1 <- beta1 %>%
  pairwise_t_test(Beta ~ Data,
                  comparisons = list(c("Cg1","alpha"),c("Cg1","beta"),
                                     c("Cg1","gamma"),c("Cg1","deltatheta")),
                  p.adjust.method = "fdr", paired = F)
res_pwc_beta1

# save pwc treatment results
res_pwc_beta1 <- res_pwc_beta1[ , !(names(res_pwc_beta1) %in% ".y.")]
write.csv(res_pwc_beta1,"res_pwc_beta1.csv")

# Pairwise comparisons - beta2
res_pwc_beta2 <- beta2 %>%
  pairwise_t_test(Beta ~ Data,
                  comparisons = list(c("Cg1","alpha"),c("Cg1","beta"),
                                     c("Cg1","gamma"),c("Cg1","deltatheta")),
                  p.adjust.method = "fdr", paired = F)
res_pwc_beta2

# save pwc treatment results
res_pwc_beta2 <- res_pwc_beta2[ , !(names(res_pwc_beta2) %in% ".y.")]
write.csv(res_pwc_beta2,"res_pwc_beta2.csv")

# Pairwise comparisons - beta4
res_pwc_beta4 <- beta4 %>%
  pairwise_t_test(Beta ~ Data,
                  comparisons = list(c("Cg1","alpha"),c("Cg1","beta"),
                                     c("Cg1","gamma"),c("Cg1","deltatheta")),
                  p.adjust.method = "fdr", paired = F)
res_pwc_beta4

# save pwc treatment results
res_pwc_beta4 <- res_pwc_beta4[ , !(names(res_pwc_beta4) %in% ".y.")]
write.csv(res_pwc_beta4,"res_pwc_beta4.csv")