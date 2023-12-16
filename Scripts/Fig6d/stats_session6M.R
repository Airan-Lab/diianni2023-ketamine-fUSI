
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)

locomotion <- read_csv('../data/locomotion_binned_distance_session6.csv')

locomotion <- locomotion[ which(locomotion$Sex == "M"),]

# tidy dataset
locomotion <- locomotion %>% 
  pivot_longer('1':'19', names_to = "Time", values_to = "Score")

# Pairwise comparisons between Treatment levels
res_pwc_treatment <- locomotion %>%
  group_by(Time) %>%
  pairwise_t_test(Score ~ Group, comparisons = list(c("VEH+KET","NTX+KET"), c("VEH+KET","VEH+VEH")),
                p.adjust.method = "fdr", paired = F)
res_pwc_treatment

# order items in res_pwc_treatment for ascending time
res_pwc_treatment$Time <- as.numeric(res_pwc_treatment$Time)
res_pwc_treatment <- arrange(res_pwc_treatment,res_pwc_treatment$Time)

# save pwc treatment results
res_pwc_treatment <- res_pwc_treatment[ , !(names(res_pwc_treatment) %in% ".y.")]
write.csv(res_pwc_treatment,"res_pwc_treatment_binned_6M.csv")
