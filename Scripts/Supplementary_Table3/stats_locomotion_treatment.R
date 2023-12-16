
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)

locomotion <- read_csv('../data/locomotion_table.csv')

# tidy dataset
locomotion <- locomotion %>% 
  pivot_longer('1':'6', names_to = "Session", values_to = "Score")

#####################################################
# normalize locomotion to mean of habituation (Session 1 and 2)

# find unique values in ID column
aa <- unique(locomotion$ID)

avghab <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(avghab) <- c('ID', 'Sex', 'Score')

for(i in 1:length(aa)) {
  idx <- which(locomotion$ID == aa[i])
  tmp <- locomotion[idx,]
  tmp2 <- tmp[ which(tmp$Session < 3),]
  
  avghab[i,]$Score <- mean(tmp2$Score)/1e3
  avghab[i,]$ID <- aa[i]
  avghab[i,]$Sex <- locomotion[idx[1],]$Sex
  
  locomotion[idx, ]$Score <- tmp$Score/mean(tmp2$Score)
}
#####################################################

locomotion <- locomotion %>%
  convert_as_factor(ID, Session, Sex, Group)

# two-way mixed anova at each level of Treatment
res_aov2_treatment <- locomotion %>%
  group_by(Group) %>%
  anova_test(dv = Score, wid = ID,  between = Sex, within = Session)
res_aov2_treatment <- get_anova_table(res_aov2_treatment)

# write anova results to .txt file
write.csv(res_aov2_treatment, "res_anova_twoway_treatment.csv", row.names=FALSE)
