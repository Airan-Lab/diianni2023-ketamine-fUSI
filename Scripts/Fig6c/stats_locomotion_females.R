
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(R.matlab)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)

locomotion <- read_csv('../data/locomotion_table.csv')

locomotion <- locomotion[ which(locomotion$Sex == "F"),]

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

  locomotion[idx, ]$Score <- tmp$Score/mean(tmp2$Score)
}
#####################################################

locomotion <- locomotion %>%
  convert_as_factor(ID, Session, Group)

# two-way mixed anova
res_aov2 <- locomotion %>%
  anova_test(dv = Score, wid = ID,  between = Group, within = Session)
res_aov2 <- get_anova_table(res_aov2)

# write anova results to .txt file
write.csv(res_aov2, "res_anova_twoway.csv", row.names=FALSE)

# Pairwise comparisons between Treatment levels
res_pwc_treatment <- locomotion %>%
  group_by(Session) %>%
  pairwise_t_test(Score ~ Group, comparisons = list(c("VEH+KET","NTX+KET"), 
                                                    c("VEH+KET","VEH+VEH")),
                  p.adjust.method = "none", paired = F)
res_pwc_treatment$p.adj <- p.adjust(res_pwc_treatment$p, method = "fdr")
res_pwc_treatment <- add_significance(res_pwc_treatment,"p.adj")

# save pwc treatment results
res_pwc_treatment <- res_pwc_treatment[ , !(names(res_pwc_treatment) %in% ".y.")]
write.csv(res_pwc_treatment,"res_pwc_treatment.csv")

# Treatment effect size
res_eff_size_treatment <- locomotion %>%
  group_by(Session) %>%
  cohens_d(Score ~ Group, comparisons = list(c("VEH+KET","NTX+KET"), 
                                             c("VEH+KET","VEH+VEH")),
           paired = F, hedges.correction = T)
res_eff_size_treatment

# save treatment effect size
res_eff_size_treatment <- res_eff_size_treatment[ , !(names(res_eff_size_treatment) %in% ".y.")]
res_eff_size_treatment <- relocate(res_eff_size_treatment,"Session")
write.csv(res_eff_size_treatment,"res_eff_size_treatment.csv")

# Bar plot
bp1 <- ggbarplot(
  locomotion, x = "Session", y = "Score", add = "mean_se",
  color = "Group", palette = "jco",
  position = position_dodge(0.8)
)
bp1

pwc_treamt2 <- res_pwc_treatment %>% add_xy_position(x = "Session")
bp1 +
  stat_pvalue_manual(pwc_treamt2, tip.length = 0, hide.ns = TRUE) 