# Repeated measures ANOVA to see if there is a difference in change of 
# central depth between decades

library(tidyverse)
library(ggpubr)
library(rstatix)

cdd <- read_csv("data/central_depth_decadal_noNA.csv")
View(cdd)

#Get summary stats

cdd |>
  group_by(decade) |>
  get_summary_stats(central_depth, type = "mean_sd")

#visualize

cdd_bxp <- ggboxplot(cdd, x = "decade", y = "central_depth", add = "point")

#check for outliers

cdd|>
  group_by(decade)|>
  identify_outliers(central_depth)

#The normality assumption can be checked by computing Shapiro-Wilk test 
#for each time point. If the data is normally distributed, the p-value 
#should be greater than 0.05.

cdd|>
  group_by(decade)|>
  shapiro_test(central_depth)

#QQ plot draws the correlation between a given data and the normal distribution. 
#Create QQ plots for each time point:

ggqqplot(cdd, "central_depth", facet.by = "decade")

#Assumption of sphericity

res.aov_cdd <- anova_test(data = cdd, dv = central_depth, wid = species, within = decade)
get_anova_table(res.aov_cdd)


#pairwise comparisons (post hoc test)

pwc_cdd <- cdd %>%
  pairwise_t_test(
    central_depth ~ decade, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc_cdd

# Visualization: box plots with p-values

pwc_cdd <- pwc_cdd %>% add_xy_position(x = "decade")
cdd_bxp + 
  stat_pvalue_manual(pwc_cdd) +
  labs(
    subtitle = get_test_label(res.aov_cdd, detailed = TRUE),
    caption = get_pwc_label(pwc_cdd)
  )
