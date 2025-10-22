#' -----------------------------------------------
#' This script fits and evaluates a model looking
#' at species thermal tolerance and change and
#' saves the model out for visualization
#' -----------------------------------------------

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(performance)
library(car)
library(broom)
library(ggrepel)
library(gt)

# load data and set themes
source("scripts/load_data_settings.r")


coefs_long <- read_csv("data/change_coefficients_ordbetareg.csv")
#View (coefs_with_indicies)

##
# initial check
##
ggplot(coefs_long, aes(x = BO21_tempmax_bdmin_mean, 
                       y = year_cent.trend,
                       color = depth))+
  geom_point()+
  depth_color_scale() +
  stat_smooth(method = "lm") #Ideally would like to color by functional group, depth strata

##
# fit the model
##
mod_bdmean_min <- 
  lm(year_cent.trend ~ depth*BO21_tempmax_bdmin_mean, 
           data = coefs_long)


##
# check model assumptions
##
check_model(mod_bdmean_min)
# looks good!

##
# Omnibus test
##
Anova(mod_bdmean_min)

##
# difference in slopes
##
emtrends(mod_bdmean_min, 
         specs =~ depth,
         var = "BO21_tempmax_bdmin_mean") |>
  contrast(method = "pairwise",
           p.adjust = "none") |>
  tidy() |>
  select(-term, -null.value) |>
  mutate(across(where(is.numeric), round, 3)) |>
  gt::gt()|>
  gtsave("tables/depth_temp_slope_comparison.docx")

#make a better looking table
gt::gt(Anova(mod_bdmean_min) |> round(3)) |>
  gtsave("tables/change_by_depth.docx")
  



##
# Visualize
##
modelbased::estimate_relation(mod_bdmean_min,
                              by = c("BO21_tempmax_bdmin_mean",
                                     "depth")) |> 
  plot(#ribbon = "none",
       show_data = TRUE,
       point = list(alpha = 1, size = 2)) +
  depth_color_scale() +
  depth_fill_scale()  +
  geom_hline(yintercept=0.0, linetype='dashed', 
             linewidth = 0.5, colour='black') +
  geom_text_repel(data = coefs_long,
                  mapping = aes(label = gen_spp, group = gen_spp,
                                x = BO21_tempmax_bdmin_mean, y = year_cent.trend),
                  size = 6, fontface = "italic", color = "black",
                  max.overlaps = 30, seed = 31415) +
  facet_wrap(vars(forcats::fct_rev(depth)), ncol = 1)+
  labs(x = "Average Thermal Maxima\n(Occupancy derived max temp at species min depth) in (°C)",  
       y = "Coefficient of Change") +
  guides(color = "none", fill = "none") +
  theme_classic(base_size = 18)
  

ggsave("figures/coefs_with_indicies.jpg", width = 8, height = 9)
