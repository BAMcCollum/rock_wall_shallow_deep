#' -----------------------------------------------
#' Evaluate change in CTI by species presence
#' of communities using individual image data
#' -----------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(performance)
library(car)
library(broom)
library(ggrepel)
library(gt)
library(glmmTMB)
library(emmeans)

# load data and set themes
source("scripts/load_data_settings.r")

##
# cover dist
##
subsite_data_thermal |> filter(!is.na(gen_spp)) |>
  group_by(site, year, subsite, depth) |>
  summarize(cover = sum(proportion)) |>
  ggplot(aes(x = cover)) +
  geom_histogram() +
  labs(x = "Sum of Proportional Cover", y = "Count") +
  facet_wrap(vars(forcats::fct_rev(depth)))

ggsave("figures/thermal_identified_cover_histogram.jpg", width = 8, height = 4)


##
# create joined data for analysis 
##
# 
# substrate_long_thermal <- left_join(substrate_long, thermal_data) |>
#   filter(!is.na(gen_spp)) 
# 
# # get thermal data at plot scale averaging over all species present
# substrate_thermal_summary <- substrate_long_thermal |>
#   group_by(site, subsite, depth, year, image) |>
#   filter(proportion>0) |>
#   summarize(cti_presence = mean(BO21_tempmax_bdmin_mean))

subsite_thermal_summary <- subsite_substrate_long |>
  group_by(site, subsite, depth, year, year_cent) |>
  filter(proportion>0) |>
  filter(!is.na(gen_spp)) |>
  summarize(cti_presence = mean(BO21_tempmax_bdmin_mean),
            cti_weighted = weighted.mean(BO21_tempmax_bdmin_mean, w = proportion))

ggplot(subsite_thermal_summary,
       aes(x = year, group = year, y = cti_presence, fill = depth)) +
  geom_boxplot(position = "dodge") +
  depth_fill_scale() +
  facet_wrap(vars(forcats::fct_rev(depth)), ncol = 1) +
  labs(y = "Average Thermal Maxima of\n Species Pool (°C)",
       x = "") +
  guides(fill = "none")

ggsave("figures/cti_by_depth_boxplot.jpg", width = 8, height = 9)

##
# Model
##

mod_cti <- glmmTMB(cti_presence ~ year_cent*depth + 
                     (1|subsite),
                   data = subsite_thermal_summary)
##
# check assumptions
##
check_model(mod_cti)

##
# Omnibus
##
Anova(mod_cti)
Anova(mod_cti)

##
# Compare slopes
##

# viz
emtrends(mod_cti, ~depth, "year_cent") |>
  contrast(method = "pairwise", adjust = "none") |> 
  plot() +
  geom_vline(xintercept = 0, lty = 2) +
  labs(subtitle = "Year trend comparisons in CTI model", y ="")

ggsave("figures/cti_slope_comparison.jpg")


# write out table
emtrends(mod_cti, ~depth, "year_cent")|>
  contrast(method = "pairwise", adjust = "none") |> 
  tidy() |>
  select(-term, -null.value) |>
  mutate(across(where(is.numeric), round, 3)) |>
  gt::gt()|>
  gtsave("tables/cti_slope_comparison.docx")

##
# Show fit model
##

modelbased::estimate_relation(mod_cti,
                              by = c("year_cent",
                                     "depth")) |> 
  plot(
    show_data = TRUE,
    point = list(alpha = 1, size = 4),
    line = list(size = 1.5)) +
  scale_x_continuous(labels = function(x) x + mean_year) +
  depth_color_scale() +
  depth_fill_scale()  +
  facet_wrap(vars(forcats::fct_rev(depth)), ncol = 1)+
  labs(x = "",  
       y = "Average Thermal Maxima of\n Species Pool (°C)") +
  guides(color = "none", fill = "none") +
  theme_classic(base_size = 18)

ggsave("figures/cti_by_depth.jpg", width = 8, height = 9)

