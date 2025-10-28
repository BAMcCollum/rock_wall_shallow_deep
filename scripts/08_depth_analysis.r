#' -----------------------------------------------
#' This script looks at change over time in the distribution
#' of species across depths
#' -----------------------------------------------

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)
library(car)
library(gt)
library(emmeans)

# load data and set themes
source("scripts/load_data_settings.r")

##
# Show depth-cover distribution for each species
# For when all depths sampled
##

# first, make sure all depths are sampled in a year
common_sampling_subsites <- subsite_substrate_long |>
  # make sure years have something at each depth
  group_by(year) |>
  mutate(num_sites = n_distinct(site),
         has_deep = "Deep" %in% depth) |>
  ungroup() |>
  filter(num_sites >=2 & has_deep) |>
  # make decades
  mutate(decade = cut(year, breaks = c(1988, 2000, 2010, 2024)),
         decade = factor(decade,
                         labels = c("1989-2000", "2001-2010", "2011-2023"))) 
  

# output plot of depth distribution for each species
for(one_sp in unique(common_sampling_subsites$species)){
  common_sampling_subsites |>
    filter(species == one_sp) |>
    group_by(year, average_depth) |>
    summarize(proportion = mean(proportion)) |>
    ggplot(aes(y = average_depth, x = year)) +
    geom_tile(aes(fill = proportion*100)) +
    scale_fill_viridis_c(
    #  transform = scales::transform_pseudo_log(base = 10), #na.value=1,
     #                    labels = scales::label_percent(accuracy = 0.01),
      ) +
    scale_y_continuous(transform = "reverse") +
    labs(title = one_sp, y = "Depth(m)", x = "", fill = "% Cover")
  
  ggsave(glue("figures/depth_dist/{one_sp}.jpg"), width = 6, height = 5)  
}


##
# Make decadal data of average proportion
# per depth per species and viz the output
##
decadal_dist <- common_sampling_subsites |>
  group_by(species, decade, average_depth, BO21_tempmax_bdmin_mean) |>
  summarize(proportion = mean(proportion)) |>
  ungroup() 

# output plot of depth distribution for each species
for(one_sp in unique(decadal_dist$species)){
  print(one_sp)
  
  ggplot(decadal_dist |> filter(species == one_sp),
         aes(y = proportion*100,
             x = average_depth,
             color = decade, group = decade)) +
    geom_point() + geom_line() +
    facet_wrap(vars(decade))  +
    coord_flip() +
    labs(x = "Depth (m)", y = "Average Percent Cover", color = "") +
    year_color_scale_discrete() +
    #scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(transform = "reverse") +
    theme_classic(base_size = 14)
  
  ggsave(glue("figures/decadal_depth_dist/{one_sp}.jpg"), width = 6, height = 5)  
}

ggplot(decadal_dist |> filter(!is.na(BO21_tempmax_bdmin_mean)),
       aes(y = proportion*100,
           x = average_depth,
           color = BO21_tempmax_bdmin_mean, group = species)) +
  geom_point(size=1.5) + geom_line(linewidth=1) +
  facet_grid(cols = vars(decade), 
             rows = vars(cut_number(BO21_tempmax_bdmin_mean, 3)),
             scale = "free_x")  +
  coord_flip() +
  labs(x = "Depth (m)", y = "Average Percent Cover", color = "Thermal\nPreference") +
 # year_color_scale_discrete() +
  #scale_y_continuous(labels = scales::percent) +
  scale_color_distiller(palette="RdYlBu") +
  scale_x_continuous(transform = "reverse") +
  theme_classic(base_size = 14)

ggsave("figures/depth_by_decade_all.jpg", width = 8, height = 6)  


##
# Make decadal data of central depth
# for each species and viz
##

# make the decadal data for central depth
central_depth_decadal <- common_sampling_subsites |>
  group_by(species, decade, year, BO21_tempmax_bdmin_mean) |>
  summarize(central_depth = sum((average_depth*proportion)/sum(proportion))) |>
  group_by(species, decade, BO21_tempmax_bdmin_mean) |>
  summarize(central_depth = mean(central_depth, na.rm = TRUE))

# for the labels
central_depth_decadal_last <- filter(central_depth_decadal, decade == "2011-2023")

# plot
ggplot(central_depth_decadal |> filter(!is.na(BO21_tempmax_bdmin_mean)),
       aes(x = decade, y = central_depth, 
           color = BO21_tempmax_bdmin_mean, group = species)) +
  geom_point() + geom_line() +
  scale_y_continuous(transform = "reverse") +
  labs(color = "Thermal\nPreference", x = "",
       y = "Central Depth (m)") +
  scale_color_distiller(palette="RdYlBu") +
  ggrepel::geom_text_repel(data = central_depth_decadal_last |> filter(!is.na(BO21_tempmax_bdmin_mean)),
            x = 3.03, color = "black", hjust = -0.3, direction = "y", 
            mapping = aes(y = central_depth, label = species),
            seed = 31415) +
  scale_x_discrete(expand = expansion(add = c(0.3, 1.4)))

ggsave("figures/central_depth_by_decade_thermal.jpg", width = 7, height = 4)  


ggplot(central_depth_decadal,
       aes(x = decade, y = central_depth, group = species)) +
  geom_point() + geom_line() +
  scale_y_continuous(transform = "reverse") +
  labs(y = "Central Depth (m)") 

ggsave("figures/central_depth_by_decade_all.jpg", width = 7, height = 4)  


##
# Viz change in central depth
# for all species by year 
##

central_depth_annual <- common_sampling_subsites |>
  group_by(species, year, BO21_tempmax_bdmin_mean) |>
  summarize(central_depth = sum((average_depth*proportion)/sum(proportion))) 

# all
ggplot(central_depth_annual,
       aes(x = year, y = central_depth, group = species)) +
  geom_point() + geom_line() +
  scale_y_continuous(transform = "reverse") +
  labs(y = "Central Depth (m)") 

# that was a mess - let's see if adding temp helps
ggplot(central_depth_annual |> filter(!is.na(BO21_tempmax_bdmin_mean)),
       aes(x = year, y = central_depth, 
           color = BO21_tempmax_bdmin_mean, group = species)) +
  geom_point() + geom_line() +
  scale_y_continuous(transform = "reverse") +
  labs(color = "Thermal\nPreference", x = "",
       y = "Central Depth (m)") +
  scale_color_distiller(palette="RdYlBu") +
  facet_wrap(vars(cut_number(BO21_tempmax_bdmin_mean, 3)))

ggsave("figures/central_depth_by_year_thermal.jpg", width = 10, height = 4)  


##
# Model influence of thermal preference on change in central depth
##

depth_thermal_decadal_mod <- 
  glmmTMB::glmmTMB(central_depth ~ decade*BO21_tempmax_bdmin_mean + 
                     (decade|species),
     data = central_depth_decadal)

# check assumptions
performance::check_model(depth_thermal_decadal_mod)

# omnibus test
Anova(depth_thermal_decadal_mod)
performance::r2(depth_thermal_decadal_mod)

# posthoc
emtrends(depth_thermal_decadal_mod, 
        ~decade, 
        "BO21_tempmax_bdmin_mean") |> plot() +
  geom_vline(xintercept = 0, lty = 2)


emtrends(depth_thermal_decadal_mod, 
         ~decade, 
         "BO21_tempmax_bdmin_mean") |> 
  contrast(method = "pairwise", adjust = "none")
  
## Viz
modelbased::estimate_expectation(depth_thermal_decadal_mod, 
                                 by = c("BO21_tempmax_bdmin_mean", 
                                        "decade")) |> 
  plot(show_data = TRUE) +
  scale_y_continuous(transform = "reverse") +
  labs(y = "Central Depth (m)", x = "Thermal Preference",
       color = "", fill = "") +
  facet_wrap(vars(decade))

ggsave("figures/central_depth_model.jpg", width = 8, height = 4)


##
# GAM by year?
# ##
# 
# 
# depth_thermal_annual_mod <- 
#   mgcv::gam(central_depth ~ t2(year, BO21_tempmax_bdmin_mean),
#                    data = central_depth_annual)
# 
# 
# ## Viz
# modelbased::estimate_relation(depth_thermal_annual_mod, 
#                                  by = c("year", 
#                                         "BO21_tempmax_bdmin_mean")) |> 
#   plot(show_data = TRUE) +
#   labs(color = "", fill= "") 
# 
# 
# modelbased::estimate_relation(depth_thermal_annual_mod, 
#                               by = c("BO21_tempmax_bdmin_mean",
#                                      "year")) |> 
#   plot(show_data = TRUE) +
#   labs(color = "", fill= "")+
#   scale_y_continuous(transform = "reverse") +
#   labs(y = "Central Depth (m)", x = "Thermal Preference",
#        color = "", fill = "") #+
#   year_color_scale() +
#   year_fill_scale()
