#' -----------------------------------------------
#' This script calculates the change coefficient
#' for each species under an ordbeta regression
#' and outputs a file with species names and 
#' coefficients (and SEs) using ordbetareg
#' -----------------------------------------------

library(readr)
library(ordbetareg)
library(visreg)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(broom.mixed)
library(broom)
library(emmeans)


# load the data
source("scripts/load_data_settings.r")

# helper functions
get_fitted_values <- function(mod, adf, 
                              mean_year = mean(seq(min(substrate_long$year), max(substrate_long$year), by = 1))){
  emmeans(mod, 
          specs = ~year_cent*depth,
          epred = TRUE,
          at = list(year_cent = 
                      modelr::seq_range(adf$year_cent, 
                                        200))) |>
    tidy() |>
    mutate(year = year_cent + mean_year)
}

# helper for trends
get_trends_by_depth <- function(mod){
  emtrends(mod, ~depth, "year_cent") |>
    tidy()
}

mods_long <- subsite_substrate_long |>
 # debug, try with only a few species
 # filter(species %in% c("scypha_sp", "botryllus_schlosseri")) |>
  
  # fit a model to each attribute
  group_by(species,
           gen_spp #for later thermal merge
           ) |>
  mutate(sp1 = species) |>
  nest() |>
  mutate(mod = map(data, ~ordbetareg(proportion ~
                                       year_cent*depth + (1|subsite),
                                      data = .x,
                                     file = glue::glue("models/abundance_models/{.x$sp1[1]}.rds"),
                                     file_refit = 
                                       #"always")),
                                       "on_change")), #.x is a placeholder for each nested df
         coef = map(mod, ~tidy(.x) |> filter(term == "year_cent")),
         fitted = map2(mod, data, get_fitted_values))

coefs_long <- mods_long |>  
  mutate(avg_trends = map(mod, get_trends_by_depth)) |>
  # great, get back the data
  unnest(avg_trends) |>
  select(-fitted, -data, -mod, -coef) |>
  arrange(species) |>
  left_join(thermal_data)

View(coefs_long)

write_csv(coefs_long,"data/change_coefficients_ordbetareg.csv")

# What about modeled predictions for figures marginalized
# over site and area?
fitted_long <- mods_long |>  
  arrange(species) |> 
  # great, get back the data
  unnest(fitted) |>
  select(-data, -mod, -coef) |>
  left_join(thermal_data)

write_csv(fitted_long,"data/fitted_long_ordbetareg.csv")
