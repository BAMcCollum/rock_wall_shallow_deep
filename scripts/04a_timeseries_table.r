#' -----------------------------------------------
#' This script creates a table of coefficients
#' and summary information about each timeseries model
#' -----------------------------------------------

library(brms)
library(broom.mixed)
library(dplyr)
library(gt)

# get models and matching species names
species <- list.files("models/abundance_models/",
                      pattern = "\\.rds") |> #use patern to only have models
  gsub("\\.rds", "", x=_)

models <- list.files("models/abundance_models",
                     pattern = "\\.rds",
                     full.names = TRUE) 

# function to get coefficient and r2 from each model
get_mod_info <- function(mod){
  mod <- readRDS(mod)
  coef_tab <- suppressWarnings(tidy(mod)) |>
    select(-effect, -component, -group)
  fit <-  rstantools::bayes_R2(mod, re.form = NA) |> 
    as_tibble() 
  
  coef_tab |>
    mutate(r2 = c(fit$Estimate, rep(NA, 6))) |>
    select(term, estimate, std.error, conf.low, conf.high, r2) |>
    mutate(across(where(is.numeric), round, 3))
}

# iterate over models
mod_info <- purrr::map2(species, models, 
                        ~get_mod_info(.y) |>
                          mutate(species = c(.x)))

# write it all out
mod_info |>
  purrr::list_rbind() |>
  mutate(sp_fit = paste0(species, ", R2 = ", r2)) |>
  select(-species, -r2) |>
  group_by(sp_fit) |>
  gt(rowname_col = "term") |>
  tab_header(
    title = "Coefficients for Time Series Models of Change by Depth",
    subtitle = "Ordered Beta Regression where proportion ~ centered year * depth + (1|Subsite)"
  ) |>

  readr::write_csv("tables/timeseries_coefs_fits.csv")
