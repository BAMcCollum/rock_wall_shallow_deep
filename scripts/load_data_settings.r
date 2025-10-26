library(readr)
library(ggplot2)

## 
# load data
##

# quad level data
substrate_long <- read_csv("data/substrate_data_long.csv")

#subsite level data
subsite_substrate_long <- read_csv("data/substrate_data_subsite_long.csv")

# for later use
mean_year <- mean(seq(min(substrate_long$year), max(substrate_long$year), by = 1))

# thermal data
thermal_data <- read_csv("data/Occurrence_based_species_thermal_indicies_Photos_100825.csv") |>
  select(gen_spp, species_id, n_obis_rec,
         BO21_tempmax_bdmean_mean, BO21_tempmax_bdmin_mean, BO21_tempmax_bdmin_max,
         BO21_tempmax_bdmean_q5, BO21_tempmax_bdmean_q95)

##
# set visual themes
# and scales
##
theme_set(theme_classic(base_size = 18))

depth_color_scale <- function(...) scale_color_manual(values = c("darkorchid4", "sienna1", "chartreuse4"), ...)
depth_fill_scale <- function(...) scale_fill_manual(values = c("darkorchid4", "sienna1", "chartreuse4"), ...)


year_color_scale <- function(...){
  pal <- wesanderson::wes_palette("Zissou1", 50, type = "continuous")
  scale_color_gradientn(colors = rev(pal),
                        expand = expansion(mult = 0, add = 0),
                        ...)
}


year_color_scale_discrete <- function(...){
  pal <- wesanderson::wes_palette("Zissou1", 3, type = "continuous")
  scale_color_manual(values = rev(pal),
                        #expand = expansion(mult = 0, add = 0),
                        ...)
}
