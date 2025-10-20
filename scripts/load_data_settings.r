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


##
# set visual themes
# and scales
##
theme_set(theme_classic(base_size = 14))

depth_scale <- function(...) scale_color_manual(values = c("darkorchid4", "sienna1", "chartreuse4"), ...)


year_color_scale <- function(...){
  pal <- wesanderson::wes_palette("Zissou1", 50, type = "continuous")
  scale_color_gradientn(colors = rev(pal),
                        expand = expansion(mult = 0, add = 0),
                        ...)
}