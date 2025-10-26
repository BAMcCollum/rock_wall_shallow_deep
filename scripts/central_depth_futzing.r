#' -----------------------------------------------
#' This script looks at change over time in the distribution
#' of species across depths
#' -----------------------------------------------

library(readr)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(performance)
library(car)
library(broom)
library(broom.mixed)
library(gt)
library(ordbetareg)
library(brms)

# load data and set themes
source("scripts/load_data_settings.r")

.x <- subsite_substrate_long |>
  # make sure years have something at each depth
  group_by(year) |>
  mutate(num_sites = n_distinct(site)) |>
  ungroup() |>
  filter(num_sites ==3) |>
  # filter to a species
  filter(species == "Ectopleura spp.") |>
  group_by(year) |>
  mutate(central_depth = sum((average_depth*proportion)/sum(proportion))) |>
  ungroup() |>
  # make decadal curves
  mutate(decade = cut(year, breaks = c(1988, 2000, 2010, 2024)),
         decade = factor(decade,
                         labels = c("1989-2003", "2004-2010", "2011-2023"))) |>
  group_by(decade) |>
  mutate(central_depth = mean(central_depth, na.rm = TRUE)) |>
  group_by(decade, average_depth) |>
  summarize(proportion = mean(proportion),
            central_depth = mean(central_depth, na.rm = TRUE)) |>
  ungroup() 

ggplot(.x,
       aes(y = proportion,
           x = average_depth,
           color = decade, group = decade)) +
  geom_point() + geom_line() +
  geom_point(aes(x = central_depth), y = 0, color = "black") +
  facet_wrap(vars(decade))  +
  coord_flip() +
  labs(x = "Depth (m)", y = "Average Percent Cover", color = "") +
  year_color_scale_discrete() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(transform = "reverse") +
  theme_classic(base_size = 14)



### GAM
.x <- subsite_substrate_long |>
  # make sure years have something at each depth
  group_by(year) |>
  mutate(num_sites = n_distinct(site)) |>
  ungroup() |>
  filter(num_sites ==3) |>
  # filter to a species
  filter(species == "Ectopleura spp.") 

mod <- ordbetareg(proportion ~ t2(year,average_depth), data = .x)

pred_dat <- crossing(average_depth = seq(7,26, length.out=100),
                     year = seq(1989, 2023))

fit_dat <- broom.mixed::augment(mod, newdata = pred_dat,
                   conf.int = TRUE,
                   effects = "fixed")

fit_avg_depth <- fit_dat |>
  group_by(year) |>
  summarize(central_depth = sum((average_depth*.fitted)/sum(.fitted)))

ggplot(fit_dat,
       aes(y = average_depth, x = year)) +
  geom_raster(aes(fill = .fitted*100), 
              interpolate = TRUE) +
  scale_fill_viridis_c() +
  scale_y_continuous(transform = "reverse") +
  labs(title = "Ectopleura", y = "Depth(m)", x = "", fill = "% Cover") +
  geom_point(data = fit_avg_depth, aes(y = central_depth))


# show raw data
.x |>
  group_by(year, average_depth) |>
  summarize(proportion = mean(proportion)) |>
  ggplot(aes(y = average_depth, x = year)) +
  geom_raster(aes(fill = proportion*100), 
              interpolate = FALSE) +
  scale_fill_viridis_c() +
  scale_y_continuous(transform = "reverse") +
  labs(title = "Ectopleura", y = "Depth(m)", x = "", fill = "% Cover")# +
  geom_point(data = fit_avg_depth, aes(y = central_depth))

