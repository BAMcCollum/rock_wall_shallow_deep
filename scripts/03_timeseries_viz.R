#' -----------------------------------------------
#' This script creates visualizations of timeseries
#' of species
#' -----------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(glue)

# load data and set themes
source("scripts/load_data_settings.r")

##
# Show what we have data for
##
substrate_long |>
  group_by(site, depth, subsite, year) |>
  summarise(photos = n_distinct(image)) |>
ggplot(aes(x = year, y = paste(site, subsite, sep = "-"),
           fill = photos, color = depth)) +
  geom_tile(size = 0.5) +
  scale_fill_viridis_c() +
  depth_color_scale() +
  labs(fill = "Numer of\nImages", x = "", y = "Subsite", color = "Depth") +
  guides(color = guide_legend(override.aes = list(fill="white")))


ggsave("figures/site_info.jpg",
       width = 10, height = 6)
# 
# ggplot(.x,
#        aes(x = proportion, y = depth,
#            group = paste0(site, subsite),
#            color = year)) +
#   geom_point(position = 
#                ggstance::position_jitterdodgev(jitter.width = 0.01, 
#                                                jitter.height = 0.01,
#                                                dodge.height  = 0.3),
#              alpha = 0.4) +
#   year_color_scale() +
#   labs(subtitle = .x$species[1], color = "", y = "") 

##
# everything!
##
ggplot(subsite_substrate_long,
       aes(x = year, y = proportion,
           color = depth,
           group = paste(site, subsite, class))) +
  geom_line() +
  facet_wrap(vars(forcats::fct_rev(depth)), ncol = 1) +
  depth_color_scale() +
  labs(color = "", x = "")


ggplot(subsite_substrate_long |> filter(!is.na(gen_spp)),
       aes(x = year, y = proportion,
           color = BO21_tempmax_bdmin_mean,
           group = paste(site, subsite, class))) +
  geom_line() +
  facet_wrap(vars(forcats::fct_rev(depth)), ncol = 1) +
  scale_color_distiller(palette = "BrBG") +
  labs(color = "thermal\npreference", x = "")

# plot all species
species_data_as_list <- 
  subsite_substrate_long |>
  group_by(species) |>
  nest() |>
  mutate(data = map(data, ~.x |> mutate(species = species))) |>
  # pull out the data as a list
  pull(data) |>
  # walk through and make plots
  walk(~{
    # what species?
    print(.x$species[1])
    
    sp <- gsub("_", " ", .x$species[1])
    sp <- stringr::str_to_sentence(sp)
    sp <- gsub(" sub$", "", sp)
    y_lab <- glue("Percent Cover of {sp}")
    
    # correct some species names
    if(!is.na(.x$gen_spp[1])) {
      sp <- .x$gen_spp[1]
      y_lab <- glue("Percent Cover of *{sp}*")
    }
    
    
    # timeseries in one panel
    ggplot(.x,
           aes(x = year, y = proportion*100,
               group = paste0(site, subsite),
               color = depth)) +
      geom_point() +
      geom_line()+
      depth_color_scale() +
    labs(x = "Year",
         color = "",
         y = y_lab)+ #sub in species name
      theme(axis.title.y = ggtext::element_markdown())
    

    ggsave(glue("figures/raw_timeseries/{sp}.jpg"),
           width = 10, height = 6)
    
    # timeseries in multiple panels
    ggplot(.x,
           aes(x = year, y = proportion*100,
               group = paste0(site, subsite),
               color = depth)) +
      geom_point() +
      geom_line()+
      depth_color_scale() +
      labs(x = "Year",
           color = "",
           y = y_lab)+ #sub in species name
      theme(axis.title.y = ggtext::element_markdown()) +
      facet_wrap(vars(forcats::fct_rev(depth)), ncol = 1)+
      #labs(x = "") +
      guides(color = "none")
    
    
    ggsave(glue("figures/raw_timeseries_panels/{sp}.jpg"),
           width = 6, height = 10)
    
    # timeseries with vertical lines across depths
    ggplot(.x,
           aes(x = proportion*100, y = depth,
               group = paste0(site, year),
               color = year)) +
      stat_summary(fun.data = "mean_se", geom = "point") + 
      stat_summary(fun.data = "mean_se", geom = "line") + 
      year_color_scale() +
      theme(axis.title.x = ggtext::element_markdown()) +
      labs(y = "",
           color = "Year",
           x = y_lab) #sub in species name
    
    ggsave(glue("figures/summary_timeseries_lines/{sp}.jpg"),
           width = 10, height = 6)
    
    # timeseries as points across depths with year color
    ggplot(.x,
           aes(x = proportion*100, y = depth,
               group = paste0(site, subsite),
               color = year)) +
      geom_point(position = 
                   ggstance::position_jitterdodgev(jitter.width = 0.01, 
                                                   jitter.height = 0.01,
                                                   dodge.height  = 0.5),
                 alpha = 0.4) +
      year_color_scale() +
      theme(axis.title.x = ggtext::element_markdown()) +
      labs(y = "",
           color = "Year",
           x = y_lab) #sub in species name
    
    ggsave(glue("figures/raw_timeseries_points/{sp}.jpg"),
           width = 5, height = 3)
    
  })


  

