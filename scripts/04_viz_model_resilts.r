#' -----------------------------------------------
#' This script creates visualizations of model results
#' of species time series
#' -----------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(glue)
library(ggarrow)

# load data and set themes
source("scripts/load_data_settings.r")
fitted_curves <- read_csv("data/fitted_long_ordbetareg.csv")
coefs_long <- read_csv("data/change_coefficients_ordbetareg.csv")


##
# show slopes and HPDs
##
ggplot(coefs_long,
       aes(x = year_cent.trend,
           xmin = lower.HPD, xmax = upper.HPD,
           y = species, color = depth, shape = depth)) +
  geom_point(size = 3) +
  geom_linerange() +
  labs(x="Coefficient of Change", y = "", color = "", shape = "") +
  geom_vline(xintercept = 0, lty = 2) +
  depth_color_scale() +
  theme(legend.position = "bottom")

ggsave("figures/coefs.jpg", width = 7, height = 5)

## show as heat map

ggplot(coefs_long |> arrange(desc(depth), desc(year_cent.trend)),
       aes(fill = year_cent.trend,
           y = depth,
           x = species |> forcats::fct_inorder())) +
  geom_tile() +
  labs(x="", y = "", 
       fill = "Coefficient of\nchange", ) +
  theme(legend.position = "bottom") +
    scale_fill_distiller(palette = "BrBG") +
  theme_classic(base_size = 16)  +
  theme(axis) +
  scale_x_discrete(position = "top",
                   labels = scales::label_wrap(15),
                   guide = guide_axis(n.dodge = 2)) 
  

ggsave("figures/coefs_heatmap.jpg", width = 12, height = 4)


##
# show data and predictions
##
for(one_sp in unique(fitted_curves$species)){
  print(one_sp) # so we know where we are

  .x <- subsite_substrate_long |>
    filter(species == one_sp)

  .fit <- filter(fitted_curves, species == one_sp)
  

  y_lab <- glue("Percent Cover of {one_sp}")
  
  # correct some species names
  if(!is.na(.x$gen_spp[1])) {
    one_sp <- .x$gen_spp[1]
    y_lab <- glue("Percent Cover of *{one_sp}*")
  }
  

  ggplot() +
  geom_line(data = .fit,
            aes(x = year, y = estimate*100, color = depth)) +
  geom_ribbon(data = .fit,
              aes(x = year, y = estimate*100, fill = depth,
                  ymin = lower.HPD*100,
                  ymax = upper.HPD*100),
              alpha = 0.3) +
  depth_color_scale() +
  depth_fill_scale() +
    geom_jitter(data = .x,
               aes(x = year, y = proportion*100, color = depth),
               alpha = 0.4) +
  facet_wrap(vars(forcats::fct_rev(depth)), ncol = 1) +
    labs(x = "Year",
         color = "", fill = "",
         y = y_lab)+ #sub in species name
    theme(axis.title.y = ggtext::element_markdown())
  
  
  ggsave(glue("figures/fits_with_data_panels/{one_sp}.jpg"),
         width = 5, height = 6)
  
  ##
  # show dot and arrow plots
  ##
  ggplot(.x,
         aes(x = proportion*100, y = depth,
             group = paste0(site, subsite),
             color = year)) +
    geom_point(position = 
                 ggstance::position_jitterdodgev(jitter.width = 0.01, 
                                                 jitter.height = 0.01,
                                                 dodge.height  = 0.5),
               alpha = 0.4) +
    geom_arrow(data = .fit |> filter(year >1989 & year < 2010),
               aes(x = estimate*100, y = depth, group = depth),
               color = "black") +
    year_color_scale() +
    theme(axis.title.x = ggtext::element_markdown()) +
    labs(y = "",
         color = "Year",
         x = y_lab) #sub in species name
  
  ggsave(glue("figures/prediction_arrows/{one_sp}.jpg"),
         width = 5, height = 3)
}
