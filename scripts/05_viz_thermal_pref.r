library(tidyverse)
library(ggplot2)
library(readr)

# read in and go from here
#source("scripts/load_data_settings.r")

thermal_data <- read_csv("data/Occurrence_based_species_thermal_indicies_Photos_100825.csv") |>
  select(gen_spp, species_id, n_obis_rec,
         BO21_tempmax_bdmean_mean, BO21_tempmax_bdmin_mean, BO21_tempmax_bdmin_max,
         BO21_tempmax_bdmean_q5, BO21_tempmax_bdmean_q95)

coefout <- thermal_data

# filter to species we have
# coefout <- filter(coefout, gen_spp %in% subsite_substrate_long$gen_spp)
View(coefout)
# plot data ---------------------------------------------------------------


color2 <- "#782391"

coefout %>% 
  filter(!is.na(gen_spp)) |>
  mutate(gen_spp = forcats::fct_reorder(gen_spp, BO21_tempmax_bdmin_mean) ) %>%
  
  ggplot(aes(x=gen_spp)) +
  
  
  geom_point(aes(y=BO21_tempmax_bdmean_q5),  color = color2, alpha=.5)+
  geom_point(aes(y=BO21_tempmax_bdmean_q95), color = color2, alpha=.5)+
  geom_point(aes(y=BO21_tempmax_bdmean_mean), color = color2, alpha=1, size=2)+
  geom_segment(aes(xend=gen_spp,
                   y=BO21_tempmax_bdmean_q5,
                   yend=BO21_tempmax_bdmean_q95), color = color2, alpha=.5)+
#  annotate(geom="text",
#           x=3, y=30, 
#           hjust=-0.1, vjust=0.2,
#           label = 
#             "\n Max temp at mean depth",
#           color = color2,
#           fontface="bold",
#           size=5) +
  
  labs(x=NULL, y= expression(paste("Water Temperature in ", "\u00b0C"))) +
  theme_classic() +
  theme(#plot.margin = margin(l=25,b=5,unit="pt"),
    axis.text.x = element_text(angle = -90, hjust = 0))+
  geom_hline(yintercept=c(14.07, 17.4), linetype='dashed', color=c('turquoise', 'green')) +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18))

ggsave("figures/thermal_preference_ranges.jpg")

