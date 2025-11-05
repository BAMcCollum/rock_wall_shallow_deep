library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(performance)
library(purrr)

# new data for annotations we already did
annotated_cover_data_raw <- #read_csv("data/annotations/Annotated_photos_coverage_19_20251029.csv") |>
  read_csv("data/annotations/Annotated_photos_coverage_19_20251031.csv") |>
  rename(image = Image) 

annotated_cover_data <- annotated_cover_data_raw |> 
  filter(class != "BACK")

ggplot(annotated_cover_data,
       aes(x = pixels_expanded, y =pixels_predicted )) +
  geom_point() +
  facet_wrap(vars(class)) +
  labs(x = "Expanded Pixels",
       y = "Predicted Pixels from Classifier") +
  stat_smooth(method = "lm") +
  theme_bw() #+
  #geom_abline(slope = 1, lty = 2)

ggsave("figures/calibration.jpg", width = 10, height = 10)

##
# model
##

annotated_cover_data |>
  group_by(class) |>
  nest() |>
  mutate(mod = map(data, ~lm(pixels_predicted ~ pixels_expanded, data = .x)),
         r2 = map(mod, ~r2(.x) [1][[1]])) |>
  unnest(r2) |>
  select(class, r2)

##
# Compare to pixel-based percent cover from original classifications
##

annotated_cover_data_percent <- 
  annotated_cover_data_raw |>
  group_by(image) |>
  mutate(#BACK_pixels = ifelse(class == "BACK", pixels, 0),  BACK is non-classified species
    framer_pixels = ifelse(class == "FRM", pixels_predicted, 0),
    sediment_pixels = ifelse(class == "SED", pixels_predicted, 0),
    mobile_pixels = ifelse(class %in% c("DROE", "AST"), pixels_predicted, 0),
    
    framer_points = ifelse(class == "FRM", points_count, 0),
    sediment_points = ifelse(class == "SED", points_count, 0),
    mobile_points = ifelse(class %in% c("DROE", "AST"), points_count, 0),
    
    
    total_sampled_pixels = `total pixels in image` -
      #sum(BACK_pixels) - #BACK is non-classified species
      sum(mobile_pixels) - 
      sum(sediment_pixels) - 
      sum(framer_pixels),
    
    total_sampled_points = 200 -
      #sum(BACK_pixels) - #BACK is non-classified species
      sum(framer_points) - 
      sum(sediment_points) - 
      sum(mobile_points),
    
  ) |>
  ungroup() |>
  filter(!(class %in% c("BACK", "DROE", "AST", "FRM", "SED"))) |>
  mutate(proportion_predicted = pixels_predicted / total_sampled_pixels,
         proportion_points = points_count / total_sampled_points,
  )
  
# plot

ggplot(annotated_cover_data_percent,
       aes(x = proportion_points*100, y =proportion_predicted *100)) +
  geom_point() +
  facet_wrap(vars(class)) +
  labs(x = "Percent from Points",
       y = "Predicted Percent from Classifier") +
  stat_smooth(method = "lm") +
  theme_bw() +
  scale_y_continuous(limits = c(0,100))#+
#geom_abline(slope = 1, lty = 2)
