library(dplyr)
library(readr)
library(ggplot2)
library(performance)

# new data for annotations we already did
annotated_cover_data <- read_csv("data/annotations/Annotated_photos_coverage_19_20251029.csv") |>
  rename(image = Image) |>
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
