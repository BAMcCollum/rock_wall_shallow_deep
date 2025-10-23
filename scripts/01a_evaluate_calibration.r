library(dplyr)
library(readr)

# our data filtered to HRI and HRO
subsite_substrate_long <- read_csv("data/substrate_data_subsite_long.csv") |>
  filter(site %in% c("HRI", "HRO"))

# point data filtered

sebens_dat <- read_csv("https://github.com/BAMcCollum/rock_wall_change_thermal_preference/raw/refs/heads/main/data/sebens_substrate_proportion_20250908.csv") |>
  pivot_longer(alcyonium_sub:tubularia_sub,
               names_to = "species_points",
               values_to = "porportion_points") |>
  filter(area %in% c("Halfway Rock Outer", "Halfway Rock Inner")) |>
  mutate(subsite = site,
         subsite = gsub("HR[IO]", "", subsite),
         site = area, 
         site = ifelse(site=="Halfway Rock Outer", "HRO", site),
         site = ifelse(site=="Halfway Rock Inner", "HRI", site),
  ) |>
  filter(subsite %in% subsite_substrate_long$subsite) |>
  filter(year %in% subsite_substrate_long$year)

merger_sp <- tribble(
  ~species, ~species_points,
  "Alcyonium siderium", "alcyonium_sub",
  "Balanus and Amphibalanus spp.", "balanus_balanus",
  "Botrylloides violaceus", "botrylloides_sp",
  #"Crustose Coralline Algae pink", #check later
  "Didemnum vexillum", "didemnum_vexillum",
  "Ectopleura spp.", "tubularia_sub",
  "Hydrozoa-Bryozoa complex", "hyd_bry_complex",
  "Isodictya spp.", "isodictya_spp",
  "Mytilus edulis", "mytilus_edulis"#,
  #"Unidentified red fleshy crust"
)

sebens_dat <- filter(sebens_dat,
                     species_points %in% merger_sp$species_points) |>
  select(site, subsite, year, month, species_points, porportion_points)

# join the two dataset
joined_proportion_dat <- subsite_substrate_long |>
  select(site, subsite, year, species, proportion) |>
  left_join(merger_sp) |>
  left_join(sebens_dat)

#plot
ggplot(joined_proportion_dat |> filter(!is.na(species_points)),
       aes(x = porportion_points,
           y = proportion, 
           color = paste(site, subsite))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  facet_wrap(vars(species)) +
  theme_classic(base_size = 14) +
  labs(x = "Proportion from Point Classifications",
       y = "Proportion from SAM2 Classifications") +
  guides(color = "none")

ggsave("figures/calibration.jpg", width = 5, height = 5)
