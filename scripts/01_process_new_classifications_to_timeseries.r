#' ---------------------------------------------
#' Generate % Cover data from classification
#' masks done by the segmentation model
#' ---------------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(glue)
library(tidyr)
library(ggplot2)


# classification data
cover_data <- read_csv("data/Photos_to_be_Annotated_predictions_20251023.csv") |>
  rename(image = Image) |>
  separate_wider_delim(image, "/",
                       names = c("site",
                                 "year",
                                 "depth", 
                                 "folder",
                                 "image_name"),
                       cols_remove = FALSE) |>
  select(-folder, -image_name) |>
  mutate(depth = gsub(" .*", "", depth),#trim site name from some
         year = gsub(" .*", "", year),
         site = gsub(" ", "", site)) 
  
# new data for annotations we already did
annotated_cover_data <- read_csv("data/Annotated_photos_predictions_20251023.csv") |>
  rename(image = Image) |>
  mutate(image = gsub("-", "/", image),
         image = gsub("_", "/", image)) |>
  separate_wider_delim(image, "/",
                       names = c("year",
                                 "site",
                                 "depth", 
                                 "image_name"),
                       too_few = "debug",
                       too_many = "merge", # some subsites and image names were split
                       cols_remove = FALSE) |>
  # For some, depths got weird
  mutate(depth = ifelse(grepl("HRDeepP", depth), "Deep", depth)) |>
  mutate(depth = gsub("HRD", "Deep", depth))|> #filename snafu
  #ok, fixed
  select(-c(image_name, image_ok, image_pieces, image_remainder)) |>
  mutate(site = ifelse(site %in% c("DW", "LR", "LRB", "DWB", "DWT", "LRT"),
                   "HRD",
                   site))

all_cover_data <- bind_rows(cover_data, annotated_cover_data) |>
  mutate(image_name = gsub(".*/", "", image)) |>
  mutate(year = as.numeric(year))

##
# write manifest so we have it
##
all_cover_data |>
  group_by(image) |>
  slice(1L) |>
  select(site, year, depth, image) |>
  write_csv("data/manifest_unmarked.csv")

##
# file info
##
manifest <- read_csv("data/manifest_20251023.csv")

# check bad joins - should be nothing
anti_join(manifest, all_cover_data)
anti_join(all_cover_data, manifest)

cover_joined <- left_join(all_cover_data, manifest) |>
  mutate(subsite = ifelse(subsite == "VDRD", "VDR", subsite)) #typo


# see when things were sampled
cover_joined |>
  group_by(site, subsite, year) |>
  summarize(n_quads = n_distinct(image)) |>
  ggplot(
       aes(x = year, 
           y = paste(site, subsite),
           fill = n_quads)) +
  geom_tile() +
  scale_fill_viridis_c()

###
# Get correct total image pixels
# by deleting the FRM and BACK from total image pixels and then 
# filtering BACK out as we no longer need it
###
cover_joined_proportions <- cover_joined |>
  group_by(site, subsite, year, image) |>
  mutate(#BACK_pixels = ifelse(class == "BACK", pixels, 0),  BACK is non-classified species
         framer_pixels = ifelse(class == "FRM", pixels, 0),
         mobile_pixels = ifelse(class %in% c("DROE", "AST"), pixels, 0),
           total_sampled_pixels = `total pixels in image` -
           #sum(BACK_pixels) - #BACK is non-classified species
           sum(mobile_pixels) - 
           sum(framer_pixels)) |>
  ungroup() |>
  filter(!(class %in% c("BACK", "DROE", "AST", "FRM"))) |>
  mutate(proportion = pixels / total_sampled_pixels)

###
# Add zeroes where needed
###

cover_joined_proportions_zeroes <- cover_joined_proportions |>
  # add zeroes
  complete(crossing(nesting(site, subsite, image,
                            depth,
                            year, 
                            image_name, total_sampled_pixels), 
                    class), 
           fill = list(pixels = 0, proportion = 0)) |>
  select(site, subsite, image,
         depth, year, class,
         proportion, pixels, total_sampled_pixels,
         image_name)

##
# expand names of classes to species
##
class_dictionary <- read_csv("data/Merged_Species_list_with_colors.csv") |>
  rename(class = CoralNetCode) |>
  rename_with(tolower)

newclass <- tribble(
  ~genus, ~species, ~class, ~`hex code`,
  "Mussel", "", "MUSS", "#?????"
)

class_dictionary <- bind_rows(class_dictionary, newclass)

#class_dictionary[class_dictionary$class %in% clean_data$class,] |> View()

# join! by class
clean_data_joined <- 
  left_join(cover_joined_proportions_zeroes, class_dictionary) |>
  mutate(species = paste(genus, species)) |>
  select(-genus)|>
  mutate(species = gsub(" $", "", species))

# check for missed classes
clean_data_joined$class[which(is.na(clean_data_joined$`hex code`))] |> unique()

# see unique species
clean_data_joined |> pull(species) |> unique() |> sort()

##
# write our what species we have
##
tibble(species = clean_data_joined$species |>
         unique() |> 
         sort()) |> 
  write_csv("data/unique_species.csv")


##
# add site continuous depth info
##
site_data_dictionary <- 
  read_csv("data/Subsite_Depth_Key.csv")|>
  rename_with(tolower) |>
  rename(depth = depth_category)

aggregated_data_with_info <- 
  left_join(clean_data_joined, site_data_dictionary) 

# check and make sure we didn't miss anything
which(is.na(aggregated_data_with_info$average_depth))

# check and make sure we have at least 1 
# measurement per subsite:site in the dictionary
anti_join(site_data_dictionary, aggregated_data_with_info)

##
# swap species and clean_species
# to avoid future problems of using a "/" in species names
# which makes some functions think we are referencing another
# directory - also prep for thermal info and add a centered year
##
species_names <- read_csv("data/co_occuring_species_20251024_with_new_classes.csv") |>
  select(-coefficients_species)

aggregated_data_with_info_sp <- 
  aggregated_data_with_info |>
  left_join(species_names) |>
  select(-species) |>
  rename(species = clean_species) |>
  mutate( year = as.numeric(year),
          year_cent = year - mean(seq(min(year), max(year), by = 1)))

# check species names
aggregated_data_with_info_sp$species |> unique() |> sort()

##
# write out
##

write_csv(aggregated_data_with_info_sp, "data/substrate_data_classification_long.csv")



##
# Create a subsite aggregated data set
##

subsite_data <- aggregated_data_with_info_sp |>
  # first, how many quads per subsite per year
  group_by(site, subsite, year, year_cent, 
           depth, average_depth) |>
  mutate (n_quads = n_distinct(image))|>
  # now calculate avg porportion
  group_by(site, subsite, year, year_cent, 
           depth, average_depth,
           class, species, gen_spp) |>
  summarize(n_quads = n_quads[1],
            proportion = sum(proportion)/n_quads[1]) |> 
  ungroup()

# check and make sure things sum to one
subsite_data |>
  group_by(site, subsite, year, year_cent, 
           depth, average_depth) |>
  summarize(total = sum(proportion)) |>
  pull(total)

# see when things were sampled
ggplot(subsite_data,
       aes(x = year, 
           y = paste(site, subsite),
           fill = n_quads)) +
  geom_tile() +
  scale_fill_viridis_c()

##
# Add thermal preference data
##
thermal_data <- read_csv("data/Occurrence_based_species_thermal_indicies_Photos_100825.csv") |>
  select(gen_spp, species_id, n_obis_rec,
         BO21_tempmax_bdmean_mean, BO21_tempmax_bdmin_mean, BO21_tempmax_bdmin_max,
         BO21_tempmax_bdmean_q5, BO21_tempmax_bdmean_q95)

subsite_data_thermal <- 
  left_join(subsite_data, thermal_data)

##
# write out
##
write_csv(subsite_data_thermal, "data/substrate_data_classification_subsite_long.csv")


##
# swap in point data from HRI and HRO
##
subsite_data_hrd <- subsite_data_thermal |>
  filter(site == "HRD")

subsite_data_hri_hro <- read_csv("https://github.com/BAMcCollum/rock_wall_change_thermal_preference/raw/refs/heads/main/data/sebens_substrate_proportion_20250908.csv") |>
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
  filter(year >= 1989) 
  
cca_dat <- subsite_data_hri_hro |>
  filter(species_points %in% c("clathromorphum_sp" ,
                               "phymatolithon_sp",
                               "lithothamnion_spp")) |>
  group_by(site, area, location, subsite, depth_m, angle, 
           date, year, month, day, no_qds, total_points) |>
  summarize(species_points = "crustose_coralline_algae",
            porportion_points = sum(porportion_points)) |>
  ungroup()

#make MUSS
muss_dat <- subsite_data_hri_hro |>
  filter(species_points %in% c("modiolus_modiolus" ,
                               "mytilus_edulis")) |>
  group_by(site, area, location, subsite, depth_m, angle, 
           date, year, month, day, no_qds, total_points) |>
  summarize(species_points = "mussels",
            porportion_points = sum(porportion_points)) |>
  ungroup()  


merger_sp <- tribble(
  ~species, ~species_points,
  "Alcyonium siderium", "alcyonium_sub",
  "Aplidium glabrum", "aplidium_glabrum",
  "Balanus/Amphibalanus spp.", "balanus_balanus",
  "Botrylloides violaceus", "botrylloides_sp",
  "Crustose Coralline Algae pink", "crustose_coralline_algae", 
  "Didemnum vexillum", "didemnum_vexillum",
  "Diplosoma listerianum", "diplosoma_listerianum",
  "Ectopleura/Tubularia spp.", "tubularia_sub",
  "Hydrozoa/Bryozoa complex", "hyd_bry_complex",
  "Halichondria panicea", "halichondria_panicea",
  "Isodictya spp.", "isodictya_spp",
  "Metridium senile", "metridium_sub",
  #"Mytilus edulis", "mytilus_edulis"#,
  "Mussel", "mussels",
  #"Unidentified red fleshy crust"
)

subsite_data_hri_hro_matching <- subsite_data_hri_hro |>
  bind_rows(cca_dat, muss_dat) |>
  rename(proportion = porportion_points) |>
  filter(species_points %in% merger_sp$species_points)|>
  left_join(merger_sp) |>
  left_join(species_names) |>
  select(-species) |>
  rename(species = clean_species) |>
  left_join(thermal_data) |>
  left_join(site_data_dictionary) |>
  filter(!is.na(depth)) # gets rid of horizontal
  
subsite_data_mixed <- bind_rows(
  subsite_data_hrd,
  subsite_data_hri_hro_matching
)

write_csv(subsite_data_mixed, "data/substrate_data_mixed_subsite_long.csv")

##
# cover dist
##
subsite_data_mixed |>
  group_by(site, year, month, subsite, depth) |>
  summarize(cover = sum(proportion)) |>
  ggplot(aes(x = cover)) +
  geom_histogram() +
  labs(x = "Sum of Proportional Cover", y = "Count") +
  facet_wrap(vars(forcats::fct_rev(depth)))



ggsave("figures/total_cover_histogram.jpg", width = 8, height = 4)
