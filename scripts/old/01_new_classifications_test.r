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
cover_data <- read_csv("data/Annotated_photos_predictions_20251023.csv") |>
  rename(image = Image)


# site-subsite info
# make sure that all image names are .png
manifest <- read_csv("data/files_to_process_subsites.csv") |>
  mutate(image = gsub("\\.jpg", "\\.png", image),
         image = gsub("\\.JPG", "\\.png", image)) |>
  rename(subsite = Subsite)

# manifest |>
#   group_by(image) |>
#   summarize(dups = n() > 1) |>
#   filter(dups)

# fixed duplicate image info
# make sure that all image names are .png
dup_manifest <- read_csv("data/fixme_cesar_name.csv")|>
  rename(subsite = Subsite) |>
  mutate(image = gsub("\\.jpg", "\\.png", image),
         image = gsub("\\.JPG", "\\.png", image)) |>
  rename(orig_image = image,
         image = cesar_name) # this is what we need

##
# join the manifest and the cover data
##

# remove duplicate names from manifest
manifest_filtered <- manifest |>
  filter(!(image %in% dup_manifest$image))

# check to make sure all dups are out
manifest_filtered |>
  group_by(image) |>
  summarize(dups = n() > 1) |>
  filter(dups)
# 
# 
# still_bad <- cover_data |>
#   filter(grepl("image0", image)) |>
#   group_by(image) |>
#   slice(1L) |>
#   select(image) 
# 
# still_bad #|>
#  # write_csv("data/moar_fixes_needed.csv")
###

corrected_manifest <- bind_rows(manifest_filtered, dup_manifest)

cover_joined <- left_join(cover_data, 
                          corrected_manifest, 
                          relationship = "many-to-one") 


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
# by deleting the BACK from total image pixels and then 
# filtering BACK out as we no longer need it
###
cover_joined_proportions <- cover_joined |>
  group_by(site, subsite, year, image) |>
  mutate(BACK_pixels = ifelse(class == "BACK", pixels, 0),
         mobile_pixels = ifelse(class %in% c("DROE", "AST"), pixels, 0),
           total_sampled_pixels = `total pixels in image` -
           sum(BACK_pixels) - sum(mobile_pixels)) |>
  ungroup() |>
  filter(!(class %in% c("BACK", "DROE", "AST"))) |>
  mutate(proportion = pixels / total_sampled_pixels)

# check and make sure it all sums to 1 for each image
cover_joined_proportions |>
  group_by(image) |>
  summarize(img_proportion = sum(proportion)) |>
  pull(img_proportion) |>
  (\(.x) which((1-.x) > 0.001))() #deals with precision error

###
# Add zeroes where needed
###

cover_joined_proportions_zeroes <- cover_joined_proportions |>
  # add zeroes
  complete(crossing(nesting(site, subsite, image,
                            depth,
                            year, 
                            full_file_name, total_sampled_pixels), 
                    class), 
           fill = list(pixels = 0, proportion = 0)) |>
  select(site, subsite, image,
         depth, year, class,
         proportion, pixels, total_sampled_pixels,
         full_file_name)

##
# expand names of classes to species
##
class_dictionary <- read_csv("data/Merged_Species_list_with_colors.csv") |>
  rename(class = CoralNetCode) |>
  rename_with(tolower)

#class_dictionary[class_dictionary$class %in% clean_data$class,] |> View()

# join! by class
clean_data_joined <- 
  left_join(cover_joined_proportions_zeroes, class_dictionary) |>
  mutate(species = paste(genus, species)) |>
  select(-genus)

# check for missed classes
clean_data_joined$class[which(is.na(clean_data_joined$`hex code`))] |> unique()

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
species_names <- read_csv("data/co_occuring_species_20250919_with_new_classes.csv") |>
  select(-coefficients_species)

aggregated_data_with_info_sp <- 
  aggregated_data_with_info |>
  left_join(species_names) |>
  select(-species) |>
  rename(species = clean_species) |>
  mutate( year = as.numeric(year),
          year_cent = year - mean(seq(min(year), max(year), by = 1)))

##
# write out
##

write_csv(aggregated_data_with_info_sp, "data/substrate_data_long.csv")



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
write_csv(subsite_data_thermal, "data/substrate_data_subsite_long.csv")
