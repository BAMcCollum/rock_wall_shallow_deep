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
cover_data <- read_csv("data/annotations/Photos_to_be_Annotated_coverage_18_20251028.csv") |>
  rename(image = Image) |>
  separate_wider_delim(image, "/",
                       names = c("site",
                                 "year",
                                 "depth", 
                                 "folder",
                                 "image_name"),
                       cols_remove = FALSE,
                       too_few = "debug")  |>
  filter(!is.na(year)) #some training data left in
# 
# ## hopefully temporary - classification data missing some info
# manifest_unknown <- read_csv("data/manifest_unknown_20251028.csv") |>
#   mutate(image = gsub("(^.*)/([^\\/]+$)", "\\2", image))
# 
# cover_data_na <- cover_data |> filter(is.na(year)) |>
#   select(image, class, pixels, `total pixels in image`) |>
#   left_join(manifest_unknown)
# # 
# # write_csv(tibble(image = cover_data_na |> pull(image) |> unique()),
# #                  "data/manifest_unknown.csv")
# 
# cover_data_na_na <- cover_data_na |> filter(is.na(year)) |>
#   select(image, class, pixels, `total pixels in image`)
#   select(image, class, pixels, `total pixels in image`) |>
#   separate_wider_delim(image, "-",
#                        names = c("year",
#                                  "subsite",
#                                  "site",
#                                  "image_name"),
#                        cols_remove = FALSE,
#                        too_few = "debug") |>
#   mutate(image_name = image)
# 
# cover_data_na_na <- cover_data_na |> filter(is.na(site)) |>
#   select(image, class, pixels, `total pixels in image`) |>
#   mutate(image = gsub("\\-P", "A\\-P", image),
#          image = gsub("VC\\-", "VC1AB\\-", image)
#   ) |>
#   # create metadata
#   separate_wider_regex(image,
#                        c(site = "^\\D+",
#                          month = "\\d\\d",
#                          day = "\\d\\d",
#                          year = "\\d\\d",
#                          subsite = "\\D+",
#                          quadrat = ".*[AB]|extra", # some funky quad names
#                          "\\-*",
#                          photo_id = ".+",
#                          "_masks\\.csv")
#                        ,too_few = "debug",
#                        cols_remove = FALSE
#   )  |>
#   mutate(image_name = image)
  

# cover_data_fixed <- bind_rows(
#   cover_data |> filter(is.na(year)),
#   cover_data_na |> filter(is.na(site)),
#   cover_data_na_na
# ) |>
#   select(image, class, pixels, `total pixels in image`,
#          site, year, depth) 
  
## Back to our regularly scheduled program
cover_data <- cover_data |>
  select(-folder, -image_name) |>
  mutate(depth = gsub(" .*", "", depth),#trim site name from some
         year = gsub(" .*", "", year),
         site = gsub(" ", "", site)) 
  
# new data for annotations we already did
annotated_cover_data <- read_csv("data/annotations/Annotated_photos_coverage_18_20251028.csv") |>
  rename(image = Image) |>
  rename(pixels = pixels_predicted) |>
  mutate(image = gsub("-", "\\/", image),
         image = gsub("_", "\\/", image)) |>
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
  mutate(year = as.numeric(year),
         image = gsub("^HRO \\/", "HRO\\/", image)) |>
  mutate(image = gsub("-", "\\/", image),
         image = gsub("_", "\\/", image)) 

##
# write manifest so we have it
##
# all_cover_data |>
#   group_by(image) |>
#   slice(1L) |>
#   select(site, year, depth, image) |>
#   write_csv("data/manifest_unmarked.csv")

##
# file info
##
# info about new images - and fix directory structure for match
new_image_manifest <- read_csv("data/manifest_even_more.csv") |>
  rename(image = file) |>
  mutate(image = gsub("JPG", "png", image),
         image = gsub("jpg", "png", image),
         image = gsub("^\\.\\/", "", image),
         image = gsub("^(\\d\\d\\d\\d) (HR[IO])\\/", 
                      "\\2\\/\\1 \\2\\/", image))

second_new_image_manifest <- read_csv("data/manifest_new_images_third.csv")         

manifest <- bind_rows(read_csv("data/manifest_20251023.csv"),
                      new_image_manifest,
                      second_new_image_manifest) |>
  mutate(image = gsub("^HRO \\/", "HRO\\/", image))|>
  mutate(image = gsub("-", "\\/", image),
         image = gsub("_", "\\/", image))

  

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
    sediment_pixels = ifelse(class == "SED", pixels, 0),
    mobile_pixels = ifelse(class %in% c("DROE", "AST"), pixels, 0),
           total_sampled_pixels = `total pixels in image` -
           #sum(BACK_pixels) - #BACK is non-classified species
      sum(mobile_pixels) - 
      sum(sediment_pixels) - 
      sum(framer_pixels)) |>
  ungroup() |>
  filter(!(class %in% c("BACK", "DROE", "AST", "FRM", "SED"))) |>
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

# newclass <- tribble(
#   ~genus, ~species, ~class, ~`hex code`,
#   "Mussel", "", "MUSS", "#?????"
# )

#class_dictionary <- bind_rows(class_dictionary, newclass)

#class_dictionary[class_dictionary$class %in% clean_data$class,] |> View()

# join! by class
clean_data_joined <- 
  left_join(cover_joined_proportions_zeroes, class_dictionary) |>
  mutate(species = paste(genus, species)) |>
  select(-genus)|>
  mutate(species = gsub("NA", "", species),
         species = gsub(" $", "", species),
  )

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

subsite_data_thermal$gen_spp |> unique()
##
# write out
##
write_csv(subsite_data_thermal, "data/substrate_data_subsite_long.csv")


##
# cover dist
##
subsite_data_thermal |>
  group_by(site, year, subsite, depth) |>
  summarize(cover = sum(proportion)) |>
  ggplot(aes(x = cover)) +
  geom_histogram() +
  labs(x = "Sum of Proportional Cover", y = "Count") +
  facet_wrap(vars(forcats::fct_rev(depth))) +
  theme_bw(base_size = 14)

ggsave("figures/total_cover_histogram.jpg", width = 8, height = 4)
