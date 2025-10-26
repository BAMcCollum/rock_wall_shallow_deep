#' ---------------------------------------------
#' Generate % Cover data from classification
#' masks done by the segmentation model
#' ---------------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(glue)
library(tidyr)


# files to process
training_mask_dir <- "data/mask_csvs_mid_shallow_training/"
deep_training_mask_dir <- "data/mask_csvs_100_hrd/"

training_masks <- list.files(training_mask_dir, pattern = "\\.csv")
deep_training_masks <- list.files(deep_training_mask_dir, pattern = "\\.csv")

# info about files
training_info <- read_csv("data/training_masks_fixed_subsites.csv") |>
  select(site:file_name)

deep_training_info <- read_csv("data/hrd_training_masks_with_subsites.csv") |>
  rename(quadrat = quad) |>
  mutate(photo_id = file_name,
         quadrat = gsub("_masks\\.csv", "", quadrat)) |> 
  select(-file_name_ok, -file_name_pieces, -file_name_remainder)

classification_info <- read_csv("data/files_to_process_subsites.csv")


###
# Function to read in one file and turn
# to data
###
get_one_file <- function(file_name, mask_dir){
  read_csv(glue("{mask_dir}/{file_name}"),
                  show_col_types = FALSE) |>
    group_by(class) |>
    summarize(
      file_name = file_name,
      pixels = sum(pixels),
      total_image_pixels = `total pixels in image`[1],
    )
}

##
# read it all in
##
training_data <- map(training_masks, get_one_file,
                     mask_dir = training_mask_dir,
                .progress = TRUE) |>
  list_rbind()

deep_training_data <- map(deep_training_masks, get_one_file,
                     mask_dir = deep_training_mask_dir,
                     .progress = TRUE) |>
  list_rbind()

###
# Clean data
###

clean_training_data <- left_join(training_data, training_info)
clean_deep_data <- left_join(deep_training_data, deep_training_info)


clean_data <- bind_rows(clean_training_data, clean_deep_data)

# check the merge - should be empty
clean_data[which(is.na(clean_data$year)),]$file_name |> unique()




# 
# clean_data <- all_data |>
#   mutate(file_name = gsub("\\-P", "A\\-P", file_name),
#          file_name = gsub("VC\\-", "VC1AB\\-", file_name)
#   ) |>
#   # create metadata
#   separate_wider_regex(file_name,
#                        c(site = "^\\D+",
#                          month = "\\d\\d",
#                          day = "\\d\\d",
#                          year = "\\d\\d",
#                          subsite = "\\D+",
#                          quadrat = ".*[AB]|extra", # some funky quad names
#                          "\\-*",
#                          photo_id = ".+",
#                          "_masks\\.csv")
#                        ,too_few = "debug"
#   ) |>

##
# some light class cleaning
# of typos in class names
##
clean_data <- clean_data |>
  mutate(class = ifelse(class=="BACK", "BAD", class),
         class = ifelse(class == "Tli", "TLI", class),
         class = ifelse(class == "BareP", "Rock", class))


clean_data_zeroed <- clean_data |>
  # add zeroes
  complete(crossing(nesting(site, subsite, quadrat,
                            month, day, year, 
                            photo_id, 
                            file_name, total_image_pixels), 
                    class), 
           fill = list(pixels = 0))
##
# expand names of classes
##
class_dictionary <- read_csv("data/Merged_Species_list_with_colors.csv") |>
  rename(class = CoralNetCode)

#class_dictionary[class_dictionary$class %in% clean_data$class,] |> View()

# join! by class
clean_data_joined <- 
  left_join(clean_data_zeroed, class_dictionary)

# check for bad classes
clean_data_joined$class[which(is.na(clean_data_joined$`Hex Code`))] |> unique()


## 
# clean out bad columns for substrate
# then create percent cover variables with new totals
##
recleaned_data_joined <- clean_data_joined |>
  # add pixel correction for mobile/sessile, bad, or shell hash
  # before removing bad classes
  mutate(bad_pixels = case_when(
    `Mobile/Sessile` == "mobile" ~ pixels,
    !is.na(Bad) ~ pixels,
    Species == "sediment" ~ pixels,
    class == "Shell" ~ pixels,
    TRUE ~ 0
  )) |>
  group_by(site, subsite, quadrat, photo_id, 
           month, day, year) |>
  mutate(total_pixels = total_image_pixels - sum(bad_pixels)) |>
  # filter out anything either with mobile in the Mobile/Sessile column
  filter((`Mobile/Sessile` != "mobile") |> replace_na(TRUE)) |>
  # bad in the Bad column
  filter(is.na(Bad)) |>
  filter(class != "BAD") |>
  # sediment from the Species column
  filter((Species != "sediment")|> replace_na(TRUE)) |>
  # (canopy) from the Canopy/Substrate column
  # actualy, do not remove as the classifier cannot distinguish
  # between the two and we will aggregate below
  #filter((`Canopy/Substrate` != "(canopy)") |> replace_na(TRUE)) |>
  # cut shell hash
  filter((class != "Shell")) 


##
# recalculate total points for each image
# calculate percent cover for each cover class
##
aggregated_data <- recleaned_data_joined |>
  # group_by(site, subsite, quadrat, photo_id, 
  #          month, day, year) |>
  # mutate(total_pixels = total_pixels - bad_pixels) |>
  group_by(site, subsite, quadrat, photo_id, 
           month, day, year,
           class, Genus, Species) |>
  summarize(pixels = sum(pixels),
            total_pixels = total_pixels[1]) |>
  ungroup() |>
  rename_with(tolower) |>
  mutate(proportion = pixels/total_pixels,
         year = as.numeric(year),
         year_cent = year - mean(seq(min(year), max(year), by = 1))) |>
  mutate(species = paste(genus, species)) |>
  select(-genus)

# debug - should have nothing
which(aggregated_data$species=="NA NA")

##
# write our what species we have
##
tibble(species = aggregated_data$species |>
  unique() |> 
  sort()) |> 
  write_csv("data/unique_species.csv")
  
##
# add site info
##
site_data_dictionary <- 
  read_csv("data/Subsite_Depth_Key.csv")|>
  rename_with(tolower)

aggregated_data_with_info <- 
  left_join(aggregated_data, site_data_dictionary)

##
# check and make sure we have at least 1 
# measurement per subsite:site in the dictionary
##
anti_join(site_data_dictionary, aggregated_data_with_info)

##
# swap species and clean_species
# to avoid future problems of using a "/" in species names
# which makes some functions think we are referencing another
# directory - also prep for thermal info
##
species_names <- read_csv("data/co_occuring_species_20250919_with_new_classes.csv") |>
  select(-coefficients_species)

aggregated_data_with_info_sp <- 
  aggregated_data_with_info |>
  left_join(species_names) |>
  select(-species) |>
  rename(species = clean_species)

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
           depth_category, average_depth) |>
  mutate (n_quads = n_distinct(photo_id))|>
  # now calculate avg porportion
  group_by(site, subsite, year, year_cent, 
           depth_category, average_depth,
           class, species, gen_spp) |>
  summarize(n_quads = n_quads[1],
            proportion = sum(proportion)/n_quads[1]) |> 
  ungroup()

# check and make sure things sum to one
subsite_data |>
  group_by(site, subsite, year, year_cent, 
           depth_category, average_depth) |>
  summarize(total = sum(proportion)) |>
  pull(total)


##
# Add thermal preference data
##
thermal_data <- read_csv("data/Occurrence_based_species_thermal_indicies_Photos_100825.csv")

subsite_data_thermal <- 
  left_join(subsite_data, thermal_data)
##
# write out
##
write_csv(subsite_data, "data/substrate_data_subsite_long.csv")
