library(tidyverse)
library(Hmisc)

# pull database tables ---------------------------------------------------------
db_filepath <- here::here("data-raw", "feather-river-db.mdb")

mdb.get(db_filepath, tables = TRUE)

snorkel_obsv_early <- mdb.get(db_filepath, "SnorkObservationsTBL")
snorkel_survey_metadata_early <- mdb.get(db_filepath, "SnorkSurveyTBL")

lookup_HUC_cover <- mdb.get(db_filepath, "HUCcoverLU")
lookup_HUC_o_cover <- mdb.get(db_filepath, "HUCOcoverLU")
lookup_HUC_substrate <- mdb.get(db_filepath, "HUCsubstrateLU")
lookup_HUC_unit <- mdb.get(db_filepath, "HUCunitLU")
lookup_weather <- mdb.get(db_filepath, "WeatherLU")
lookup_species <- mdb.get(db_filepath, "OrganismCodeLU")
detach(package:Hmisc)

# write to csvs
write_csv(snorkel_obsv_early, here::here("data-raw", "raw_pre_2004_snorkel_data_feather.csv"))
write_csv(snorkel_survey_metadata_early, here::here("data-raw", "raw_pre_2004_snorkel_data_feather_metadata.csv"))

# read in csvs -----------------------------------------------------------------
# need this step to deal with "labeled" column types, update if we come up with a cleaner solution
snorkel_raw_early <- read_csv(here::here("data-raw", "raw_pre_2004_snorkel_data_feather.csv"))
snorkel_metadata_raw_early <- read_csv(here::here("data-raw","raw_pre_2004_snorkel_data_feather_metadata.csv"))


# Join tables to lookup and & clean --------------------------------------------
# Clean snorkel observations
cleaner_snorkel_data_early <- snorkel_raw_early |>
  janitor::clean_names() |>
  select(-bank_distance, -max_fl, -comments, -snorkler) |> # Remove size because post processing, duplication of FL, TODO check on lwd, remove comments
  left_join(lookup_species, by = c("species" = "OrganismCode")) |>
  select(-species) |>
  rename(species = CommonName, observation_id = obs_id,  hydrology = hu_cunit,
         instream_cover = huc_icover,
         overhead_cover = huc_ocover,
         substrate = hu_csubstrate) |>
  select(-c(Order1, Family, Genus, Species)) |>
  mutate(run = case_when(species == "Chinook Salmon- Fall" ~ "fall",
                         species == "Chinook Salmon- Late Fall" ~ "late fall",
                         species %in% c("Chinook Salmon - Spring", "Chjnook Salmon- Spring") ~ "spring",
                         TRUE ~ NA_character_),
         clipped = case_when(species == "Steelhead Trout (ad clipped)" ~ TRUE,
                             species == "Rainbow Trout (wild)" ~ FALSE),
         species = str_to_title(case_when(species %in% c("Chinook Salmon- Fall",
                                                         "Chinook Salmon- Late Fall",
                                                         "Chinook Salmon- Spring",
                                                         "Chjnook Salmon- Spring") ~ "Chinook Salmon",
                                          species %in% c("Rainbow Trout (wild)",
                                                         "Steelhead Trout (ad clipped)",
                                                         "Steelhead Trout - form not i.d.'d") ~ "O. Mykiss",
                                          species == "Unid Juvenile Sculpin" ~ "Unidentified Juvenile Sculpin",
                                          species == "Unid Juvenile Bass (Micropterus sp.)" ~ "Unidentified Juvenile Bass",
                                          species == "Unid Juvenile Lamprey" ~ "Unidentified Juvenile Lamprey",
                                          species == "Unid Juvenile Minnow" ~ "Unidentified Juvenile Minnow",
                                          species == "UNID Sunfish"   ~ "Unidentified Sunfish",
                                          species == "Unid Juvenile non-Micropterus Sunfish" ~ "Unidentified Juvenile non-Micropterus Sunfish",
                                          species == "Unid Juvenile Fish" ~ "Unidentified Juvenile Fish",
                                          species == "NO FISH CAUGHT" ~ NA,
                                          TRUE ~ species)),
         instream_cover = ifelse(is.na(instream_cover), NA, str_arrange(toupper(instream_cover))),
         instream_cover = case_when(instream_cover == "AG" ~ "A", # G is not an instream cover code, remove
                                    TRUE ~ instream_cover),
         hydrology = case_when(hydrology == "RM" ~ "Riffle Margin",
                               hydrology == "GM" ~ "Glide Margin",
                               hydrology == "W" ~ "Backwater",
                               hydrology == "G" ~ "Glide",
                               hydrology == "R" ~ "Riffle",
                               hydrology == "P" ~ "Pool",
                               hydrology == "M" ~ "Riffle Margin Eddy",
                               TRUE ~ hydrology)) |>
  filter(species != "Sacramento Squafish") |> glimpse()

cleaner_snorkel_data_early$overhead_cover |> unique()
cleaner_snorkel_data_early$hydrology |> unique()

cleaner_snorkel_metadata_early <- snorkel_metadata_raw |>
  janitor::clean_names() |>
  left_join(lookup_weather, by = c("weather" = "WeatherCode")) |>
  select(-c(visibility_comments, x_of_divers, x_of_center_passes, pass_width, comments,
            temp_time, snorkel_time_start, snorkel_time_stop, weather,
            snorkel_crew, shore_crew, recorder)) |> # doesn't seem like time information is being read in from mdb.get - TODO
  mutate(location = str_to_title(location),
         survey_type = str_to_lower(survey_type),
         section_type = str_to_lower(section_type),
         weather = str_to_lower(Weather)) |>
  select(-c(Weather)) |>
  glimpse()

