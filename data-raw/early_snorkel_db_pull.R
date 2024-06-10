library(tidyverse)
library(Hmisc)
library(stringr)
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

# Create helper function -------------------------------------------------------
# str_arrange created to arrange instream cover in alphabetical order
# reduces duplicates that are arranged differently
str_arrange <- function(x){
  x %>%
    stringr::str_split("") %>% # Split string into letters
    purrr::map(~sort(.) %>% paste(collapse = "")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}

# initial clean of site names
format_site_name <- function(string) {
  clean <-
    str_replace_all(string, "'", "") %>%
    str_replace_all("G-95", "G95") %>%
    str_replace_all("[^[:alnum:]]", " ") %>%
    trimws() %>%
    stringr::str_squish() %>%
    stringr::str_to_title()
}

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
         substrate = hu_csubstrate,
         fork_length = fl,
         count = number) |>
  select(-c(Order1, Family, Genus, Species)) |>
  mutate(run = case_when(species == "Chinook Salmon- Fall" ~ "fall",
                         species == "Chinook Salmon- Late Fall" ~ "late fall",
                         species %in% c("Chinook Salmon - Spring", "Chjnook Salmon- Spring") ~ "spring",
                         TRUE ~ NA_character_),
         clipped = case_when(species == "Steelhead Trout (ad clipped)" ~ TRUE,
                             species == "Rainbow Trout (wild)" ~ FALSE),
         species = tolower(case_when(species %in% c("Chinook Salmon- Fall",
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
                                          species == "Smallmouth Bass" ~ "Small Mouth Bass",
                                          species == "Largemouth Bass" ~ "Large Mouth Bass",
                                          species == "Sacramento Squawfish" ~ "Sacramento Pikeminnow",
                                          species %in% c("Sacramento Squawfish or Hardhead", "Pikeminnow/Hardhead") ~ "Sacramento Pikeminnow or hardhead",
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
                               TRUE ~ hydrology),
         overhead_cover = case_when(overhead_cover %in% c("e", "be", "b", "o", "O", "I") ~ NA,
                                    TRUE ~ as.numeric(overhead_cover)),
         unit = toupper(unit),
         unit = case_when(unit == "32A" ~ "32", # cleaning up these units because they are not in the snorkel_section_river_miles table
                          unit == "329.5" ~ "329",
                          unit == "255A" ~ "255",
                          unit ==  "229B" ~ "229",
                          unit == "323B                          323B" ~ "323B",
                          unit == "266A" ~ "266",
                          unit == "172B" ~ "172",
                          unit == "26A" ~ "26",
                          unit == "329B" ~ "329",
                          unit == "111A" ~ "111",
                          unit %in% c("274B", "274A") ~ "274",
                          unit == "448A" ~ "448",
                          unit == "271B" ~ "271",
                          unit == "273B" ~ "273",
                          unit %in% c("272A", "272B") ~ "272",
                          unit == "118A" ~ "118",
                          unit == "335B" ~ "335",
                          unit == "487B" ~ "487",
                          TRUE  ~ unit)) |>
  select(-run, -fish_depth, -adj_velocity) |>
  filter(species != "Sacramento Squawfish",
         !is.na(unit) # if there is no unit, it is not useful
         ) |> glimpse()

cleaner_snorkel_data_early$substrate |> unique()
cleaner_snorkel_data_early$hydrology |> unique()
cleaner_snorkel_data_early$species |> table()

# TODO, major issue with location / section_name. does not appear to be following any standard section naming conventions here
# TODO these section names still need to be cleaned up
# use unit lookup table and above units to clean up as we can
# Pull in cleaned name lookup table
raw_created_lookup <- readxl::read_excel("data-raw/snorkel_built_lookup_table.xlsx") |>
  mutate(section_name = ifelse(section_name == "Mo's Ditch", "Hatchery Ditch", section_name)) |> #Decided to change Mo's Ditch for unit 28 being consistent with map, but not slides (no Mo's Ditch, but located in "Hatchery Ditch)
  glimpse()

units_per_survey  <- cleaner_snorkel_data_early |>
  select(survey_id, unit) |>
  left_join(raw_created_lookup) |>
  select(survey_id, updated_section_name = section_name, section_number) |>
  filter(!is.na(updated_section_name)) |>
  distinct() |>
  glimpse()

# clean snorkel metadat
cleaner_snorkel_metadata_early <- snorkel_metadata_raw_early |>
  janitor::clean_names() |>
  left_join(lookup_weather, by = c("weather" = "WeatherCode")) |>
  select(-c(visibility_comments, x_of_divers, x_of_center_passes, pass_width, comments,
            temp_time, snorkel_time_start, snorkel_time_stop, weather,
            snorkel_crew, shore_crew, recorder, survey_type, section_type)) |>
  mutate(location = str_to_title(location),
         Weather = str_to_lower(Weather)) |>
  rename(flow = river_flow, units_covered = units,
         section_name = location,
         weather = Weather) |>
  left_join(units_per_survey) |>
  mutate(section_name = ifelse(!is.na(updated_section_name), updated_section_name, section_name)) |>
  select(-updated_section_name) |>
  # update additional section_names, concern with manual update is that section_names will be associated with units not contained within that section...
  # mutate(section_name = case_when())
  glimpse()

# still more clean up to do
# Initially ~ 200 unique section names, after join with units_per_survey table we get down to ~100
cleaner_snorkel_metadata_early$section_name |> unique() |> sort()



