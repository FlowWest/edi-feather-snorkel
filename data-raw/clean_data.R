library(tidyverse)
library(googleCloudStorageR)
library(sf)
source("data-raw/metadata/species_lookup.R")
library(readxl)

# google cloud set up
gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))
gcs_global_bucket(bucket = Sys.getenv("GCS_DEFAULT_BUCKET"))

# get data from google cloud
gcs_get_object(object_name = "juvenile-rearing-monitoring/seine-and-snorkel-data/feather-river/data/combined_feather_snorkel_data.csv",
               bucket = gcs_get_global_bucket(),
               saveToDisk =  here::here("data-raw", "combined_feather_snorkel_data.csv"),
               overwrite = TRUE)

combined_snorkel <- read_csv(here::here("data-raw", "combined_feather_snorkel_data.csv")) |> glimpse()

# clean data --------------------------------------------------------------
# here is where we clean up the data and make sure it all looks as expected
# check unique values for each column
# check that all fields are being read in the right way

summary(combined_snorkel)
glimpse(combined_snorkel)

# WEATHER ----------------------------------------------------------------------
#character variables ----
unique(combined_snorkel$weather) # fine as is

# SECTION NAME -----------------------------------------------------------------
unique(combined_snorkel$section_name)
# still some messy section names but we address with a Lookup Table Below

# SECTION TYPE -----------------------------------------------------------------
unique(combined_snorkel$section_type)
# Suggest that we remove, will add a new section_type into lookup
combined_snorkel |> filter(section_type == "permanent") |> pull(section_name) |> unique()
combined_snorkel |> filter(section_type == "permanent") |> pull(section_number) |> unique()

combined_snorkel |> filter(section_type == "random") |> pull(section_name) |> unique()
combined_snorkel |> filter(section_type == "random") |> pull(section_number) |> unique()


combined_snorkel |> filter(is.na(section_type)) |> pull(section_name) |> unique()
combined_snorkel |> filter(is.na(section_type)) |> pull(section_number) |> unique()

# UNITS COVERED ----------------------------------------------------------------
unique(combined_snorkel$units_covered)

combined_snorkel |>
  mutate(has_units_covered = ifelse(is.na(units_covered), FALSE, TRUE)) |>
  ggplot(aes(x = date, y = has_units_covered)) +
  geom_point()

# UNIT -----------------------------------------------------------------
unique(combined_snorkel$unit) #check field meaning, why can there be two units?

combined_snorkel |>
  mutate(has_units = ifelse(is.na(unit), FALSE, TRUE)) |>
  ggplot(aes(x = date, y = has_units)) +
  geom_point()

# size class -----------------------------------------------------------------
# Remove size class
unique(combined_snorkel$size_class)

# instream cover -----------------------------------------------------------------
unique(combined_snorkel$instream_cover) # confirm metadata has code lookup

# hydrology & unit_type --------------------------------------------------------
unique(combined_snorkel$hydrology)
# Combined below, since they are the same just collected at different times (see plot below)
unique(combined_snorkel$unit_type)
sum(combined_snorkel$unit_type == combined_snorkel$hydrology, na.rm = TRUE)

combined_snorkel |> ggplot(aes(x = date, y = hydrology, color = hydrology)) +
  geom_point()
combined_snorkel |> ggplot(aes(x = date, y = unit_type, color = unit_type)) +
  geom_point()

# run -----------------------------------------------------------------
unique(combined_snorkel$run)

# tagged -----------------------------------------------------------------
unique(combined_snorkel$tagged)

# clipped -----------------------------------------------------------------
unique(combined_snorkel$clipped)

# overhead cover -----------------------------------------------------------------
unique(combined_snorkel$overhead_cover)

# location -----------------------------------------------------------------
# removed location, messy and not providing additional helpful context
unique(combined_snorkel$location)

# survey_type -----------------------------------------------------------------
unique(combined_snorkel$survey_type) #check field meaning, no response in email on what comp was, PROPOSE REMOVE (OKAY?)

# species -----------------------------------------------------------------
# Mapping to lookup codes from database in cleaned table below
unique(combined_snorkel$species)

# substrate --------------------------------------------------------------------------
unique(combined_snorkel$substrate) #keeping substrate as character since numbers are referring to code, multiple codes at once in some fields

# survey_id --------------------------------------------------------------------------
#numeric variables ----
summary(combined_snorkel$survey_id)

# Date --------------------------------------------------------------------------
# TODO Update with new database from Casey
# we are missing 2021 - 2024, should we get this data to add in?
range(combined_snorkel$date)

# Flow --------------------------------------------------------------------------
summary(combined_snorkel$flow)
ggplot(combined_snorkel, aes(flow)) +
  geom_histogram()
combined_snorkel$flow <- ifelse(combined_snorkel$flow == 0, NA, combined_snorkel$flow) |> #changing flow values from 0 to NA
  glimpse()

# section number --------------------------------------------------------------------------
# TODO - we created this field based on section number in DMP, is this accurate mapping
# section_number = case_when(section_name == "Aleck Riffle" ~ 8,
# section_name == "Auditorium Riffle" ~ 4,
# section_name == "Bedrock Park Riffle" ~ 5,
# section_name == "Bedrock Riffle" ~ 10,
# section_name == "Big Riffle" ~ 17,
# section_name == "Eye Riffle" ~ 11,
# section_name == "G95" ~ 14,
# section_name == "Gateway Riffle" ~ 12,
# section_name == "Goose Riffle" ~ 16,
# section_name == "Gridley Riffle" ~ 19,
# section_name == "Hatchery Ditch" ~ 2,
# section_name == "Hatchery Riffle" ~ 1,
# section_name == "Junkyard Riffle" ~ 20,
# section_name == "Kiester Riffle" ~ 15,
# section_name == "Matthews Riffle" ~ 7,
# section_name == "McFarland" ~ 18,
# section_name == "Mo's Ditch" ~ 3,
# section_name == "Robinson Riffle" ~ 9,
# section_name == "Steep Riffle" ~ 10,
# section_name == "Trailer Park Riffle" ~ 6,
# section_name == "Vance Riffle" ~ 13,
# TRUE ~ NA))
summary(combined_snorkel$section_number)

# Turbidity --------------------------------------------------------------------------
summary(combined_snorkel$turbidity)
ggplot(combined_snorkel, aes(turbidity)) +
  geom_histogram()

# Temp --------------------------------------------------------------------------
summary(combined_snorkel$temperature)
ggplot(combined_snorkel, aes(x = date, y = temperature)) +
  geom_point()
combined_snorkel$temperature <- ifelse(combined_snorkel$temperature == 0, NA, combined_snorkel$temperature) |> #changing flow values from 0 to NA since they are potential outliers
  glimpse()

# Time (start and end) --------------------------------------------------------------------------

head(combined_snorkel$end_time[5:10])
head(combined_snorkel$start_time[5:10])
summary(combined_snorkel$count)
ggplot(combined_snorkel, aes(count)) +
  geom_histogram()

# Est size --------------------------------------------------------------------------
# Remove est size
summary(combined_snorkel$est_size)
ggplot(combined_snorkel, aes(est_size, fill = species)) +
  geom_histogram()


# Fork length --------------------------------------------------------------------------
summary(combined_snorkel$fork_length)
ggplot(combined_snorkel, aes(fork_length, fill = species)) +
  geom_histogram()

# Date --------------------------------------------------------------------------
summary(combined_snorkel$water_depth_m)
ggplot(combined_snorkel, aes(water_depth_m)) +
  geom_histogram()

# Bank distance  --------------------------------------------------------------------------
# remove, no longer collected
summary(combined_snorkel$bank_distance)

# visibility --------------------------------------------------------------------------
summary(combined_snorkel$visibility)



cleaned_combined_snorkel <- combined_snorkel |>
  mutate(instream_cover = toupper(instream_cover),
         hydrology = ifelse(year(date) > 2005, hydrology, unit_type),
         hydrology = recode(hydrology, "g" = "glide", "w" = "backwater")) |>
  select(-unit_type) |>
  left_join(species_lookup, by = c("species" = "OrganismCode")) |>
  select(-species) |>
  rename(species = CommonName) |>
  select(-c(Order1, Family, Genus, Species)) |>
  mutate(run = case_when(species == "Chinook Salmon- Fall" ~ "fall",
                        species == "Chinook Salmon- Late Fall" ~ "late fall",
                        species %in% c("Chinook Salmon - Spring", "Chjnook Salmon- Spring") ~ "spring",
                        TRUE ~ NA_character_),
         clipped = case_when(species == "Steelhead Trout (ad clipped)" ~ TRUE,
                             species == "Rainbow Trout (wild)" ~ FALSE,
                             TRUE ~ clipped),
         species = str_to_title(case_when(species %in% c("Chinook Salmon- Fall",
                                         "Chinook Salmon- Late Fall",
                                         "Chinook Salmon- Spring",
                                         "Chjnook Salmon- Spring") ~ "Chinook Salmon",
                             species =="Rainbow Trout (wild)" ~ "Rainbow Trout",
                             species == "Steelhead Trout (ad clipped)" ~ "Steelhead Trout",
                             species == "Unid Juvenile Sculpin" ~ "Unidentified Juvenile Sculpin",
                             species == "Unid Juvenile Bass (Micropterus sp.)" ~ "Unidentified Juvenile Bass",
                             species == "Unid Juvenile Lamprey" ~ "Unidentified Juvenile Lamprey",
                             species == "Unid Juvenile Minnow" ~ "Unidentified Juvenile Minnow",
                             species == "UNID Sunfish"   ~ "Unidentified Sunfish",
                             species == "Unid Juvenile non-Micropterus Sunfish" ~ "Unidentified Juvenile non-Micropterus Sunfish",
                             species == "Unid Juvenile Fish" ~ "Unidentified Juvenile Fish",
                             species == "NO FISH CAUGHT" ~ NA,
                             TRUE ~ species))) |>
  select(-c(bank_distance, est_size, size_class, location, survey_type)) |>
   filter(!unit %in% c("77-80", "86-89",
                    "104, 106, 112", "104 106  112",
                    "104 106 112", "446/449", "323b                          323b"
                    )) |> # remove all duplicates
  mutate(
    # unit = as.numeric(gsub("([0-9]+).*$", "\\1", unit)),
         unit = toupper(unit),
         section_name = case_when(
           section_name == "Bedrock Park Riffle" ~ "Bedrock Riffle",
           section_name == "Mcfarland" ~ "McFarland",
           section_name == "Trailer Parkk" ~ "Trailer Park Riffle",
           section_name == "Gridley S C Riffle" ~ "Gridley Riffle",
           section_name == "Vance" ~ "Vance Riffle",
           section_name %in% c("Big Riffle Downstream Rl", "Bigriffle") ~ "Big Riffle",
           section_name %in% c("Mo's Ditch", "Moes Side Channel", "Hatchery And Mo's Riffles",
                               "Hatchery Ditch And Mo's Ditch", "Upper Hatchery Ditch",
                               "Hatchery And Moes Ditches") ~ "Hatchery Ditch", TRUE ~ section_name),
         section_type = ifelse(section_number %in% c(1:20), "permanent", "random")) |>
  filter(!is.na(unit)) |> glimpse() # this filter and the filter to remove multiple units looses 560 reccods,
# TODO allowing for now given thoes values cannot be spatially linked anywhere, but confirm and check

cleaned_combined_snorkel$species |> unique()

# decision to split data into 3 tables: survey_characteristics, site_lookup, fish_observations

# characteristics ----
# Each survey id corresponds to a single section_name so including section name in this table
# TODO - erin to check with Ashley on structure here
# # Lots of section_name NA especially in early reccord. Not sure how to figure out location of these surveys
# For now, adding unit to ensure we can keep spatial info, plan to try and remove and keep this info but allow spatial linking through location table
# Basically looks like 2015 and beyond, the data is good, before lots of NAs
# Lots of obs where 1 unit is assigned 2 hydrology designations on the same day...
survey_characteristics <- cleaned_combined_snorkel |>
  select(survey_id, date, flow, weather, turbidity, start_time, end_time,
         section_name, units_covered, unit, visibility, temperature, hydrology) |>
  distinct() |>
  glimpse()

# site_lookup ------------------------------------------------------------------
# TODO add in river mile from updated snorkel revised, this will map unit to a river mile

#reading in xlsx created based on slides and dmp
#section names: bedrock riffle might show as bedrock park riffle. Upper/Lower McFarland are both same section_number so keeping it ad "McFarland"
raw_created_lookup <- readxl::read_excel("data-raw/snorkel_built_lookup_table.xlsx") |>
  mutate(section_name = ifelse(section_name == "Mo's Ditch", "Hatchery Ditch", section_name)) |> #Decided to change Mo's Ditch for unit 28 being consistent with map, but not slides (no Mo's Ditch, but located in "Hatchery Ditch)
  glimpse()

# not_listed <- anti_join(site_lookup_fields, raw_created_lookup, by = "unit") #finding those "unit" that are not in the raw_created_lookup table
# not_listed |>
#   select(c(unit, section_name, section_type)) |> #showing only fields of interest
#   glimpse()

#joining both tables by "unit"
#changing names to identify source of field clearly. created lookup_table are known data
# created_lookup <- raw_created_lookup |>
#   rename(known_section_name = section_name,
#          known_section_number = section_number)
#
# site_lookup_raw <- site_lookup_fields |>
#   rename(unknown_section_name = section_name,
#          unknown_section_number = section_number) |>
#   glimpse()
#
# lookup_join <- inner_join(site_lookup_raw, created_lookup, by = "unit") |>
#   mutate(unknown_section_name = ifelse(unknown_section_name == known_section_name & unknown_section_number == known_section_number, NA, unknown_section_name),
#          unknown_section_number = ifelse(unknown_section_name == known_section_name & unknown_section_number == known_section_number, NA, unknown_section_number)) |> #setting to NA those section_name, section_number that are consistent
#   glimpse()
#
# #identifying inconsistencies between data given and created built lookup table based on map and slides
# # six reccords that are inconsistant, filter out of the "raw site lookup table)
# inconsistent <- lookup_join |>
#   filter(!is.na(unknown_section_number)) |>  #deleting those that are NA (since those are consistent)
#   filter(unknown_section_name != "Hatchery Ditch") |>  #keeping this one out since we identified the issue
#   glimpse()


# filter out permanant units from raw survey data to provide only the "random" units that are inconsistently sampled
random_sampling_units <- cleaned_combined_snorkel |>
  select(section_name, section_number, unit, section_type) |>
  distinct() |>
  filter(!unit %in% c(raw_created_lookup$unit)) |>
  # filter(section_type == "random") |>
  distinct() |> glimpse()

# One row in test showing up as random, update or fix metadata table
random_sampling_units |>
  filter(!unit %in% c(raw_created_lookup$unit)) |>
  filter(section_type == "permanent") |> glimpse()
# according to map, no 227 in "Gateway Riffle" remove below
raw_created_lookup |> filter(section_number == 12) |> glimpse()

clean_random_sampling_units <- random_sampling_units |>
  filter(unit != "227") |> glimpse()

# ALL random now so that looks good
clean_random_sampling_units$section_type |> unique()

#final lookup table with all units
# TODO/Note - the majority of these (88%) are still "orphaned" units that have no grounding in space currently...
lookup_table <- bind_rows(clean_random_sampling_units, raw_created_lookup) |>
  glimpse()

river_mile_lookup <- read_csv("data-raw/river_miles_lookup.csv") |> glimpse()

sites_with_river_miles <- left_join(lookup_table, river_mile_lookup, by = c("unit" = "Snorkel.Sections")) |>
  glimpse()

# fish_observations ------------------------------------------------------------
# TODO EXISTING TODOS
# Database does not give definition for cover = D, figure out what that is
# Database does not give definition for substrate = 6, figure out what that is
fish_observations <- cleaned_combined_snorkel |>
  select(survey_id, date, unit, species, count, fork_length, substrate, instream_cover, overhead_cover, water_depth_m, tagged, clipped) |> glimpse()

# explore fish obs for issues
fish_observations |> filter(is.na(count)) |> View()

# big gap where they did not collect substrate/cover data between 2004 - 2011
fish_observations |> ggplot(aes(x=date, y = instream_cover, color = instream_cover)) +
  geom_point()

# what is D? Assuming R and G are typos and will remove below
fish_observations$instream_cover |> table()

fish_observations$substrate |> table()

# overhead cover codes go from 0 - 3, removing codes above that have other numbers in them
fish_observations |> ggplot(aes(x=date, y = overhead_cover, color = overhead_cover)) +
  geom_point()

fish_observations$overhead_cover |> table()

# create clean version of table
cleaned_fish_observations <- fish_observations |>
  filter(!is.na(count)) |> # there are a few of these associated with chinook salmon, but these do not have any habitat data associated with them
  mutate(overhead_cover = ifelse(overhead_cover == 4, NA, overhead_cover),
         instream_cover = case_when(instream_cover == "R" ~ NA,
                                    instream_cover == "0" ~ "A", # Assuming by 0 they mean "No Apparent Cover - A"
                                    instream_cover == "BG" ~ NA,
                                    TRUE ~ instream_cover)) |>
  glimpse()


# write files -------------------------------------------------------------

# save cleaned data to `data/`
write_csv(survey_characteristics, here::here("data", "survey_characteristics_feather_snorkel_data.csv"))
write_csv(lookup_table, here::here("data", "site_lookup_table_feather_snorkel_data.csv"))
write_csv(cleaned_fish_observations, here::here("data", "fish_observations_feather_snorkel_data.csv"))
