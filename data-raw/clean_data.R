library(tidyverse)
library(googleCloudStorageR)
library(sf)
source("data-raw/metadata/species_lookup.R")


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
# TODO Section name updates/questions
unique(combined_snorkel$section_name)
# - we updated Bigriffle to be Big Riffle, is that accurate?)
# - ? what to do about the section names that include multiple "hatchery and mo's riffles"
# - do you have a list of ones you currently use? Do you have a map of sites?

# SECTION TYPE -----------------------------------------------------------------
# TODO ?
unique(combined_snorkel$section_type) # what does this mean? Do we need to keep? should we only use permanant sections for analysis
# all permanent and random sectin types have NA section names and numbers...not useful
# Suggest that we remove
combined_snorkel |> filter(section_type == "permanent") |> pull(section_name) |> unique()
combined_snorkel |> filter(section_type == "permanent") |> pull(section_number) |> unique()

combined_snorkel |> filter(section_type == "random") |> pull(section_name) |> unique()
combined_snorkel |> filter(section_type == "random") |> pull(section_number) |> unique()


combined_snorkel |> filter(is.na(section_type)) |> pull(section_name) |> unique()
combined_snorkel |> filter(is.na(section_type)) |> pull(section_number) |> unique()

# UNITS COVERED -----------------------------------------------------------------
# TODO can we remove? seems more like metadata then data for a given reccord
# # are these ones covered in a survey? do we need both this and unit"
# Suggest removing
unique(combined_snorkel$units_covered)

combined_snorkel |>
  mutate(has_units_covered = ifelse(is.na(units_covered), FALSE, TRUE)) |>
  ggplot(aes(x = date, y = has_units_covered)) +
  geom_point()

# UNIT -----------------------------------------------------------------
# TODO What is the definition of this?
unique(combined_snorkel$unit) #check field meaning, why can there be two units?

combined_snorkel |>
  mutate(has_units = ifelse(is.na(unit), FALSE, TRUE)) |>
  ggplot(aes(x = date, y = has_units)) +
  geom_point()

# size class -----------------------------------------------------------------
# TODO ?
#  What is this? Fish or other size class? do you have the lookup for this? What do these correspond to?
unique(combined_snorkel$size_class)

# instream cover -----------------------------------------------------------------
unique(combined_snorkel$instream_cover) # confirm metadata has code lookup

unique(combined_snorkel$hydrology)
# TODO used unit type pre 2005 and hydrology post, called them hydrology
unique(combined_snorkel$unit_type) #still unsure about different between this field and hydrology
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
# TODO - are there clipped trout? If so is this RBTC? Assumed that but can remove?
unique(combined_snorkel$clipped)

# overhead cover -----------------------------------------------------------------
unique(combined_snorkel$overhead_cover)

# location -----------------------------------------------------------------
# TODO questions?
# how is this different from section name, seems maybe like it is more specific
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
# TODO questions
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
# TODO - size and fork length? do we need to keep them both, which one should we keep
# they do not have quite the same distribution
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
# TODO what are the units here?
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
                             TRUE ~ species))) |> glimpse()

cleaned_combined_snorkel$species |> unique()
# TODO - Badhia, additional updates
# discussed with Casey, remove the following from final tables: size_class, est_size, bank_dist (more than 95% na, no longer used), run
# clean up the following columns: hydrology - w: backwater, g: glide
# decision to split data into 3 tables: survey_characteristics, site_lookup, fish_observations

# characteristics ----
# Each survey id coresponds to a single section_name so including section name in this table
survey_characteristics <- cleaned_combined_snorkel |>
  select(survey_id, date, flow, weather, turbidity, start_time, end_time,
         section_name, units_covered, unit, visibility, temperature, survey_type, hydrology) |>
  distinct() |>
  glimpse()

# TODO / concerns
# Lots of section_name NA especially in early reccord. Not sure how to figure out location of these surveys
# For now, adding unit to ensure we can keep spatial info, plan to try and remove and keep this info but allow spatial linking through location table

# site_lookup -----
# Goal of site lookup is to have a location lookup table. There should be 1 row for each "unit".
# We want the following columns: section_name, section_number, unit, unit_type, section_type (permanent or random)
# we can assign section_type based on the metadata we have about each section. If section number 1-20, assign section_type = "permanent" else assign "random"
# If "random" is there a way we can figure out where these units are located

#reading in KMZ file
lat_long_file <- st_read("data-raw/Snorkel_Survey_Locations.kml")

coords <- st_coordinates(lat_long_file$geometry)
# Convert the coordinates to a data frame
coords_df <- as.data.frame(coords)
colnames(coords_df) <- c("longitude", "latitude")

class(coords_df)
str(coords_df)


#creating lookup table
site_lookup <- cleaned_combined_snorkel |>
  select(section_name, section_number, unit) |>
  distinct() |>
  mutate(section_type = case_when(between(section_number, 1, 20) ~ "permanent", TRUE ~ "random")) |>
  glimpse()

#facts: there are 499 different units and 39 different section_names

#TODO create a "first lookup" with section_name section_number, unit (add sample_type == permanent)

#approach 2 - trying another lookup table format
site_lookup_test <- site_lookup |>
  select(c(unit, section_name, section_type)) |>
  distinct() |>
  glimpse()



# fish_observations -----
fish_observations <- cleaned_combined_snorkel |>
  select(survey_id, date, unit, species, count, fork_length, substrate, instream_cover, overhead_cover, water_depth_m, tagged, clipped) |> glimpse()

# write files -------------------------------------------------------------

# save cleaned data to `data/`
write_csv(cleaned_combined_snorkel, here::here("data", "combined_feather_snorkel_data.csv"))

