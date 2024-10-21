library(tidyverse)

# Read in data files from the earlier database (pre 2004) which are created in "early_snorkel_db_pull.R"
cleaner_snorkel_metadata_early <- read_csv("data/cleaner_snorkel_metadata_early.csv")
cleaner_snorkel_data_early <- read_csv("data/cleaner_snorkel_data_early.csv")
# source script that prepares cleaner data from the current database. This needs to be rerun when data are added
source("data-raw/scripts_for_updating_EDI/revised_snorkel_db_pull.R")

# # look at data early
# glimpse(cleaner_snorkel_data_early)
# glimpse(cleaner_snorkel_metadata_early)
#
#
# # look at data revised
# glimpse(cleaner_snorkel_observations)
# glimpse(cleaner_snorkel_survey_metadata)
#
#
# # snorkel obs data
# sort(colnames(cleaner_snorkel_data_early))
# sort(colnames(cleaner_snorkel_observations))
# # columns: observation_id, survey_id, unit, count, fork_length, substrate, instream_cover, overhead_cover, hydrology, depth, species, clipped
# # TODO column discrepancies - velocity only in early data
#
# # snorkel survey metadata data
# # columns: survey_id, date, flow, section_name, section_number, survey_id, temperature, turbidity, units_covered, weather
# # TODO: survey_names are still a mess in the early data
# sort(colnames(cleaner_snorkel_metadata_early))
# sort(colnames(cleaner_snorkel_survey_metadata))
#
# # unit lookup - table to join sampling unit to river mile/section/section type/channel
# sampling_unit_lookup |> glimpse()

# FEATHER SNORKEL METADATA -----------------------------------------------------
# in the historical data there are 41 records where there are duplicate survey_ids
# they are duplicate because they have different section names. the section name
# data is pretty messy and not useful so decided to remove the section name and number
# from this metadata

# update the section_type field. we are not sure what this field means for the historical data. we decided to
# update this field using the following logic, if a survey on a given day includes units in this table then they are permanent,
# if not then random. This logic only applies to 2015 and onwards. For years before 2015 we will have to make NA.

# FlowWest created a lookup table that includes unit, section_name, section_number and section_type based on
# the map book (see "new_snork_maps.ppt") from Casey Campos. This lookup only includes units and sections
# that were permanent. Random sections/units are not included
# We just use section_type from this file
snorkel_built_lookup <- readxl::read_excel(here::here('data-raw', 'processed-tables', 'snorkel_built_lookup_table.xlsx')) |>
    select(-section_name) |>
    glimpse()

combined_snorkel_metadata <- bind_rows(cleaner_snorkel_metadata_early |>
                                         mutate(database = "historical"),
                                       cleaner_snorkel_survey_metadata |>
                                         mutate(database = "current")) |>
  select(-section_type) |>
  left_join(snorkel_built_lookup |>
              select(section_number, section_type) |>
              distinct()) |>
  mutate(survey_id = paste0(survey_id, "_", database),
         section_type = ifelse(year(date) >= 2015 & is.na(section_type), "random", section_type),
         survey_type = ifelse(year(date) >= 2001 & is.na(survey_type), "unit", survey_type)) |>
  select(survey_id, date, survey_type, section_type, flow, weather, turbidity, temperature, visibility)

# combined_snorkel_metadata |> group_by(survey_id) |> tally() |> filter(n > 1)
# combined_snorkel_metadata$date |> summary() # Data from April 1999 - July 2023
# combined_snorkel_metadata$flow |> summary()
# #combined_snorkel_metadata$section_name |> table() # TODO section name is still messy, I think this is all coming from the location field in early table; decided to remove
# #combined_snorkel_metadata$units_covered |> table() # messy but not sure if there is anything to do with this
# combined_snorkel_metadata$turbidity |> summary()
# combined_snorkel_metadata$temperature |> summary()
# combined_snorkel_metadata$weather |> table()

# combine and clean
# FEATHER SNORKEL OBS ----------------------------------------------------------
combined_snorkel_observations <- bind_rows(cleaner_snorkel_data_early |>
                                             mutate(instream_cover = as.character(instream_cover),
                                                    database = "historical"
                                                    ),
                                           cleaner_snorkel_observations |>
                                             mutate(database = "current")
                                             ) |>
  mutate(survey_id = paste0(survey_id, "_", database)) |>
  filter(!unit %in% c("77-80", "86-89",
                      "104, 106, 112", "104 106  112",
                      "104 106 112", "446/449")) |>
  mutate(species = case_when(observation_id %in% c(16208, 16207) ~ 'chinook salmon', # change z.nada to chinook for these two observations per Casey comment
                             .default = as.character(species)),
         channel_geomorphic_unit = tolower(channel_geomorphic_unit),
         count = ifelse(is.na(count), 0, count)) |> # if count is NA, changed to zero
  # run is all NA so removed
  left_join(combined_snorkel_metadata |>
              select(survey_id, date)) |>
  filter(!is.na(date)) |> # filter out NA dates because not useful
  select(observation_id, survey_id, date, unit, count, species, fork_length, size_class, clipped, substrate, instream_cover, overhead_cover,
         channel_geomorphic_unit, depth, velocity) |>
  glimpse() # filtered out these messy units for now, alternatively we can see if casey can assign a non messy unit

# combined_snorkel_observations$unit |> unique() |> length() #395 in this
# combined_snorkel_observations$count |> summary() # 750000 is high, leaving in for now but noting
# combined_snorkel_observations$fork_length |> summary() # a few big ones here as well but could be possible
# combined_snorkel_observations$substrate |> table() # still a lot of variation in substrate but ordered and removed weird values
# combined_snorkel_observations$overhead_cover |> table() # removed letterd variables because no lookup (only < 10 of these, so not a big removal, they were in the early data)
# combined_snorkel_observations$instream_cover |> table() # looks as good as we are going to get
# combined_snorkel_observations$channel_geomorphic_unit |> table() # clean
# combined_snorkel_observations$velocity |> summary() # Only exists for early data, lots of NAs
# combined_snorkel_observations$depth |> summary() # clean
# combined_snorkel_observations$species |> sort() |> unique() # still a lot of species, could potentially refine some of the unidentified ones...or get. abiologiest to imrpve, but probably fine for now
# combined_snorkel_observations$clipped |> table() # clean
# combined_snorkel_observations$size_class |> table(useNA = "always")


# exploratory on units_covered
# decision: remove units_covered
# unit_eda <- combined_snorkel_metadata |>
#   # separate_rows(units_covered, sep = ",") |>
#   # separate_rows(units_covered, sep = " ") |>
#   # separate_rows(units_covered, sep = "-") |>
#   left_join(combined_snorkel_observations) |>
#   select(survey_id, date, count, units_covered, unit)
#
# unit_eda |>
#   filter(!is.na(unit) & count > 0) |>
#   View()
#
# unit_eda |>
#   filter(!is.na(unit) & count == 0) |>
#   View()

# write csv for fish_observations
write_csv(combined_snorkel_observations, "data/fish_observations.csv")

combined_snorkel_metadata_na_rm <- combined_snorkel_metadata |>
  filter(!is.na(date))
# write csv for survey_characteristics
write_csv(combined_snorkel_metadata_na_rm, "data/survey_characteristics.csv")

# FEATHER LOCATION LOOKUP TABLE ------------------------------------------------

# pull in coordinates of river miles
# river_mile_coordinates <- readxl::read_xlsx("data-raw/processed-tables/featherrivermile_coordinates.xlsx")
# location_lookup <- sampling_unit_lookup_coordinates |>
#   select(section_name, section_type, channel_type, section_number, unit, river_mile, latitude, longitude) |>
#   left_join(river_mile_coordinates |>
#               rename(river_mile = RIVER_MILE) |>
#               mutate(river_mile = as.numeric(river_mile)) |>
#               select(river_mile, Latitude, Longitude)) |>
#   mutate(latitude = ifelse(is.na(latitude), Latitude, latitude),
#          longitude = ifelse(is.na(longitude), Longitude, longitude),
#          unit_sub_level = case_when(grepl("A", unit) ~ "A",
#                                     grepl("B", unit) ~ "B"),
#          unit = case_when(unit == "215A" ~ "215",
#                           unit == "215B" ~ "215",
#                           unit == "323A" ~ "323",
#                           unit == "323B" ~ "323",
#                           unit == "29A" ~ "29",
#                           unit == "31A" ~ "31",
#                           unit == "121A" ~ "121",
#                           unit == "30A" ~ "30",
#                           T ~ unit),
#          section_name = case_when(unit == "28" ~ "moe's side channel",
#                                   T ~ section_name)) |>
#   select(-c(Latitude, Longitude))

#filter(location_lookup, grepl("A", unit) | grepl("B", unit))

# Katie Lentz at DWR digitized the survey locations. Pulling this in and combining with information we have about location
digital_units <- read_csv("data-raw/processed-tables/Snorkel_Centroid_ExportFeatures_TableToExcel.csv")

full_location_lookup <- digital_units |>
                  mutate(unit = as.character(unit)) |>
  left_join(river_mile_lookup |> # join with the river mile lookup table from the revised snorkel db that includes the channel type (e.g. LFC/HFC)
              rename(unit = Snorkel.Sections,
                     channel_type = Channel,
                     river_mile = River.Mile)) |>
  select(unit, unit_sub_level, channel_type, river_mile, area_sq_m, latitude, longitude)


# min(full_location_lookup$area_sq_m, na.rm = T)
# max(full_location_lookup$area_sq_m, na.rm = T)
# write csv
write_csv(full_location_lookup, "data/locations_lookup.csv")


# Data checks -------------------------------------------------------------
# Summarize the number that are missing coordinates
# combined <- read_csv("data/fish_observations.csv")
# metadata <- read_csv("data/survey_characteristics.csv")
#
# # note that when join metadata to attach the date need to select distinct
#
# ck <- combined |>
#   left_join(location_lookup) |>
#   left_join(metadata |>
#               select(survey_id, date) |>
#               distinct())
#
#
# number_random <- ck |>
#   filter(is.na(section_name), section_type == "random") |>
#   distinct(date)
#
# number_random |>
#   group_by(year(date)) |>
#   tally()
#
# # number without river mile (filled in lat/long for some of these)
# no_river_mile <- filter(ck, is.na(river_mile)) |>
#   distinct(date, .keep_all = T)
#
# # number without coordinates
#
# no_coordinate <- filter(ck, is.na(latitude)) |>
#   distinct(date, .keep_all = T)

#LWD
# lwd_dataset <- ck |>
#   select(date, section_name, lwd) |>
#   filter(!is.na(lwd) & !is.na(section_name)) |>
#   distinct() |>
#   arrange(date)
