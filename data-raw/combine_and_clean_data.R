library(tidyverse)

source("data-raw/early_snorkel_db_pull.R")
source("data-raw/revised_snorkel_db_pull.R")


# look at data early
glimpse(cleaner_snorkel_data_early)
glimpse(cleaner_snorkel_metadata_early)


# look at data revised
glimpse(cleaner_snorkel_observations)
glimpse(cleaner_snorkel_survey_metadata)


# snorkel obs data
sort(colnames(cleaner_snorkel_data_early))
sort(colnames(cleaner_snorkel_observations))
# columns: observation_id, survey_id, unit, count, fork_length, substrate, instream_cover, overhead_cover, hydrology, depth, species, clipped
# TODO column discrepancies - velocity only in early data

# snorkel survey metadata data
# columns: survey_id, date, flow, section_name, section_number, survey_id, temperature, turbidity, units_covered, weather
# TODO: survey_names are still a mess in the early data
sort(colnames(cleaner_snorkel_metadata_early))
sort(colnames(cleaner_snorkel_survey_metadata))

# unit lookup - table to join sampling unit to river mile/section/section type/channel
sampling_unit_lookup |> glimpse()

# combine and clean
# FEATHER SNORKEL OBS ----------------------------------------------------------
combined_snorkel_observations <- bind_rows(cleaner_snorkel_data_early |> mutate(instream_cover = as.character(instream_cover)),
                                           cleaner_snorkel_observations) |>
  filter(!unit %in% c("77-80", "86-89",
                      "104, 106, 112", "104 106  112",
                      "104 106 112", "446/449"), # filtered out these messy units for now, alternatively we can see if casey can assign a non messy unit
         species != "Sacramento Squawfish Or Hardhead") |> glimpse()

combined_snorkel_observations$unit |> unique() |> length() #395 in this
combined_snorkel_observations$count |> summary() # 750000 is high, leaving in for now but noting
combined_snorkel_observations$fork_length |> summary() # a few big ones here as well but could be possible
combined_snorkel_observations$substrate |> table() # still a lot of variation in substrate but ordered and removed weird values
combined_snorkel_observations$overhead_cover |> table() # removed letterd variables because no lookup (only < 10 of these, so not a big removal, they were in the early data)
combined_snorkel_observations$instream_cover |> table() # looks as good as we are going to get
combined_snorkel_observations$hydrology |> table() # clean
combined_snorkel_observations$velocity |> summary() # Only exists for early data, lots of NAs
combined_snorkel_observations$depth |> summary() # clean
combined_snorkel_observations$species |> sort() |> unique() # still a lot of species, could potentially refine some of the unidentified ones...or get. abiologiest to imrpve, but probably fine for now
combined_snorkel_observations$clipped |> table() # clean

# write csv
write_csv(combined_snorkel_observations, "data/feather_snorkel_observations.csv")

# FEATHER SNORKEL METADATA -----------------------------------------------------
combined_snorkel_metadata <- bind_rows(cleaner_snorkel_metadata_early,
                                       cleaner_snorkel_survey_metadata) |>
  select(-section_number) |> # removing because you can get this from the site lookup
  glimpse()

combined_snorkel_metadata$date |> summary() # Data from April 1999 - July 2023
combined_snorkel_metadata$flow |> summary()
combined_snorkel_metadata$section_name |> table() # TODO section name is still messy, I think this is all coming from the location field in early table
combined_snorkel_metadata$units_covered |> table() # messy but not sure if there is anything to do with this
combined_snorkel_metadata$turbidity |> summary()
combined_snorkel_metadata$temperature |> summary()
combined_snorkel_metadata$weather |> table()

# write csv
write_csv(combined_snorkel_metadata, "data/feather_snorkel_metadata.csv")


# FEATHER LOCATION LOOKUP TABLE ------------------------------------------------
sampling_unit_lookup |> glimpse()


# write csv
write_csv(sampling_unit_lookup, "data/feather_location_lookup.csv")



