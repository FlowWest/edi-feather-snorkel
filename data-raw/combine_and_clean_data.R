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







