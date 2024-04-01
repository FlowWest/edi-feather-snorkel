library(tidyverse)
library(googleCloudStorageR)
library(ggplot2)


# google cloud set up
gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))
gcs_global_bucket(bucket = Sys.getenv("GCS_DEFAULT_BUCKET"))

# get data from google cloud
gcs_get_object(object_name = "juvenile-rearing-monitoring/seine-and-snorkel-data/feather-river/data/combined_feather_snorkel_data.csv",
               bucket = gcs_get_global_bucket(),
               saveToDisk =  here::here("data-raw", "combined_feather_snorkel_data.csv"),
               overwrite = TRUE)

combined_snorkel <- read_csv(here::here("data-raw", "combined_feather_snorkel_data.csv"))


#fields that still need clarification from Casey or team


#section_name and location

same_names <- intersect(combined_snorkel$section_name, combined_snorkel$location)


setdiff(combined_snorkel$section_name, combined_snorkel$location)



#unit_type and hydrology

table(combined_snorkel$unit_type)

table(combined_snorkel$hydrology)


