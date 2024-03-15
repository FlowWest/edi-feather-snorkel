library(tidyverse)
library(googleCloudStorageR)


# google cloud set up
gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))
gcs_global_bucket(bucket = Sys.getenv("GCS_DEFAULT_BUCKET"))

# get data from google cloud
gcs_get_object(object_name = "juvenile-rearing-monitoring/seine-and-snorkel-data/feather-river/data/combined_feather_snorkel_data.csv",
               bucket = gcs_get_global_bucket(),
               saveToDisk =  here::here("data-raw", "combined_feather_snorkel_data.csv"),
               overwrite = TRUE)

combined_snorkel <- read_csv(here::here("data-raw", "combined_feather_snorkel_data.csv"))

# clean data --------------------------------------------------------------
# here is where we clean up the data and make sure it all looks as expected
# check unique values for each column
# check that all fields are being read in the right way

summary(combined_snorkel)
glimpse(combined_snorkel)

#character variables ----
unique(combined_snorkel$weather)
unique(combined_snorkel$section_name)
unique(combined_snorkel$section_type)
unique(combined_snorkel$units_covered) #check field meaning
unique(combined_snorkel$unit) #check field meaning
unique(combined_snorkel$size_class)
unique(combined_snorkel$substrate) #decoded for fields that only had one value, those with more than 1 stayed as code
unique(combined_snorkel$instream_cover) #decoded for fields that only had one value, those with more than 1 stayed as code
unique(combined_snorkel$hydrology)
unique(combined_snorkel$run)
unique(combined_snorkel$tagged)
unique(combined_snorkel$clipped)
unique(combined_snorkel$overhead_cover)
unique(combined_snorkel$location)
unique(combined_snorkel$survey_type) #check field meaning
unique(combined_snorkel$section_type) #check field meaning
unique(combined_snorkel$species)
unique(combined_snorkel$unit_type) #check decoding system for this field since it is the same code as hydrology

#numeric variables ----
summary(combined_snorkel$survey_id)
range(combined_snorkel$date)
summary(combined_snorkel$flow)
ggplot(combined_snorkel, aes(flow)) +
  geom_histogram()
combined_snorkel$flow <- ifelse(combined_snorkel$flow == 0, NA, combined_snorkel$flow) |> #changing flow values from 0 to NA
  glimpse()
summary(combined_snorkel$section_number)
summary(combined_snorkel$turbidity)
ggplot(combined_snorkel, aes(turbidity)) +
  geom_histogram()
summary(combined_snorkel$temperature)
ggplot(combined_snorkel, aes(x = date, y = temperature)) +
  geom_point()
combined_snorkel$temperature <- ifelse(combined_snorkel$temperature == 0, NA, combined_snorkel$temperature) |> #changing flow values from 0 to NA since they are potential outliers
  glimpse()

head(combined_snorkel$end_time[5:10])
head(combined_snorkel$start_time[5:10])
summary(combined_snorkel$count)
ggplot(combined_snorkel, aes(count)) +
  geom_histogram()
summary(combined_snorkel$est_size) #check field meaning
summary(combined_snorkel$water_depth_m)
ggplot(combined_snorkel, aes(water_depth_m)) +
  geom_histogram()
summary(combined_snorkel$fork_length)
ggplot(combined_snorkel, aes(fork_length)) +
  geom_histogram()
summary(combined_snorkel$bank_distance) #96.6% of this values are NA, TODO check if we want to keep, check description on metadata
summary(combined_snorkel$visibility) #check for field meaning
summary(combined_snorkel$section_number)


# write files -------------------------------------------------------------
write.csv(combined_snorkel, here::here("data-raw", "combined_feather_snorkel_data.csv"), row.names = FALSE)

# save cleaned data to `data/`
read.csv(here::here("data-raw", "combined_feather_snorkel_data.csv")) |> glimpse()
