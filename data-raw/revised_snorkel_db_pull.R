library(tidyverse)
library(Hmisc)

# pull database tables ---------------------------------------------------------
db_filepath <- here::here("data-raw", "Snorkel_Revised.mdb")

mdb.get(db_filepath, tables = TRUE)

snorkel_obsv <- mdb.get(db_filepath, "Observation") |> glimpse()
write_csv(snorkel_obsv, "data-raw/snorkel_observations.csv")

snorkel_survey_metadata <- mdb.get(db_filepath, "Survey") |> glimpse()
write_csv(snorkel_survey_metadata, "data-raw/snorkel_survey_metadata.csv")

# Other
river_miles <- mdb.get(db_filepath, "SnorkelSections_RiverMiles") |> glimpse()
write_csv(river_miles, "data-raw/river_miles_lookup.csv")
# Lookup tables
species_lookup <- mdb.get(db_filepath, "SpeciesLU")  |> glimpse()
lookup_cover <- mdb.get(db_filepath, "ICoverLookUp") |> glimpse()
lookup_o_cover <- mdb.get(db_filepath, "OCoverLookUp") |> glimpse()
lookup_substrate <- mdb.get(db_filepath, "SubstrateCodeLookUp") |> glimpse()
lookup_hydrology <- mdb.get(db_filepath, "CGUCodeLookUp") |> glimpse()
lookup_weather <- mdb.get(db_filepath, "WeatherCodeLookUp") |> glimpse()

detach(package:Hmisc) # detach

# read in csvs -----------------------------------------------------------------
# need this step to deal with "labeled" column types, update if we come up with a cleaner solution
raw_snorkel_observations <- read_csv("data-raw/snorkel_observations.csv")
raw_snorkel_survey_metadata <- read_csv("data-raw/snorkel_survey_metadata.csv")

river_mile_lookup <- read_csv("data-raw/river_miles_lookup.csv")

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
cleaner_snorkel_observations <- raw_snorkel_observations |>
  janitor::clean_names() |>
  select(-size_class, -est_size, -lwd, -comments) |> # Remove size because post processing, duplication of FL, TODO check on lwd, remove comments
  left_join(species_lookup, by = c("species" = "SpeciesCode")) |>
  select(-species, -observer) |>
  rename(species = Species, observation_id = obs_id, survey_id = sid, hydrology = hydrology_code) |>
  mutate(clipped = case_when(species == "O. mykiss (not clipped)" ~ FALSE,
                             species == "O. Mykiss (clipped)" ~ TRUE,
                             species == "Chinook Salmon - Clipped" ~ TRUE,
                             T ~ NA),
         species = str_to_title(case_when(species %in% c("Chinook Salmon- Fall",
                                                         "Chinook Salmon- Late Fall",
                                                         "Chinook Salmon- Spring",
                                                         "Chjnook Salmon- Spring",
                                                         "Chinook Salmon - Clipped",
                                                         "Chinook salmon - Unknown",
                                                         "Chinook salmon - Tagged") ~ "Chinook Salmon",
                                          species %in% c("O. mykiss (not clipped)",
                                                         "O. mykiss (unknown)",
                                                         "O. Mykiss (Unknown)",
                                                         "O. mykiss (clipped)") ~ "O. Mykiss",
                                          species == "Unid Juvenile Sculpin" ~ "Unidentified Juvenile Sculpin",
                                          species == "Unid Juvenile Bass (Micropterus sp.)" ~ "Unidentified Juvenile Bass",
                                          species == "Unid Juvenile Lamprey" ~ "Unidentified Juvenile Lamprey",
                                          species == "Unid Juvenile Minnow" ~ "Unidentified Juvenile Minnow",
                                          species == "UNID Sunfish"   ~ "Unidentified Sunfish",
                                          species == "Unid Juvenile non-Micropterus Sunfish" ~ "Unidentified Juvenile non-Micropterus Sunfish",
                                          species == "Unid Juvenile Fish" ~ "Unidentified Juvenile Fish",
                                          species == "NO FISH CAUGHT" ~ NA,
                                          TRUE ~ species)),
         hydrology = case_when(hydrology %in% c("Riffle Edgewater", "Riffle Margin") ~ "Riffle Margin",
                               hydrology %in% c("Glide Edgewater", "Glide Margin", "GM") ~ "Glide Margin",
                               hydrology %in% c("Backwater", "W") ~ "Backwater",
                               hydrology %in% c("Glide", "G") ~ "Glide",
                               TRUE ~ hydrology),
         instream_cover = ifelse(is.na(instream_cover), NA, str_arrange(toupper(instream_cover))),
         instream_cover = case_when(instream_cover == "VCDE" ~ "CDE", # V is not an instream cover code, remove
                                    instream_cover == "R" ~ NA, # R is not an instream cover code, remove
                                    instream_cover == "BG" ~ "B", # G is not an instream cover code, remove
                                    instream_cover == "0" ~ "A", # Assuming by 0 they mean "No Apparent Cover - A"
                                    TRUE ~ instream_cover),
         unit = case_when(unit == "32A" ~ "32", # cleaning up these units because they are not in the snorkel_section_river_miles table
                                 unit == "329.5" ~ "329",
                                 unit == "255A" ~ "255",
                                 unit == "266A" ~ "266",
                                 unit == "172B" ~ "172",
                                 unit == "26A" ~ "26",
                                 unit == "329B" ~ "329",
                                 unit == "111A" ~ "111",
                                 unit %in% c("274B", "274A") ~ "274",
                                 unit == "448A" ~ "448",
                                 unit == "271B" ~ "271",
                                 unit %in% c("272A", "272B") ~ "272",
                                 unit == "118A" ~ "118",
                                 unit == "335B" ~ "335",
                                 unit == "487B" ~ "487",
                                 TRUE  ~ unit)) |>  glimpse()

# Clean snorkel survey metadata
# TODO are mos and hatchery ditches the same thing? There is no slide for Mos riffle but it is on the map, making mos hatchery for now
# TODO existing units to clean are located in messy units object below, would be good to ask Casey
cleaner_snorkel_survey_metadata <- raw_snorkel_survey_metadata |>
  janitor::clean_names() |>
  select(-snorkelers, -comments, -shore_crew, -time_of_temperature, -snorkel_start_ttime, -snorkel_end_time) |> # removing times because they are just dates(something lost in pull)
  rename(weather = weather_code) |>
  mutate(weather = case_when(weather %in% c("CLD", "CLDY") ~ "cloudy",
                             weather %in% c("CLR (Hot)", "CLR/Hot", "Hot and CLR", "CLR Hot") ~ "clear and hot",
                             weather %in% c("RAIN", "RAN", "CLD/RAIN", "LT RAIN", "CLD, Wind, Light Sprinkles") ~
                               "precipitation",
                             weather %in% c("CLR 95", "CLR") ~ "clear",
                             weather %in% c("PT. CLDY", "CLR/CLD") ~ "partly cloudy",
                             weather %in% c("sun", "SUN") ~ "sunny",
                             weather == c("CLR WINDY") ~ "clear and windy",
                             weather == c("WND") ~ "windy",
                             weather == c("LT CLD/HAZE") ~ "hazy"),
         section_name = format_site_name(section_name),
         section_name = case_when(section_name %in% c("Vance W", "Vance West") ~ "Vance Riffle",
                                  section_name == "Eye" ~ "Eye Riffle",
                                  section_name == "Hatchery Side Ditch" ~ "Hatchery Ditch",
                                  section_name == "Hatchery Side Channel" ~ "Hatchery Riffle",
                                  section_name %in% c("Gridley Side Channel", "Hidden Gridley Side Channel", "Gridley S C Riffle", "Gridley S C", "Gridley Sc") ~ "Gridley Riffle",
                                  section_name %in% c("Robinson", "Lower Robinson") ~ "Robinson Riffle",
                                  section_name == "Goose" ~ "Goose Riffle",
                                  section_name %in% c("Auditorium", "Upper Auditorium") ~ "Auditorium Riffle",
                                  section_name %in% c("Matthews", "Mathews", "Mathews Riffle") ~ "Matthews Riffle",
                                  section_name %in% c("G95 Side Channel", "G95 Sc", "G95 West Side Channel", "G95 Side West", "G95 Side") ~ "G95",
                                  section_name %in% c("Vance West Riffle", "Vance W Riffle", "Vance East", "Vance") ~ "Vance Riffle",
                                  section_name %in% c("Alec Riffle", "Aleck") ~ "Aleck Riffle",
                                  section_name %in% c("Lower Mcfarland", "Mcfarland", "Upper Mcfarland", "McFarland", "Mc Farland") ~ "McFarland",
                                  section_name %in% c("Bed Rock Riffle", "Bedrock Riffle", "Bedrock", "Bedrock Park") ~ "Bedrock Park Riffle",
                                  section_name == "Steep" ~ "Steep Riffle",
                                  section_name %in% c("Upper Hour Side Channel Rl", "Hour To Palm Side Rl")  ~ "Hour Side Channel",
                                  section_name %in% c("Keister", "Kiester", "Keister Riffle") ~ "Kiester Riffle",
                                  section_name == "Junkyard" ~ "Junkyard Riffle",
                                  section_name == "Gateway" ~ "Gateway Riffle",
                                  section_name %in% c("Trailerpark", "Trailer Park", "Trailer Parkk", "Trailer Park Side Channel Pond" ) ~ "Trailer Park Riffle",
                                  section_name %in% c("Big Riffle Downstream Rl", "Bigriffle", "Big Riffle Bayou Rl") ~ "Big Riffle",
                                  section_name == "Section 2" ~ "Hatchery Ditch",
                                  section_name %in% c("Hatchery Ditch And Moes", "Mo's Ditch", "Mo's Ditch", "Hatchery Ditch Moes Ditch",
                                                      "Hatchery Side Channel Moes Ditch", "Upper Hatchery Ditch",
                                                      "Hatchery Ditch Lower Moes Ditch Upper", "Moes", "Moes Ditch",
                                                      "Hatchery Ditch And Moes Ditch",
                                                      "Hatchery Side Channel And Moes Ditch",
                                                      "Mo's Ditch",
                                                      "Hatchery And Moes Ditches",
                                                      "Hatchery Ditch Moes") ~"Hatchery Ditch",
                                  section_name %in% c("Hatchery And Moes Side Channels","Hatchery Sc", "Moes Side Channel", "Moes Sc",  "Hatchery Side Channel Moes", "Hatchery Side Ch Moes Side Ch",
                                                      "Hatchery Side Channel And Moes", "Hatchery Side Channel And Moes Side Channel") ~ "Hatchery Riffle",
                                  .default = as.character(section_name)),
                  section_number = case_when(section_name == "Aleck Riffle" ~ 8,
                                           section_name == "Auditorium Riffle" ~ 4,
                                           section_name == "Bedrock Park Riffle" ~ 5,
                                           section_name == "Bedrock Riffle" ~ 10,
                                           section_name == "Big Riffle" ~ 17,
                                           section_name == "Eye Riffle" ~ 11,
                                           section_name == "G95" ~ 14,
                                           section_name == "Gateway Riffle" ~ 12,
                                           section_name == "Goose Riffle" ~ 16,
                                           section_name == "Gridley Riffle" ~ 19,
                                           section_name == "Hatchery Ditch" ~ 2,
                                           section_name == "Hatchery Riffle" ~ 1,
                                           section_name == "Junkyard Riffle" ~ 20,
                                           section_name == "Kiester Riffle" ~ 15,
                                           section_name == "Matthews Riffle" ~ 7,
                                           section_name == "McFarland" ~ 18,
                                           # section_name == "Mo's Ditch" ~ 3,
                                           section_name == "Robinson Riffle" ~ 9,
                                           section_name == "Steep Riffle" ~ 10,
                                           section_name == "Trailer Park Riffle" ~ 6,
                                           section_name == "Vance Riffle" ~ 13,
                                           TRUE ~ NA)) |>

  glimpse()

# Pull in cleaned name lookup table
raw_created_lookup <- readxl::read_excel("data-raw/snorkel_built_lookup_table.xlsx") |>
  mutate(section_name = ifelse(section_name == "Mo's Ditch", "Hatchery Ditch", section_name)) |> #Decided to change Mo's Ditch for unit 28 being consistent with map, but not slides (no Mo's Ditch, but located in "Hatchery Ditch)
  glimpse()

# add in existing missing units
random_sampling_units <- cleaner_snorkel_observations |>
  select(unit) |>
  distinct() |>
  filter(!unit %in% c(raw_created_lookup$unit)) |>
  mutate(section_type = "random") |> glimpse()

# create location lookup table
sampling_unit_lookup <- bind_rows(raw_created_lookup, random_sampling_units) |>
  left_join(river_mile_lookup, by = c("unit" = "Snorkel.Sections")) |>
  rename(river_mile = River.Mile, channel = Channel) |> glimpse()

# look at ones that are missing info on location, there are 171 of these, a lot are "HGSC, maybe Hatchery Glide Side Channel, but that is a section not a unit..."
messy_units <- sampling_unit_lookup |>
  filter(is.na(river_mile)) |> pull(unit)

cleaned_fish_observations |> filter(unit %in% messy_units) |> View()

# Write clean CSVS -------------------------------------------------------------

