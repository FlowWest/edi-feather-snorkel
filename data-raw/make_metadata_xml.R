library(EDIutils)
library(tidyverse)
library(EMLaide)
library(readxl)
library(EML)

datatable_metadata <-
  dplyr::tibble(filepath = c("data/survey_characteristics.csv",
                             "data/locations_lookup.csv",
                             "data/fish_observations.csv"),
                attribute_info = c("data-raw/metadata/survey_characteristics_metadata.xlsx",
                                   "data-raw/metadata/locations_lookup_metadata.xlsx",
                                   "data-raw/metadata/fish_observations_metadata.xlsx"),
                datatable_description = c("Survey metadata from Feather River snorkel survey data",
                                          "Location lookup for Feather River snorkel survey data",
                                          "Fish observations from Feather River snorkel survey data"),
                datatable_url = paste0("https://github.com/FlowWest/edi-feather-snorkel/tree/create-metadata/data",
                                       c("survey_characteristics.csv",
                                         "locations_lookup.csv",
                                         "fish_observations.csv")))

# save cleaned data to `data/`
excel_path <- "data-raw/metadata/feather_ongoing_snorkel_metadata.xlsx" #TODO check this excel path/name
sheets <- readxl::excel_sheets(excel_path)
metadata <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata) <- sheets

abstract_docx <- "data-raw/metadata/abstract.docx"
methods_docx <- "data-raw/metadata/methods.docx"

#edi_number <- reserve_edi_id(user_id = Sys.getenv("EDI_USER_ID"), password = Sys.getenv("EDI_PASSWORD"))
edi_number <- "feather-snorkel"

dataset <- list() %>%
  add_pub_date() %>%
  add_title(metadata$title) %>%
  add_personnel(metadata$personnel) %>%
  add_keyword_set(metadata$keyword_set) %>%
  add_abstract(abstract_docx) %>%
  add_license(metadata$license) %>%
  add_method(methods_docx) %>%
  add_maintenance(metadata$maintenance) %>%
  add_project(metadata$funding) %>%
  add_coverage(metadata$coverage, metadata$taxonomic_coverage) %>%
  add_datatable(datatable_metadata)

# GO through and check on all units
custom_units <- data.frame(id = c("count of fish", "NTU"),
                           unitType = c("dimensionless", "dimensionless"),
                           parentSI = c(NA, NA),
                           multiplierToSI = c(NA, NA),
                           description = c("number of fish counted", "NTU"))


unitList <- EML::set_unitList(custom_units)

eml <- list(packageId = edi_number,
            system = "EDI",
            access = add_access(),
            dataset = dataset,
            additionalMetadata = list(metadata = list(unitList = unitList))
)
edi_number
EML::write_eml(eml, paste0(edi_number, ".xml"))
EML::eml_validate(paste0(edi_number, ".xml"))
