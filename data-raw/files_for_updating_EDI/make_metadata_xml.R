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
                datatable_url = paste0("https://raw.githubusercontent.com/FlowWest/edi-feather-snorkel/main/data/",
                                       c("survey_characteristics.csv",
                                         "locations_lookup.csv",
                                         "fish_observations.csv")))

other_entity_metadata_1 <- list("file_name" = "unit_spatial_data.zip",
                                "file_description" = "Kmz and GIS files for survey units",
                                "file_type" = "zip",
                                "physical" = create_physical("data-raw/metadata/unit_spatial_data.zip",
                                                             data_url = "https://raw.githubusercontent.com/FlowWest/edi-feather-snorkel/main/data-raw/metadata/unit_spatial_data.zip"))

other_entity_metadata_1$physical$dataFormat <- list("externallyDefinedFormat" = list("formatName" = "zip"))

# save cleaned data to `data/`
excel_path <- "data-raw/metadata/feather_ongoing_snorkel_metadata.xlsx" #TODO check this excel path/name
sheets <- readxl::excel_sheets(excel_path)
metadata <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata) <- sheets

abstract_docx <- "data-raw/metadata/abstract.docx"
methods_docx <- "data-raw/metadata/methods.md"

#edi_number <- reserve_edi_id(user_id = Sys.getenv("EDI_USER_ID"), password = Sys.getenv("EDI_PASSWORD"))
edi_number <- "edi.1764.1"

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
  add_datatable(datatable_metadata) |>
  add_other_entity(other_entity_metadata_1)

# GO through and check on all units
custom_units <- data.frame(id = c("parr marks", "decimal degrees", "decimal degrees", "count of fish", "NTU", "square meters"),
                           unitType = c("dimensionless", "dimensionless", "dimensionless", "dimensionless", "dimensionless", "dimensionless"),
                           parentSI = c(NA, NA, NA, NA, NA,NA),
                           multiplierToSI = c(NA, NA, NA, NA,NA,NA),
                           description = c("parr marks", "decimal degrees", "decimal degrees","number of fish counted", "NTU", "square meters"))


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
# EMLaide::update_edi_package(Sys.getenv("edi_user_id"), Sys.getenv("edi_password"), "edi.1764.1", paste0(edi_number, ".xml"))
# EMLaide::upload_edi_package(Sys.getenv("EDI_USER_ID"),
#                             Sys.getenv("EDI_PASSWORD"),
#                             "edi.1764.1.xml",
#                             environment = "production")
