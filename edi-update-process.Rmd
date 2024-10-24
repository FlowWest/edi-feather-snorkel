# Feather River EDI Upload Workflow

This guide provides step-by-step instructions to update the Feather River EDI package with new data.

## Navigating to the Repository
All data processing and EDI uploads are managed in the same repository.  
1. Navigate to the [project repository](https://github.com/FlowWest/edi-feather-snorkel) and clone it to your local machine.

## Adding Data
1. After adding new data to the Microsoft Access Database (`Snorkel_Revised.mdb`), make sure updates are also located in the `data-raw/db-files` folder.
2. In addition to saving the Access database, save as an `.accdb` file with `*_PC*` appended to the file name. This new file should be named `Snorkel_Revised_PC.accdb`. The reason we are doing this is because the existing .mdb file can not be accessed using the functions we developed for Windows machines.

## Processing Data
Data processing is handled by one script located in the `files_for_uploading_EDI` folder: `combine_and_clean_data.R`

1. Run `combine_and_clean_data.R` (located [here](https://github.com/FlowWest/edi-feather-snorkel/tree/main/data-raw/data-prep-scripts/combine_and_clean_data.R)).
   - This script cleans and consolidates data to ensure consistency and accuracy across different years.
   - A lookup table (`locations_lookup.csv`) is generated to address challenges in relating data to specific locations.
   - Data duplicates and missing data are handled by filtering or categorizing appropriately.
   - The cleaned data and metadata are saved as CSV files in the project repository’s `data` folder:
     - `fish_observations.csv`
     - `survey_characteristics.csv`
   
2. After generating the CSV files, update their metadata (located [here](https://github.com/FlowWest/edi-feather-snorkel/tree/main/data-raw/metadata)):
   - **Survey Characteristics** (`survey_characteristics_metadata.xlsx`): Update the minimum and maximum values for fields such as date, flow, turbidity, temperature, and visibility.
   - **Fish Observations** (`fish_observations_metadata.xlsx`): Update the minimum and maximum values for fields like date, count, fork length, depth, and velocity.
   - **Locations Lookup** will most likely not change, unless a new location is added. If so, then metadata will also need to be updated.
   - If the time range of the data has changed, update the **project metadata** in `feather_ongoing_snorkel_metadata.xlsx` (under the Coverage tab).
   - If applicable, adjust the language in the data abstract to reflect the latest updates.

### Summary of CSVs Generated for EDI Upload
- `fish_observations.csv`
- `survey_characteristics.csv`
- `locations_lookup.csv`

## EDI Update/Upload
  - Before starting this process, make sure you have an EDI account setup
  - Create or edit your .Renviron file to incluse your EDI username and password. To do this, enter the following code: `usethis::edit_r_environ()`. This will open your .Renviron file. Add a line for` edi_user_id = [enter your user name]`, and `edi_password = [enter your password]`
  
The data upload to EDI is handled in the `make_metadata_xml.R` script. The necessary modifications include:

1. Change the EDI package number at line 40 (`edi_number <- "edi.1764.1"`) to the new version number. For example, if the current version is `edi.1764.1`, change it to `edi.1764.2`.
2. After successfully running the script, manually evaluate the package by logging in to the [EDI website](https://portal.edirepository.org/nis/login.jsp) portal, and navigating to the Tools tab and click on Evaluate/Upload Data Packages. 
3. Add .xml file under EML Metadata File, select "manually upload data" under Data Upload Options. Click "Evaluate" button.
4. Attach corresponding csv file and click "Evaluate". Check for any errors, warning messages are generally okay.
5. After evaluating the package without any errors, return back to `make_metadata_xml` script and update line 75 to the version number that will be updated. On this example, package version 1764.1 will be updated. For the next update, this code will have the next most recent package version number (i.e 1764.2, 1764.3, 1764.4, etc): `EMLaide::update_edi_package(Sys.getenv("edi_user_id"), Sys.getenv("edi_password"), "edi.1764.1", paste0(edi_number, ".xml"))`
3. Uncomment this code line, and run it (note: running this code will automatically upload the EDI package. Packages can not be overwritten, so if any changes are needed, both (1) new edi number on line 40 and (2) update_edi_package on line 75 will have to be updated, and script has to be ran again)

## EDI Upload Check
To verify the new package upload, navigate to the [EDI repository portal](https://portal.edirepository.org/nis/home.jsp) and search for the updated package.
