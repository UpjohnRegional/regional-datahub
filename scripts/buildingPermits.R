rm(list = ls())

library(httr)
library(dplyr)
library(readr)
library(stringr)

# Base URL
base_url <- "https://www2.census.gov/econ/bps/County/"

# Define years
years <- 2010:2025

# Loop through years
for (year in years) {
  
  file_name <- paste0("co", year, "a.txt")
  
  file_url <- paste0(
    base_url,
    file_name
  )
  
  # Save files in building_permits folder
  destfile <- file.path(
    "buildingPermits",
    file_name
  )
  
  # Only download if file does not already exist
  if (!file.exists(destfile)) {
    
    tryCatch({
      
      download.file(
        file_url,
        destfile,
        mode = "wb"
      )
      
      message(
        "Downloaded: ",
        file_name
      )
      
    }, error = function(e) {
      
      message(
        "Failed: ",
        file_name
      )
      
    })
    
  } else {
    
    message(
      "Already exists: ",
      file_name
    )
    
  }
}

##### Combine Files #####

# List downloaded building permit files
files <- list.files(
  "buildingPermits",
  pattern = "^co\\d{4}a\\.txt$",
  full.names = TRUE
)

# Function to read each comma-separated file
read_bps_file <- function(file) {
  
  read_csv(
    file,
    col_types = cols(.default = "c")
  )
  
}

# Read and combine all files
bps_data <- files %>%
  lapply(read_bps_file) %>%
  bind_rows()

##### Clean Data #####

# Southwest Michigan county order
county_order <- c(
  "Kalamazoo",
  "Berrien",
  "Branch",
  "Calhoun",
  "Cass",
  "St. Joseph",
  "Van Buren"
)

bps_data_michigan <- bps_data %>%
  
  # Drop first row
  slice(-1) %>%
  
  # Keep necessary columns
  select(
    Survey,
    `FIPS...2`,
    `FIPS...3`,
    Region,
    Division,
    County,
    `1-unit`,
    `2-units`,
    `3-4 units`,
    `5+ units`
  ) %>%
  
  # Keep Michigan
  filter(
    `FIPS...2` == 26
  ) %>%
  
  # Remove Michigan Balance of State
  filter(
    !str_detect(
      County,
      "Michigan Balance of State"
    )
  ) %>%
  
  # Remove " County" from county names
  mutate(
    County = str_remove(
      County,
      " County$"
    )
  ) %>%
  
  # Drop unnecessary columns
  select(
    -`FIPS...2`,
    -`FIPS...3`,
    -Region,
    -Division
  ) %>%
  
  # Order counties
  mutate(
    county_order = case_when(
      County %in% county_order ~
        match(
          County,
          county_order
        ),
      TRUE ~
        length(county_order) + 1
    )
  ) %>%
  
  arrange(
    county_order,
    County
  ) %>%
  
  select(
    -county_order
  )

##### Save final files #####

# RDS for Quarto dashboard
saveRDS(
  bps_data_michigan,
  "data/buildingPermits.rds"
)

# CSV for user download
write.csv(
  bps_data_michigan,
  "data_download/buildingPermits.csv",
  row.names = FALSE
)