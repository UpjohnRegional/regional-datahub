rm(list = ls())

# Required packages
library(dplyr)

# Function to get QCEW data for a specific year, quarter, and area (county or state code)
qcewGetAreaData <- function(year, qtr, area) {
  url <- "https://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case = FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case = FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case = FALSE)
  
  # Try to read, handle if 404 or empty
  tryCatch({
    df <- read.csv(url, stringsAsFactors = FALSE)
    return(df)
  }, error = function(e) {
    message(paste("Failed:", url))
    return(NULL)
  })
}

# Michigan FIPS county codes start with "26" (state FIPS = 26)
mi_fips <- sprintf("26%03d", c(
  1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39,
  41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75, 77, 79,
  81, 83, 85, 87, 89, 91, 93, 95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115,
  117, 119, 121, 123, 125, 127, 129, 131, 133, 135, 137, 139, 141, 143, 145, 147,
  149, 151, 153, 155, 157, 159, 161, 163, 165
))

years <- 2013:2025
quarters <- c("1", "2", "3", "4")

# Empty list to hold all data
all_data <- list()

# Loop over years, quarters, and counties
for (yr in years) {
  for (q in quarters) {
    for (fips in mi_fips) {
      cat("Fetching:", yr, "Q", q, "County", fips, "\n")
      result <- qcewGetAreaData(yr, q, fips)
      if (!is.null(result)) {
        result$year <- yr
        result$quarter <- q
        all_data[[length(all_data) + 1]] <- result
      }
      Sys.sleep(0.2)  # give the server a sec 
    }
  }
}

# Combine and filter for private ownership and total industry
combined <- bind_rows(all_data) %>%
  filter(own_code == 5, industry_code == "10") %>%
  select(area_fips, year, quarter, avg_wkly_wage)

state_data <- list()

for (year in years) {
  for (qtr in quarters) {
    temp <- tryCatch({
      qcewGetAreaData(as.character(year), qtr, "26000")
    }, error = function(e) {
      NULL
    })
    if (!is.null(temp)) {
      state_data <- rbind(state_data, temp)
    }
  }
}

# Filter to total ownership and all industries for state
state_data_filtered <- state_data %>%
  filter(own_code == 5, industry_code == "10") %>%
  select(year, qtr, state_avg_wkly_wage = avg_wkly_wage)

state_data_filtered <- state_data_filtered %>%
  rename(quarter = qtr)

state_data_filtered <- state_data_filtered %>%
  mutate(
    quarter = as.character(quarter)
  )

# Merge state data into county data
final_data <- combined %>%
  left_join(state_data_filtered, by = c("year", "quarter"))

# Create a lookup table for Michigan counties
mi_county_lookup <- data.frame(
  county_fips = sprintf("26%03d", c(
    1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39,
    41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75, 77, 79,
    81, 83, 85, 87, 89, 91, 93, 95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115,
    117, 119, 121, 123, 125, 127, 129, 131, 133, 135, 137, 139, 141, 143, 145, 147,
    149, 151, 153, 155, 157, 159, 161, 163, 165
  )),
  county = c(
    "Alcona", "Alger", "Allegan", "Alpena", "Antrim", "Arenac", "Baraga", "Barry",
    "Bay", "Benzie", "Berrien", "Branch", "Calhoun", "Cass", "Charlevoix", "Cheboygan",
    "Chippewa", "Clare", "Clinton", "Crawford", "Delta", "Dickinson", "Eaton", "Emmet",
    "Genesee", "Gladwin", "Gogebic", "Grand Traverse", "Gratiot", "Hillsdale", "Houghton",
    "Huron", "Ingham", "Ionia", "Iosco", "Iron", "Isabella", "Jackson", "Kalamazoo",
    "Kalkaska", "Kent", "Keweenaw", "Lake", "Lapeer", "Leelanau", "Lenawee", "Livingston",
    "Luce", "Mackinac", "Macomb", "Manistee", "Marquette", "Mason", "Mecosta", "Menominee",
    "Midland", "Missaukee", "Monroe", "Montcalm", "Montmorency", "Muskegon", "Newaygo",
    "Oakland", "Oceana", "Ogemaw", "Ontonagon", "Osceola", "Oscoda", "Otsego", "Ottawa",
    "Presque Isle", "Roscommon", "Saginaw", "St. Clair", "St. Joseph", "Sanilac", "Schoolcraft",
    "Shiawassee", "Tuscola", "Van Buren", "Washtenaw", "Wayne", "Wexford"
  ),
  stringsAsFactors = FALSE
)

mi_county_lookup <- mi_county_lookup %>%
  mutate(county_fips = as.integer(county_fips))

# Merge county names into combined county-level data
final_data_2 <- final_data %>%
  left_join(mi_county_lookup, by = c("area_fips" = "county_fips")) %>%
  # Add YYYY QQ date column
  mutate(date = paste0(year, " Q", quarter),
         date_order = paste0(year, "-", quarter)) %>%
  select(area_fips, county, year, quarter, date, avg_wkly_wage, state_avg_wkly_wage)

# order counties and order oldest to newest
county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

final_data_2 <- final_data_2 %>%
  mutate(order = case_when(
    county %in% county_order ~ match(county, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, county) %>%
  select(-order)

saveRDS(final_data_2, "data/qcew.rds")