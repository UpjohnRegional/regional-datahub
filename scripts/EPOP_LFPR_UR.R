# Clear Environment
rm(list = ls())

# Install packages
#install.packages(c("blscrapeR", "tidyverse", "lubridate", "httr", "jsonlite", "zoo"))

# Call in packages
library(blscrapeR)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(zoo)
library(dplyr)

##### BLS DATA #####
# Series IDs, 3 thru 6 for each MI county
blsMICountySeries_ids <- c(
  "LAUCN260010000000003",
  "LAUCN260010000000004",
  "LAUCN260010000000005",
  "LAUCN260010000000006",
  "LAUCN260030000000003",
  "LAUCN260030000000004",
  "LAUCN260030000000005",
  "LAUCN260030000000006",
  "LAUCN260050000000003",
  "LAUCN260050000000004",
  "LAUCN260050000000005",
  "LAUCN260050000000006",
  "LAUCN260070000000003",
  "LAUCN260070000000004",
  "LAUCN260070000000005",
  "LAUCN260070000000006",
  "LAUCN260090000000003",
  "LAUCN260090000000004",
  "LAUCN260090000000005",
  "LAUCN260090000000006",
  "LAUCN260110000000003",
  "LAUCN260110000000004",
  "LAUCN260110000000005",
  "LAUCN260110000000006",
  "LAUCN260130000000003",
  "LAUCN260130000000004",
  "LAUCN260130000000005",
  "LAUCN260130000000006",
  "LAUCN260150000000003",
  "LAUCN260150000000004",
  "LAUCN260150000000005",
  "LAUCN260150000000006",
  "LAUCN260170000000003",
  "LAUCN260170000000004",
  "LAUCN260170000000005",
  "LAUCN260170000000006",
  "LAUCN260190000000003",
  "LAUCN260190000000004",
  "LAUCN260190000000005",
  "LAUCN260190000000006",
  "LAUCN260210000000003",
  "LAUCN260210000000004",
  "LAUCN260210000000005",
  "LAUCN260210000000006",
  "LAUCN260230000000003",
  "LAUCN260230000000004",
  "LAUCN260230000000005",
  "LAUCN260230000000006",
  "LAUCN260250000000003",
  "LAUCN260250000000004",
  "LAUCN260250000000005",
  "LAUCN260250000000006",
  "LAUCN260270000000003",
  "LAUCN260270000000004",
  "LAUCN260270000000005",
  "LAUCN260270000000006",
  "LAUCN260290000000003",
  "LAUCN260290000000004",
  "LAUCN260290000000005",
  "LAUCN260290000000006",
  "LAUCN260310000000003",
  "LAUCN260310000000004",
  "LAUCN260310000000005",
  "LAUCN260310000000006",
  "LAUCN260330000000003",
  "LAUCN260330000000004",
  "LAUCN260330000000005",
  "LAUCN260330000000006",
  "LAUCN260350000000003",
  "LAUCN260350000000004",
  "LAUCN260350000000005",
  "LAUCN260350000000006",
  "LAUCN260370000000003",
  "LAUCN260370000000004",
  "LAUCN260370000000005",
  "LAUCN260370000000006",
  "LAUCN260390000000003",
  "LAUCN260390000000004",
  "LAUCN260390000000005",
  "LAUCN260390000000006",
  "LAUCN260410000000003",
  "LAUCN260410000000004",
  "LAUCN260410000000005",
  "LAUCN260410000000006",
  "LAUCN260430000000003",
  "LAUCN260430000000004",
  "LAUCN260430000000005",
  "LAUCN260430000000006",
  "LAUCN260450000000003",
  "LAUCN260450000000004",
  "LAUCN260450000000005",
  "LAUCN260450000000006",
  "LAUCN260470000000003",
  "LAUCN260470000000004",
  "LAUCN260470000000005",
  "LAUCN260470000000006",
  "LAUCN260490000000003",
  "LAUCN260490000000004",
  "LAUCN260490000000005",
  "LAUCN260490000000006",
  "LAUCN260510000000003",
  "LAUCN260510000000004",
  "LAUCN260510000000005",
  "LAUCN260510000000006",
  "LAUCN260530000000003",
  "LAUCN260530000000004",
  "LAUCN260530000000005",
  "LAUCN260530000000006",
  "LAUCN260550000000003",
  "LAUCN260550000000004",
  "LAUCN260550000000005",
  "LAUCN260550000000006",
  "LAUCN260570000000003",
  "LAUCN260570000000004",
  "LAUCN260570000000005",
  "LAUCN260570000000006",
  "LAUCN260590000000003",
  "LAUCN260590000000004",
  "LAUCN260590000000005",
  "LAUCN260590000000006",
  "LAUCN260610000000003",
  "LAUCN260610000000004",
  "LAUCN260610000000005",
  "LAUCN260610000000006",
  "LAUCN260630000000003",
  "LAUCN260630000000004",
  "LAUCN260630000000005",
  "LAUCN260630000000006",
  "LAUCN260650000000003",
  "LAUCN260650000000004",
  "LAUCN260650000000005",
  "LAUCN260650000000006",
  "LAUCN260670000000003",
  "LAUCN260670000000004",
  "LAUCN260670000000005",
  "LAUCN260670000000006",
  "LAUCN260690000000003",
  "LAUCN260690000000004",
  "LAUCN260690000000005",
  "LAUCN260690000000006",
  "LAUCN260710000000003",
  "LAUCN260710000000004",
  "LAUCN260710000000005",
  "LAUCN260710000000006",
  "LAUCN260730000000003",
  "LAUCN260730000000004",
  "LAUCN260730000000005",
  "LAUCN260730000000006",
  "LAUCN260750000000003",
  "LAUCN260750000000004",
  "LAUCN260750000000005",
  "LAUCN260750000000006",
  "LAUCN260770000000003",
  "LAUCN260770000000004",
  "LAUCN260770000000005",
  "LAUCN260770000000006",
  "LAUCN260790000000003",
  "LAUCN260790000000004",
  "LAUCN260790000000005",
  "LAUCN260790000000006",
  "LAUCN260810000000003",
  "LAUCN260810000000004",
  "LAUCN260810000000005",
  "LAUCN260810000000006",
  "LAUCN260830000000003",
  "LAUCN260830000000004",
  "LAUCN260830000000005",
  "LAUCN260830000000006",
  "LAUCN260850000000003",
  "LAUCN260850000000004",
  "LAUCN260850000000005",
  "LAUCN260850000000006",
  "LAUCN260870000000003",
  "LAUCN260870000000004",
  "LAUCN260870000000005",
  "LAUCN260870000000006",
  "LAUCN260890000000003",
  "LAUCN260890000000004",
  "LAUCN260890000000005",
  "LAUCN260890000000006",
  "LAUCN260910000000003",
  "LAUCN260910000000004",
  "LAUCN260910000000005",
  "LAUCN260910000000006",
  "LAUCN260930000000003",
  "LAUCN260930000000004",
  "LAUCN260930000000005",
  "LAUCN260930000000006",
  "LAUCN260950000000003",
  "LAUCN260950000000004",
  "LAUCN260950000000005",
  "LAUCN260950000000006",
  "LAUCN260970000000003",
  "LAUCN260970000000004",
  "LAUCN260970000000005",
  "LAUCN260970000000006",
  "LAUCN260990000000003",
  "LAUCN260990000000004",
  "LAUCN260990000000005",
  "LAUCN260990000000006",
  "LAUCN261010000000003",
  "LAUCN261010000000004",
  "LAUCN261010000000005",
  "LAUCN261010000000006",
  "LAUCN261030000000003",
  "LAUCN261030000000004",
  "LAUCN261030000000005",
  "LAUCN261030000000006",
  "LAUCN261050000000003",
  "LAUCN261050000000004",
  "LAUCN261050000000005",
  "LAUCN261050000000006",
  "LAUCN261070000000003",
  "LAUCN261070000000004",
  "LAUCN261070000000005",
  "LAUCN261070000000006",
  "LAUCN261090000000003",
  "LAUCN261090000000004",
  "LAUCN261090000000005",
  "LAUCN261090000000006",
  "LAUCN261110000000003",
  "LAUCN261110000000004",
  "LAUCN261110000000005",
  "LAUCN261110000000006",
  "LAUCN261130000000003",
  "LAUCN261130000000004",
  "LAUCN261130000000005",
  "LAUCN261130000000006",
  "LAUCN261150000000003",
  "LAUCN261150000000004",
  "LAUCN261150000000005",
  "LAUCN261150000000006",
  "LAUCN261170000000003",
  "LAUCN261170000000004",
  "LAUCN261170000000005",
  "LAUCN261170000000006",
  "LAUCN261190000000003",
  "LAUCN261190000000004",
  "LAUCN261190000000005",
  "LAUCN261190000000006",
  "LAUCN261210000000003",
  "LAUCN261210000000004",
  "LAUCN261210000000005",
  "LAUCN261210000000006",
  "LAUCN261230000000003",
  "LAUCN261230000000004",
  "LAUCN261230000000005",
  "LAUCN261230000000006",
  "LAUCN261250000000003",
  "LAUCN261250000000004",
  "LAUCN261250000000005",
  "LAUCN261250000000006",
  "LAUCN261270000000003",
  "LAUCN261270000000004",
  "LAUCN261270000000005",
  "LAUCN261270000000006",
  "LAUCN261290000000003",
  "LAUCN261290000000004",
  "LAUCN261290000000005",
  "LAUCN261290000000006",
  "LAUCN261310000000003",
  "LAUCN261310000000004",
  "LAUCN261310000000005",
  "LAUCN261310000000006",
  "LAUCN261330000000003",
  "LAUCN261330000000004",
  "LAUCN261330000000005",
  "LAUCN261330000000006",
  "LAUCN261350000000003",
  "LAUCN261350000000004",
  "LAUCN261350000000005",
  "LAUCN261350000000006",
  "LAUCN261370000000003",
  "LAUCN261370000000004",
  "LAUCN261370000000005",
  "LAUCN261370000000006",
  "LAUCN261390000000003",
  "LAUCN261390000000004",
  "LAUCN261390000000005",
  "LAUCN261390000000006",
  "LAUCN261410000000003",
  "LAUCN261410000000004",
  "LAUCN261410000000005",
  "LAUCN261410000000006",
  "LAUCN261430000000003",
  "LAUCN261430000000004",
  "LAUCN261430000000005",
  "LAUCN261430000000006",
  "LAUCN261450000000003",
  "LAUCN261450000000004",
  "LAUCN261450000000005",
  "LAUCN261450000000006",
  "LAUCN261470000000003",
  "LAUCN261470000000004",
  "LAUCN261470000000005",
  "LAUCN261470000000006",
  "LAUCN261490000000003",
  "LAUCN261490000000004",
  "LAUCN261490000000005",
  "LAUCN261490000000006",
  "LAUCN261510000000003",
  "LAUCN261510000000004",
  "LAUCN261510000000005",
  "LAUCN261510000000006",
  "LAUCN261530000000003",
  "LAUCN261530000000004",
  "LAUCN261530000000005",
  "LAUCN261530000000006",
  "LAUCN261550000000003",
  "LAUCN261550000000004",
  "LAUCN261550000000005",
  "LAUCN261550000000006",
  "LAUCN261570000000003",
  "LAUCN261570000000004",
  "LAUCN261570000000005",
  "LAUCN261570000000006",
  "LAUCN261590000000003",
  "LAUCN261590000000004",
  "LAUCN261590000000005",
  "LAUCN261590000000006",
  "LAUCN261610000000003",
  "LAUCN261610000000004",
  "LAUCN261610000000005",
  "LAUCN261610000000006",
  "LAUCN261630000000003",
  "LAUCN261630000000004",
  "LAUCN261630000000005",
  "LAUCN261630000000006",
  "LAUCN261650000000003",
  "LAUCN261650000000004",
  "LAUCN261650000000005",
  "LAUCN261650000000006"
)

# MI County FIPS
mi_county_lookup <- tibble::tribble(
  ~county_fips, ~county,
  "001", "Alcona",
  "003", "Alger",
  "005", "Allegan",
  "007", "Alpena",
  "009", "Antrim",
  "011", "Arenac",
  "013", "Baraga",
  "015", "Barry",
  "017", "Bay",
  "019", "Benzie",
  "021", "Berrien",
  "023", "Branch",
  "025", "Calhoun",
  "027", "Cass",
  "029", "Charlevoix",
  "031", "Cheboygan",
  "033", "Chippewa",
  "035", "Clare",
  "037", "Clinton",
  "039", "Crawford",
  "041", "Delta",
  "043", "Dickinson",
  "045", "Eaton",
  "047", "Emmet",
  "049", "Genesee",
  "051", "Gladwin",
  "053", "Gogebic",
  "055", "Grand Traverse",
  "057", "Gratiot",
  "059", "Hillsdale",
  "061", "Houghton",
  "063", "Huron",
  "065", "Ingham",
  "067", "Ionia",
  "069", "Iosco",
  "071", "Iron",
  "073", "Isabella",
  "075", "Jackson",
  "077", "Kalamazoo",
  "079", "Kalkaska",
  "081", "Kent",
  "083", "Keweenaw",
  "085", "Lake",
  "087", "Lapeer",
  "089", "Leelanau",
  "091", "Lenawee",
  "093", "Livingston",
  "095", "Luce",
  "097", "Mackinac",
  "099", "Macomb",
  "101", "Manistee",
  "103", "Marquette",
  "105", "Mason",
  "107", "Mecosta",
  "109", "Menominee",
  "111", "Midland",
  "113", "Missaukee",
  "115", "Monroe",
  "117", "Montcalm",
  "119", "Montmorency",
  "121", "Muskegon",
  "123", "Newaygo",
  "125", "Oakland",
  "127", "Oceana",
  "129", "Ogemaw",
  "131", "Ontonagon",
  "133", "Osceola",
  "135", "Oscoda",
  "137", "Otsego",
  "139", "Ottawa",
  "141", "Presque Isle",
  "143", "Roscommon",
  "145", "Saginaw",
  "147", "St. Clair",
  "149", "St. Joseph",
  "151", "Sanilac",
  "153", "Schoolcraft",
  "155", "Shiawassee",
  "157", "Tuscola",
  "159", "Van Buren",
  "161", "Washtenaw",
  "163", "Wayne",
  "165", "Wexford"
)

# API KEY
blsAPIKey <- Sys.getenv("BLS_API_KEY")

# check series IDs
# raw <- bls_api(blsCountySeries_ids, startyear = 2022, endyear = 2025, registrationKey = blsAPIKey)
# names(raw)

# need to break BLS requests into chunks
# function to split vector into chunks
chunk_vec <- function(x, n) split(x, ceiling(seq_along(x)/n))

# split into chunks of 50 (max per request)
series_chunks <- chunk_vec(blsMICountySeries_ids, 50)

# get county data frame from series IDs and mutate to add county names, series ID titles, convert to wide
blsMICountyList <- lapply(series_chunks, function(chunk){
  bls_api(
    chunk,
    startyear = 2022,
    endyear = 2025,
    registrationKey = Sys.getenv("BLS_API_KEY")
  ) %>%
    dateCast() %>%
    mutate(
      county_fips = stringr::str_extract(seriesID, "(?<=LAUCN26)\\d{3}"),
      variable = case_when(
        str_detect(seriesID, "0000000003$") ~ "unemployment_rate",
        str_detect(seriesID, "0000000004$") ~ "unemployment",
        str_detect(seriesID, "0000000005$") ~ "employment",
        str_detect(seriesID, "0000000006$") ~ "labor_force",
        TRUE ~ "other"
      )
    ) %>%
    left_join(mi_county_lookup, by = "county_fips") %>%
    select(year, period, footnotes,
           county_fips,   
           county,        
           variable, value)
})

blsMICounty <- bind_rows(blsMICountyList)

# Collapse duplicates that occur across chunks
blsMICounty <- blsMICounty %>%
  group_by(year, period, county_fips, county, variable) %>%
  summarise(
    value = mean(value, na.rm = TRUE),        
    footnotes = first(footnotes),            
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(year, period, county_fips, county, footnotes),
    names_from = variable,
    values_from = value
  )

# fix period column to be MMM-YY
blsMICounty <- blsMICounty %>%
  mutate(month_num = as.integer(sub("M", "", period)),
         date = make_date(year, month_num, 1),
         period = format(date, "%b-%y")) %>%
  select(-month_num, -date)

# Series IDs for the State of Michigan
blsMIStateSeries_ids <- c(
  "LAUST260000000000003",
  "LAUST260000000000004",
  "LAUST260000000000005",
  "LAUST260000000000006",
  "LAUST260000000000007",
  "LAUST260000000000008",
  "LAUST260000000000009"
)


# get state data frame from series IDs and convert to wide
blsMIState <- bls_api(
  blsMIStateSeries_ids,
  startyear = 2022,
  endyear = 2025,
  registrationKey = Sys.getenv("BLS_API_KEY")
) %>%
  dateCast() %>%
  mutate(
    state_code = str_extract(seriesID, "(?<=LAUST)\\d{2}"),
    variable = case_when(
      str_detect(seriesID, "0000000000003$") ~ "MI_unemployment_rate",
      str_detect(seriesID, "0000000000004$") ~ "MI_unemployment",
      str_detect(seriesID, "0000000000005$") ~ "MI_employment",
      str_detect(seriesID, "0000000000006$") ~ "MI_labor_force",
      str_detect(seriesID, "0000000000007$") ~ "MI_employment_population_ratio",
      str_detect(seriesID, "0000000000008$") ~ "MI_labor_force_participation_rate",
      str_detect(seriesID, "0000000000009$") ~ "MI_civilian_noninstitutional_population",
      TRUE ~ "other"
    ),
    value = as.numeric(value)
  ) %>%
  select(year, period, footnotes, variable, value) %>%
  
  # pivot long-to-wide temporarily so we can coalesce complementary rows
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  
  # collapse rows by year and period, taking non-NA values
  group_by(year, period) %>%
  summarise(
    across(starts_with("MI_"), ~ coalesce(.[1], .[2])),
    footnotes = first(footnotes),
    .groups = "drop"
  ) %>%
  
  # just in case pivot_wider created any extra columns
  ungroup()

# fix period column to be MMM-YY
blsMIState <- blsMIState %>%
  mutate(month_num = as.integer(sub("M", "", period)),
         date = make_date(year, month_num, 1),
         period = format(date, "%b-%y")) %>%
  select(-month_num, -date)

bls_combined <- blsMICounty %>%
  left_join(
    blsMIState %>%
      select(
        period,
        MI_unemployment_rate,
        MI_employment_population_ratio,
        MI_labor_force_participation_rate
      ),
    by = "period"
  )

##### Resident Population Estimates #####

# 2024 estimates, edit to 2025 when it is released
urls <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/asrh/cc-est2024-agesex-26.csv"

class(urls)

# select only needed columns from csv
pop_est_MI_Counties <- read.csv(urls) %>%
  select(STATE, COUNTY, CTYNAME, YEAR, AGE16PLUS_TOT, POPESTIMATE)


# change year code to actual values (after loading lubridate)
pop_est_MI_Counties <- pop_est_MI_Counties %>%
  mutate(
    date = case_when(
      YEAR == 1 ~ as.Date("2020-04-01"),
      YEAR == 2 ~ as.Date("2020-07-01"),
      YEAR == 3 ~ as.Date("2021-07-01"),
      YEAR == 4 ~ as.Date("2022-07-01"),
      YEAR == 5 ~ as.Date("2023-07-01"),
      YEAR == 6 ~ as.Date("2024-07-01")
    ),
    date = format(date, "%Y_%m_%d")
  )

# pivot wide
pop_est_wide <- pop_est_MI_Counties %>%
  pivot_wider(
    id_cols = c(STATE, COUNTY, CTYNAME),
    names_from = date,
    values_from = c(AGE16PLUS_TOT, POPESTIMATE),
    names_glue = "{.value}_{date}"
  )

# calculate 3 and 5 year change
pop_est_wide <- pop_est_wide %>%
  mutate(
    AGE16PLUS_TOT_3YRChange =
      (AGE16PLUS_TOT_2023_07_01 / AGE16PLUS_TOT_2020_07_01)^(1/3),
    
    AGE16PLUS_TOT_5YRChange =
      (AGE16PLUS_TOT_2024_07_01 / AGE16PLUS_TOT_2020_07_01)^(1/5)
  )

# estimate from 3 year change
pop_est_wide <- pop_est_wide %>%
  mutate(
    AGE16PLUS_TOT_2025 =
      AGE16PLUS_TOT_2024_07_01 * AGE16PLUS_TOT_3YRChange,
    
    AGE16PLUS_TOT_2026 =
      AGE16PLUS_TOT_2025 * AGE16PLUS_TOT_3YRChange
  )

# add county fips as character column
pop_est_wide <- pop_est_wide %>%
  mutate(
    county_fips = sprintf("%03d", COUNTY)
  )


##### Combine BLS with Pop Est #####
# join pop estimates to bls data
bls_combined_popest <- bls_combined %>%
  left_join(
    pop_est_wide %>%
      select(county_fips, AGE16PLUS_TOT_2024_07_01, AGE16PLUS_TOT_2025, AGE16PLUS_TOT_2026),
    by = "county_fips"
  )

# create one population column based on year, drop unneeded columns, drop duplicate most recent period
bls_combined_popest <- bls_combined_popest %>%
  mutate(
    AGE16PLUS_TOT = case_when(
      year == 2024 ~ AGE16PLUS_TOT_2024_07_01,
      year == 2025 ~ AGE16PLUS_TOT_2025,
      year == 2026 ~ AGE16PLUS_TOT_2026,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-AGE16PLUS_TOT_2025, -AGE16PLUS_TOT_2026, -AGE16PLUS_TOT_2024_07_01) %>%
  distinct(county_fips, period, .keep_all = TRUE)

# filter to last 13 months  
bls_combined_popest <- bls_combined_popest %>%
  mutate(period_date = my(period)) %>%
  filter(period_date >= max(period_date) %m-% months(12))

# mark preliminary or estimated data
bls_combined_popest <- bls_combined_popest %>%
  mutate(
    period_txt = case_when(
      str_detect(footnotes, "Preliminary") ~ paste0(period, "p"),
      str_detect(footnotes, "Data unavailable") ~ paste0(period, "e"),
      TRUE ~ period
    )
  )

bls_combined_popest_impute <- bls_combined_popest %>%
  arrange(county_fips, period_date) %>%
  group_by(county_fips) %>%
  mutate(
    unemployment_rate = na.approx(unemployment_rate, na.rm = FALSE),
    unemployment = na.approx(unemployment, na.rm = FALSE),
    employment = na.approx(employment, na.rm = FALSE),
    labor_force = na.approx(labor_force, na.rm = FALSE),
    MI_unemployment_rate = na.approx(MI_unemployment_rate, na.rm = FALSE),
    MI_employment_population_ratio = na.approx(MI_employment_population_ratio, na.rm = FALSE),
    MI_labor_force_participation_rate = na.approx(MI_labor_force_participation_rate, na.rm = FALSE),
  ) %>%
  ungroup()

# calculate LFPR and EPOP
bls_combined_popest_impute <- bls_combined_popest_impute %>%
  mutate(
    LFPR = (labor_force / AGE16PLUS_TOT) * 100,
    EPOP = (employment / AGE16PLUS_TOT) * 100
  )

# order counties 
county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

EPOP_LFPR_UR_Final <- bls_combined_popest_impute %>%
  mutate(order = case_when(
    county %in% county_order ~ match(county, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(period_date, order, county) %>%
  select(-order)

# save to rds
saveRDS(EPOP_LFPR_UR_Final, "data/EPOP_LFPR_UR.rds")

# save to csv
#output_csv <- "EPOP_LFPR_UR.csv"
#write.csv(EPOP_LFPR_UR_Final, output_csv, row.names = FALSE)

#cat("BLS County and State data saved to", output_csv)