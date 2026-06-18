rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

educationalAttainment = c("S1501_C02_007", "S1501_C02_008", "S1501_C02_009", "S1501_C02_010", "S1501_C02_011",
                          "S1501_C02_012", "S1501_C02_013")

S1501 <- get_acs(geography = "county",
                 variables = educationalAttainment,
                 state = "MI",
                 year = 2024,
                 survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

S1501_2 <- S1501 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

S1501_2 <- S1501_2 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

# Pivot data to wide format, with each variable as a column
S1501_wide <- S1501_2 %>%
  pivot_wider(names_from = variable, values_from = estimate, id_cols = NAME) # Pivot to wide format

S1501_wide <- S1501_wide %>%
  rename(
    'Less than 9th grade' = S1501_C02_007,
    '9th to 12th grade, no diploma' = S1501_C02_008,
    'High school graduate (includes equivalency)' = S1501_C02_009,
    'Some college, no degree' = S1501_C02_010,
    "Associate's degree" = S1501_C02_011,
    "Bachelor's degree" = S1501_C02_012,
    'Graduate or professional degree' = S1501_C02_013,
  )

saveRDS(S1501_wide, "data/eduAttainment.rds")