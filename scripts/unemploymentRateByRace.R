rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

employmentStatus = c("S2301_C04_001", "S2301_C04_013", "S2301_C04_017", "S2301_C04_018", "S2301_C04_019",
                     "S2301_C04_020")

S2301 <- get_acs(geography = "county",
                 variables = employmentStatus,
                 state = "MI",
                 year = 2024,
                 survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

S2301_2 <- S2301 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

S2301_2 <- S2301_2 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

S2301_2 <- S2301_2 %>%
  select(-moe)

S2301_2 <- S2301_2 %>%
  mutate(variable = case_when(
    variable == "S2301_C04_001" ~ "Total",
    variable == "S2301_C04_013" ~ "Black or African American",
    variable == "S2301_C04_017" ~ "Some other race",
    variable == "S2301_C04_018" ~ "Two or more races",
    variable == "S2301_C04_019" ~ "Hispanic or Latino origin (of any race)",
    variable == "S2301_C04_020" ~ "White alone, not Hispanic or Latino"
  ))

# Pivot data to wide format, with each variable as a column
S2301_wide <- S2301_2 %>%
  pivot_wider(names_from = variable, values_from = estimate, id_cols = NAME) # Pivot to wide format

saveRDS(S2301_wide, "data/unemploymentRatebyRace.rds")

write.csv(S2301_wide, "data_download/unemploymentRatebyRace.csv")