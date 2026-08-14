rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

povertyStatusByRace = c("S1701_C03_001", "S1701_C03_014", "S1701_C03_018", "S1701_C03_019", "S1701_C03_020", 
                        "S1701_C03_021")

S1701 <- get_acs(geography = "county",
                 variables = povertyStatusByRace,
                 state = "MI",
                 year = 2024,
                 survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

S1701_2 <- S1701 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

S1701_2 <- S1701_2 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

S1701_2 <- S1701_2 %>%
  select(-moe)

S1701_2 <- S1701_2 %>%
  mutate(variable = case_when(
    variable == "S1701_C03_001" ~ "Total",
    variable == "S1701_C03_014" ~ "Black or African American",
    variable == "S1701_C03_018" ~ "Some other race",
    variable == "S1701_C03_019" ~ "Two or more races",
    variable == "S1701_C03_020" ~ "Hispanic or Latino origin (of any race)",
    variable == "S1701_C03_021" ~ "White alone, not Hispanic or Latino"
  ))

# Pivot data to wide format, with each variable as a column
S1701_wide <- S1701_2 %>%
  pivot_wider(names_from = variable, values_from = estimate, id_cols = NAME) # Pivot to wide format

saveRDS(S1701_wide, "data/povertyStatusByRace.rds")

write.csv(S1701_wide, "data_download/povertyStatusByRace.csv")