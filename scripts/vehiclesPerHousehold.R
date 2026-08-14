rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

vehiclesPerHousehold = c("DP04_0058P", "DP04_0059P", "DP04_0060P", "DP04_0061P")

DP04 <- get_acs(geography = "county",
                variables = vehiclesPerHousehold,
                state = "MI",
                year = 2024,
                survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

DP04_2 <- DP04 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

DP04_2 <- DP04_2 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

DP04_2 <- DP04_2 %>%
  select(-moe)

DP04_2 <- DP04_2 %>%
  mutate(variable = case_when(
    variable == "DP04_0058P" ~ "No Vehicles",
    variable == "DP04_0059P" ~ "One Vehicle",
    variable == "DP04_0060P" ~ "Two Vehicles",
    variable == "DP04_0061P" ~ "Three Or More Vehicles"
  ))

# Pivot data to wide format, with each variable as a column
DP04_wide <- DP04_2 %>%
  pivot_wider(names_from = variable, values_from = estimate, id_cols = NAME) # Pivot to wide format

saveRDS(DP04_wide, "data/vehiclesPerHousehold.rds")

write.csv(DP04_wide, "data_download/vehiclesPerHousehold.csv")