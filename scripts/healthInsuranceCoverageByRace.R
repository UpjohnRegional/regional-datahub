rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

healthInsuranceByRace = c("S2701_C03_001", "S2701_C03_017", "S2701_C03_021", "S2701_C03_022", "S2701_C03_023", 
                          "S2701_C03_024")

S2701 <- get_acs(geography = "county",
                 variables = healthInsuranceByRace,
                 state = "MI",
                 year = 2024,
                 survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

S2701_2 <- S2701 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

S2701_2 <- S2701_2 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

S2701_2 <- S2701_2 %>%
  select(-moe)

S2701_2 <- S2701_2 %>%
  mutate(variable = case_when(
    variable == "S2701_C03_001" ~ "Total",
    variable == "S2701_C03_017" ~ "Black or African American",
    variable == "S2701_C03_021" ~ "Some other race",
    variable == "S2701_C03_022" ~ "Two or more races",
    variable == "S2701_C03_023" ~ "Hispanic or Latino origin (of any race)",
    variable == "S2701_C03_024" ~ "White alone, not Hispanic or Latino"
  ))

# Pivot data to wide format, with each variable as a column
S2701_wide <- S2701_2 %>%
  pivot_wider(names_from = variable, values_from = estimate, id_cols = NAME) # Pivot to wide format

saveRDS(S2701_wide, "data/healthInsuranceByRace.rds")

write.csv(S2701_wide, "data_download/healthInsuranceByRace.csv")