rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

disabilityStatus = c("S1810_C03_001", "S1810_C03_019", "S1810_C03_029", "S1810_C03_039", "S1810_C03_047", 
                     "S1810_C03_055", "S1810_C03_063")

S1810 <- get_acs(geography = "county",
                 variables = disabilityStatus,
                 state = "MI",
                 year = 2024,
                 survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

S1810_2 <- S1810 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

S1810_2 <- S1810_2 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

S1810_2 <- S1810_2 %>%
  select(-moe)

S1810_2 <- S1810_2 %>%
  mutate(variable = case_when(
    variable == "S1810_C03_001" ~ "Civilian Population With a Disability",
    variable == "S1810_C03_019" ~ "Hearing Difficulty",
    variable == "S1810_C03_029" ~ "Vision Difficulty",
    variable == "S1810_C03_039" ~ "Cognitive Difficulty",
    variable == "S1810_C03_047" ~ "Ambulatory Difficulty",
    variable == "S1810_C03_055" ~ "Self-Care Difficulty",
    variable == "S1810_C03_063" ~ "Independent Living Difficulty"
  ))

# Pivot data to wide format, with each variable as a column
S1810_wide <- S1810_2 %>%
  pivot_wider(names_from = variable, values_from = estimate, id_cols = NAME) # Pivot to wide format

saveRDS(S1810_wide, "data/civilianNoninstitutionalizedPopulationWithADisability.rds")

write.csv(S1810_wide, "data_download/civilianNoninstitutionalizedPopulationWithADisability.csv")