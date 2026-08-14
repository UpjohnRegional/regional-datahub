rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

lfprSex = c("S2301_C02_001", "S2301_C02_022", "S2301_C02_023", "S2301_C02_024", "S2301_C02_025", 
            "S2301_C02_026", "S2301_C02_027")

S2301 <- get_acs(geography = "county",
                 variables = lfprSex,
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
    variable == "S2301_C02_001" ~ "Total",
    variable == "S2301_C02_022" ~ "Men",
    variable == "S2301_C02_023" ~ "Women",
    variable == "S2301_C02_024" ~ "Women with Kids Under 18",
    variable == "S2301_C02_025" ~ "Women with Kids Under 6",
    variable == "S2301_C02_026" ~ "Women with Kids Under 6 and 6 to 17",
    variable == "S2301_C02_027" ~ "Women with Kids 6 to 17"
  ))

# Pivot data to wide format, with each variable as a column
S2301_wide <- S2301_2 %>%
  pivot_wider(names_from = variable, values_from = estimate, id_cols = NAME) # Pivot to wide format

saveRDS(S2301_wide, "data/lfprBySex.rds")

write.csv(S2301_wide, "data_download/lfprBySex.csv")