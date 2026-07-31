rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

ageOfHousingStock = c("S2504_C02_009", "S2504_C02_010", "S2504_C02_011", "S2504_C02_012",
                      "S2504_C02_013", "S2504_C02_014", "S2504_C02_015")

S2504 <- get_acs(geography = "county",
                 variables = ageOfHousingStock,
                 state = "MI",
                 year = 2024,
                 survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

S2504_2 <- S2504 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

S2504_2 <- S2504_2 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

S2504_2 <- S2504_2 %>%
  select(-moe)

S2504_2 <- S2504_2 %>%
  mutate(variable = case_when(
    variable == "S2504_C02_009" ~ "2020 or later",
    variable == "S2504_C02_010" ~ "2010 to 2019",
    variable == "S2504_C02_011" ~ "2000 to 2009",
    variable == "S2504_C02_012" ~ "1980 to 1999",
    variable == "S2504_C02_013" ~ "1960 to 1979",
    variable == "S2504_C02_014" ~ "1940 to 1959",
    variable == "S2504_C02_015" ~ "1939 or earlier"
  ))

saveRDS(S2504_2, "data/ageOfHousingStock.rds")

write.csv(S2504_2, "data_download/ageOfHousingStock.csv")