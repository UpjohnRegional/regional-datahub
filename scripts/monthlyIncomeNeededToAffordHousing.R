rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

medianHousingCost = c("DP04_0134", "DP04_0101")

DP04 <- get_acs(geography = "county",
                variables = medianHousingCost,
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
    variable == "DP04_0101" ~ "Homeowners",
    variable == "DP04_0134" ~ "Renters"
  ))

DP04_2 <- DP04_2 %>%
  rename('Median Cost' = 'estimate')

DP04_2 <- DP04_2 %>%
  mutate(
    `Income Needed to Afford Cost` = round((`Median Cost` * 33.33)/ 12))

saveRDS(DP04_2, "data/monthlyIncomeNeededToAffordHousing.rds")

write.csv(DP04_2, "data_download/monthlyIncomeNeededToAffordHousing.csv")