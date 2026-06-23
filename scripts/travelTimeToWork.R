rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

travelTimetoWork = c("B08135_001", "B08135_002", "B08135_003", "B08135_004", "B08135_005", "B08135_006", "B08135_007",
                     "B08135_008", "B08135_009", "B08135_010")

B08135 <- get_acs(geography = "county",
                  variables = travelTimetoWork,
                  state = "MI",
                  year = 2024,
                  survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

B08135_2 <- B08135 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

B08135_2 <- B08135_2 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

B08135_2 <- B08135_2 %>%
  select(-moe)

B08135_2 <- B08135_2 %>%
  mutate(variable = case_when(
    variable == "B08135_001" ~ "Aggregated travel time to work (minutes)",
    variable == "B08135_002" ~ "Less than 10 minutes",
    variable == "B08135_003" ~ "10-14 minutes",
    variable == "B08135_004" ~ "15-19 minutes",
    variable == "B08135_005" ~ "20-24 minutes",
    variable == "B08135_006" ~ "25-29 minutes",
    variable == "B08135_007" ~ "30-34 minutes",
    variable == "B08135_008" ~ "35-44 minutes",
    variable == "B08135_009" ~ "45-59 minutes",
    variable == "B08135_010" ~ "60 or greater minutes"
  ))

# Pivot data to wide format, with each variable as a column
B08135_wide <- B08135_2 %>%
  pivot_wider(names_from = variable, values_from = estimate, id_cols = NAME) # Pivot to wide format

B08135_wide <- B08135_wide %>%
  mutate(
    `less than 10 minutes` = ((`Less than 10 minutes`)/ `Aggregated travel time to work (minutes)`)*100,
    `10 to 29 minutes` = ((`10-14 minutes` + `15-19 minutes` + `20-24 minutes` + `25-29 minutes`)/ `Aggregated travel time to work (minutes)`)*100,
    `30 to 59 minutes` = ((`30-34 minutes` + `35-44 minutes` + `45-59 minutes`)/ `Aggregated travel time to work (minutes)`)*100,
    `60 or more minutes` = ((`60 or greater minutes`)/`Aggregated travel time to work (minutes)`)*100
  )

B08135_wide <- B08135_wide %>%
  select(-`Less than 10 minutes`,
         -`Aggregated travel time to work (minutes)`,
         -`10-14 minutes`,
         -`15-19 minutes`,
         -`20-24 minutes`,
         -`25-29 minutes`,
         -`30-34 minutes`,
         -`35-44 minutes`,
         -`45-59 minutes`,
         -`60 or greater minutes`)

B08135_long <- B08135_wide %>%
  pivot_longer(cols = c(`less than 10 minutes`, `10 to 29 minutes`, `30 to 59 minutes`, `60 or more minutes`),
               names_to = "TimeLabel",
               values_to = "TimeProportion")

saveRDS(B08135_long, "data/travelTimeToWork.rds")