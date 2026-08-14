rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

vehiclesPerHousehold = c("DP04_0004E", "DP04_0005E", "DP04_0002E", "DP04_0077P",
                         "DP04_0078E", "DP04_0079E", "DP04_0101E", "DP04_0134E")

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
    variable == "DP04_0004" ~ "Homeowner Vacancy Rate",
    variable == "DP04_0005" ~ "Rental Vacancy Rate",
    variable == "DP04_0002" ~ "Occupied Housing Units",
    variable == "DP04_0077P" ~ "One or Less Occupant Per Room (%)",
    variable == "DP04_0078" ~ "One to One Point Five Occupants Per Room",
    variable == "DP04_0079" ~ "One Point Five or More Occupants Per Room",
    variable == "DP04_0101" ~ "Average Monthly Owner Cost (With Mortgage) ($)",
    variable == "DP04_0134" ~ "Average Monthly Rent ($)"
  ))

# Pivot data to wide format, with each variable as a column
DP04_wide <- DP04_2 %>%
  pivot_wider(names_from = variable, values_from = estimate, id_cols = NAME) # Pivot to wide format

DP04_wide <- DP04_wide %>%
  mutate(
    `More than 1 Occupants Per Room (%)` = round(((`One to One Point Five Occupants Per Room` + 
                                                     `One Point Five or More Occupants Per Room`)
                                                  /`Occupied Housing Units`)*100, 1
    ))

DP04_wide <- DP04_wide %>%
  select(-c(`One to One Point Five Occupants Per Room`, `One Point Five or More Occupants Per Room`))

DP04_long <- DP04_wide %>%
  pivot_longer(
    cols = c(`Homeowner Vacancy Rate`, `Rental Vacancy Rate`, `Occupied Housing Units`,
             `One or Less Occupant Per Room (%)`, `More than 1 Occupants Per Room (%)`,
             `Average Monthly Owner Cost (With Mortgage) ($)`, `Average Monthly Rent ($)`)
  )

saveRDS(DP04_long, "data/keyHousingIndicators.rds")

write.csv(DP04_long, "data_download/keyHousingIndicators.csv")