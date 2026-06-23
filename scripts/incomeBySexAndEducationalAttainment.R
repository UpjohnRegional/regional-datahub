rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

###Male Income###

incomeBySexAndEducationalAttainment = c("B20004_008", "B20004_009", "B20004_010", "B20004_011", "B20004_012")

B20004 <- get_acs(geography = "county",
                  variables = incomeBySexAndEducationalAttainment,
                  state = "MI",
                  year = 2024,
                  survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

B20004_2 <- B20004 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

B20004_2 <- B20004_2 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

B20004_2 <- B20004_2 %>%
  mutate(variable = case_when(
    variable == "B20004_008" ~ "Less than high school graduate",
    variable == "B20004_009" ~ "High school graduate (includes equivalency)",
    variable == "B20004_010" ~ "Some college or associate's degree",
    variable == "B20004_011" ~ "Bachelor's degree",
    variable == "B20004_012" ~ "Graduate or professional degree"
  ))

B20004_2 <- B20004_2 %>%
  rename(
    'Male Income' = estimate
  )

B20004_2 <- B20004_2 %>%
  select(-moe)

###female income###

incomeBySexAndEducationalAttainmentFemale = c("B20004_014", "B20004_015", "B20004_016", "B20004_017", "B20004_018")

B20004_3 <- get_acs(geography = "county",
                    variables = incomeBySexAndEducationalAttainmentFemale,
                    state = "MI",
                    year = 2024,
                    survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

B20004_4 <- B20004_3 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

B20004_4 <- B20004_4 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

B20004_4 <- B20004_4 %>%
  mutate(variable = case_when(
    variable == "B20004_014" ~ "Less than high school graduate",
    variable == "B20004_015" ~ "High school graduate (includes equivalency)",
    variable == "B20004_016" ~ "Some college or associate's degree",
    variable == "B20004_017" ~ "Bachelor's degree",
    variable == "B20004_018" ~ "Graduate or professional degree"
  ))

B20004_4 <- B20004_4 %>%
  rename(
    'Female Income' = estimate
  )

B20004_4 <- B20004_4 %>%
  select(-moe)

# Merge male and female income datasets
B20004_combined <- full_join(
  B20004_2, 
  B20004_4, 
  by = c("NAME", "variable")
)

B20004_combined <- B20004_combined %>%
  select(-GEOID.y)

saveRDS(B20004_combined, "data/incomeBySexAndEducationalAttainment.rds")