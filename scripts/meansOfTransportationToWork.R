rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

meansOfTransportationToWork = c("B08128_001", "B08128_011", "B08128_021", "B08128_031", "B08128_041", 
                                "B08128_051", "B08128_061")

B08128 <- get_acs(geography = "county",
                  variables = meansOfTransportationToWork,
                  state = "MI",
                  year = 2024,
                  survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

B08128_2 <- B08128 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

B08128_2 <- B08128_2 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

B08128_2 <- B08128_2 %>%
  select(-moe)

B08128_2 <- B08128_2 %>%
  mutate(variable = case_when(
    variable == "B08128_001" ~ "Totals",
    variable == "B08128_011" ~ "Car, truck, or van - drove alones",
    variable == "B08128_021" ~ "Car, truck, or van - carpooleds",
    variable == "B08128_031" ~ "Public transportation (excluding taxicab)s",
    variable == "B08128_041" ~ "Walkeds",
    variable == "B08128_051" ~ "Taxicab, motorcycle, bicycle, or other meanss",
    variable == "B08128_061" ~ "Worked from homes"
  ))

# Pivot data to wide format, with each variable as a column
B08128_wide <- B08128_2 %>%
  pivot_wider(names_from = variable, values_from = estimate, id_cols = NAME) # Pivot to wide format

B08128_wide <- B08128_wide %>%
  mutate(
    `Car, truck, or van - drove alone` = ((`Car, truck, or van - drove alones`)/ `Totals`)*100,
    `Car, truck, or van - carpooled` = ((`Car, truck, or van - carpooleds`)/ `Totals`)*100,
    `Public transportation (excluding taxicab)` = ((`Public transportation (excluding taxicab)s`)/ `Totals`)*100,
    `Walked` = ((`Walkeds`)/`Totals`)*100,
    `Taxicab, motorcycle, bicycle, or other means` = ((`Taxicab, motorcycle, bicycle, or other meanss`)/`Totals`)*100,
    `Worked from home` = ((`Worked from homes`)/`Totals`)*100
  )

B08128_wide <- B08128_wide %>%
  select(-`Totals`,
         -`Car, truck, or van - drove alones`,
         -`Car, truck, or van - carpooleds`,
         -`Public transportation (excluding taxicab)s`,
         -`Walkeds`,
         -`Taxicab, motorcycle, bicycle, or other meanss`,
         -`Worked from homes`)

B08128_long <- B08128_wide %>%
  pivot_longer(cols = c(`Car, truck, or van - drove alone`, `Car, truck, or van - carpooled`, 
                        `Public transportation (excluding taxicab)`, `Walked`,
                        `Taxicab, motorcycle, bicycle, or other means`,
                        `Worked from home`),
               names_to = "Category",
               values_to = "Proportion")

saveRDS(B08128_long, "data/meansOfTransportationToWork.rds")

write.csv(B08128_long, "data_download/meansOfTransportationToWork.csv")