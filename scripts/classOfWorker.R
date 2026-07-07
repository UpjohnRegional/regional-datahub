rm(list = ls())

library(tidyverse)
library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)

classOfWorker = c("B08528_001", "B08528_002", "B08528_005", "B08528_006", "B08528_007", "B08528_008", "B08528_009",
                  "B08528_010")

B08528 <- get_acs(geography = "county",
                  variables = classOfWorker,
                  state = "MI",
                  year = 2024,
                  survey = "acs5")

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

B08528_2 <- B08528 %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

B08528_2 <- B08528_2 %>%
  mutate(order = case_when(
    NAME %in% county_order ~ match(NAME, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, NAME) %>%
  select(-order)

B08528_2 <- B08528_2 %>%
  select(-moe)

B08528_2 <- B08528_2 %>%
  mutate(variable = case_when(
    variable == "B08528_001" ~ "Total Workers",
    variable == "B08528_002" ~ "Private for-profit wage and salary workers",
    variable == "B08528_005" ~ "Private not-for-profit wage and salary workers",
    variable == "B08528_006" ~ "Local government workers",
    variable == "B08528_007" ~ "State government workers",
    variable == "B08528_008" ~ "Federal government workers",
    variable == "B08528_009" ~ "Self-employed in own not incorporated business workers",
    variable == "B08528_010" ~ "Unpaid Family Workers"
  ))

# Pivot data to wide format, with each variable as a column
B08528_wide <- B08528_2 %>%
  pivot_wider(names_from = variable, values_from = estimate, id_cols = NAME) # Pivot to wide format

B08528_wide <- B08528_wide %>%
  mutate(
    `Private Wage and Salary Workers` = ((`Private for-profit wage and salary workers` + `Private not-for-profit wage and salary workers`)/ `Total Workers`)*100,
    `Government Workers` = ((`Local government workers` + `State government workers` + `Federal government workers`)/ `Total Workers`)*100,
    `Self-employed in own, not incorporated, business workers` = ((`Self-employed in own not incorporated business workers`)/ `Total Workers`)*100,
    `Unpaid family workers` = ((`Unpaid Family Workers`)/`Total Workers`)*100
  )

B08528_wide <- B08528_wide %>%
  select(-`Total Workers`,
         -`Private for-profit wage and salary workers`,
         -`Private not-for-profit wage and salary workers`,
         -`Local government workers`,
         -`State government workers`,
         -`Federal government workers`,
         -`Self-employed in own not incorporated business workers`,
         -`Unpaid Family Workers`)

saveRDS(B08528_wide, "data/classOfWorker.rds")