rm(list = ls())

library(bea.R)

library(httr)
library(devtools)
library(dplyr)
library(tidyr)
library(readr)

beaKey <- Sys.getenv("BEA_API_KEY")

beaSearch("gross domestic product", beaKey, asHtml = TRUE)

beaParams(beaKey, "Regional")

beaParamVals(beaKey, "Regional", "GeoFips")

beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'Regional',
  'TableName' = 'CAINC1',
  'Frequency' = 'A',
  'Year' = '2020,2021,2022,2023,2024',
  'ResultFormat' = 'json',
  'LineCode' = '3',
  'GeoFips' = '26001,26003,26005,26007,26009,26011,26013,26015,26017,26019,26021,26023,26025,26027,26029,26031,26033,26035,26037,26039,26041,26043,26045,26047,26049,26051,26053,26055,26057,26059,26061,26063,26065,26067,26069,26071,26073,26075,26077,26079,26081,26083,26085,26087,26089,26091,26093,26095,26097,26099,26101,26103,26105,26107,26109,26111,26113,26115,26117,26119,26121,26123,26125,26127,26129,26131,26133,26135,26137,26139,26141,26143,26145,26147,26149,26151,26153,26155,26157,26159,26161,26163,26165'
);
beaPayload <- beaGet(beaSpecs);

beaData_clean <- beaPayload %>%
  mutate(
    across(starts_with("DataValue_"), as.numeric)
  ) %>%
  mutate(
    pct_21_20 = 100 * (DataValue_2021 / DataValue_2020 - 1),
    pct_22_21 = 100 * (DataValue_2022 / DataValue_2021 - 1),
    pct_23_22 = 100 * (DataValue_2023 / DataValue_2022 - 1),
    pct_24_23 = 100 * (DataValue_2024 / DataValue_2023 - 1)
  ) %>%
  mutate(
    rank_2020 = rank(-DataValue_2020, ties.method = "min"),
    rank_2021 = rank(-DataValue_2021, ties.method = "min"),
    rank_2022 = rank(-DataValue_2022, ties.method = "min"),
    rank_2023 = rank(-DataValue_2023, ties.method = "min"),
    rank_2024 = rank(-DataValue_2024, ties.method = "min")
  ) %>%
  mutate(
    pc_rank_2021 = rank(-pct_21_20, ties.method = "min"),
    pc_rank_2022 = rank(-pct_22_21, ties.method = "min"),
    pc_rank_2023 = rank(-pct_23_22, ties.method = "min"),
    pc_rank_2024 = rank(-pct_24_23, ties.method = "min")
  )

county_order <- c("Kalamazoo", "Berrien", "Branch", "Calhoun", "Cass", "St. Joseph", "Van Buren")

beaData_clean <- beaData_clean %>%
  mutate(order = case_when(
    GeoName %in% county_order ~ match(GeoName, county_order),
    TRUE ~ length(county_order) + 1
  )) %>%
  arrange(order, GeoName) %>%
  select(-order)

saveRDS(beaData_clean, "data/PerCapitaIncome.rds")

write.csv(beaData_clean, "data_download/PerCapitaIncome.csv")