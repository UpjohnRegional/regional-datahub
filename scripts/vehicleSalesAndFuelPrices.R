rm(list = ls())

#eia data
library(eia)
library(tidyverse)
library(lubridate)

eia_set_key(Sys.getenv("EIA_API_KEY"))

eia_dir("petroleum/pri")

#series is under gnd
eia_metadata("petroleum/pri/gnd")

gasPrices <- eia_data(
  dir = "petroleum/pri/gnd",
  data = "value",
  freq = "monthly",
  start = "2007",
  facets = list(
    series = "EMM_EPM0_PTE_NUS_DPG"
  )
)

gasPrices <- gasPrices %>%
  mutate(date = ym(period))

#fred data
library(fredr)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

ALTSALES <- fredr(
  series_id = "ALTSALES",
  observation_start = as.Date("1950-01-01"),
  observation_end   = Sys.Date()
)

combined_data <- ALTSALES %>%
  full_join(gasPrices, by = "date") %>%
  arrange(date)

saveRDS(combined_data, "data/vehicleSalesFuelPrices.rds")