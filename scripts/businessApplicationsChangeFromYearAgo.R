rm(list = ls())

library(fredr)
library(dplyr)
library(readr)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

BUSAPPWNSAMIYY <- fredr(
  series_id = "BUSAPPWNSAMIYY",
  observation_start = as.Date("1950-01-01"),
  observation_end   = Sys.Date(),
  frequency = "m"
)

BUSAPPWNSAUS <- fredr(
  series_id = "BUSAPPWNSAUS",
  observation_start = as.Date("1950-01-01"),
  observation_end   = Sys.Date(),
  units = "pc1",
  frequency = "m",
  aggregation_method = "avg"
)

BUSAPPWNSAMIYY_df <- BUSAPPWNSAMIYY %>%
  select(date, value) %>%
  rename(BUSAPPWNSAMIYY = value)

BUSAPPWNSAUS_df <- BUSAPPWNSAUS %>%
  select(date, value) %>%
  rename(BUSAPPWNSAUS = value)

combined_data <- BUSAPPWNSAMIYY_df %>%
  full_join(BUSAPPWNSAUS_df, by = "date") %>%
  arrange(date)

combined_data <- combined_data %>%
  rename(
    Michigan = BUSAPPWNSAMIYY,
    United_States = BUSAPPWNSAUS
  )

saveRDS(combined_data, "data/businessAppChange.rds")