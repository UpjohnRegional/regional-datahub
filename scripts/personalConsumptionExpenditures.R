rm(list = ls())

library(fredr)
library(dplyr)
library(readr)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

DSPIC96 <- fredr(
  series_id = "DSPIC96",
  observation_start = as.Date("1950-01-01"),
  observation_end   = Sys.Date()
)

PCEDGC96 <- fredr(
  series_id = "PCEDGC96",
  observation_start = as.Date("1950-01-01"),
  observation_end   = Sys.Date()
)

PCENDC96 <- fredr(
  series_id = "PCENDC96",
  observation_start = as.Date("1950-01-01"),
  observation_end   = Sys.Date()
)

PCESC96 <- fredr(
  series_id = "PCESC96",
  observation_start = as.Date("1950-01-01"),
  observation_end   = Sys.Date()
)

DSPIC96_df <- DSPIC96 %>%
  select(date, value) %>%
  rename(DSPIC96 = value)

PCEDGC96_df <- PCEDGC96 %>%
  select(date, value) %>%
  rename(PCEDGC96 = value)

PCENDC96_df <- PCENDC96 %>%
  select(date, value) %>%
  rename(PCENDC96 = value)

PCESC96_df <- PCESC96 %>%
  select(date, value) %>%
  rename(PCESC96 = value)

combined_data <- DSPIC96_df %>%
  full_join(PCEDGC96_df, by = "date") %>%
  full_join(PCENDC96_df, by = "date") %>%
  full_join(PCESC96_df, by = "date") %>%
  arrange(date)

combined_data <- combined_data %>%
  rename(
    disposable_income = DSPIC96,
    durable_goods     = PCEDGC96,
    nondurable_goods  = PCENDC96,
    services          = PCESC96
  )

combined_data <- combined_data %>%
  filter(date >= as.Date("2007-01-01"))

str(combined_data$date)

names(combined_data)

saveRDS(combined_data, "data/fred_spending.rds")