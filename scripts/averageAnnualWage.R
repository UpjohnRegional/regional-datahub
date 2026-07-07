rm(list = ls())

library(blscrapeR)
library(tidyverse)
library(tidyr)
library(dplyr)

averageAnnualWageMI <- bls_api(c("ENU2600050511", "ENU2600050521", "ENU2600050522", "ENU2600050523", "ENU2600050531-33",
                                 "ENU2600050542", "ENU2600050544-45", "ENU2600050548-49", "ENU2600050551", "ENU2600050552",
                                 "ENU2600050553", "ENU2600050554", "ENU2600050555", "ENU2600050556", "ENU2600050561", 
                                 "ENU2600050562", "ENU2600050571", "ENU2600050572", "ENU2600050581"), 
                               startyear = 2012, endyear = 2024, 
                               registrationKey = "c4c9bd6207ad4323a8d4fe18fd6960e0") %>%
  spread(seriesID, value) %>%
  dateCast() 

averageAnnualWageMI_long <- averageAnnualWageMI %>%
  pivot_longer(
    cols = starts_with("ENU"),
    names_to = "seriesID",
    values_to = "value"
  )

averageAnnualWageMI_long <- averageAnnualWageMI_long %>%
  mutate(seriesID = case_when(
    seriesID == "ENU2600050511" ~ "NAICS 11 Agriculture, forestry, fishing and hunting",
    seriesID == "ENU2600050521" ~ "NAICS 21 Mining, quarrying, and oil and gas extraction",
    seriesID == "ENU2600050522" ~ "NAICS 22 Utilities",
    seriesID == "ENU2600050523" ~ "NAICS 23 Construction",
    seriesID == "ENU2600050531-33" ~ "NAICS 31-33 Manufacturing",
    seriesID == "ENU2600050542" ~ "NAICS 42 Wholesale trade",
    seriesID == "ENU2600050544-45" ~ "NAICS 44-45 Retail trade",
    seriesID == "ENU2600050548-49" ~ "NAICS 48-49 Transportation and warehousing",
    seriesID == "ENU2600050551" ~ "NAICS 51 Information",
    seriesID == "ENU2600050552" ~ "NAICS 52 Finance and insurance",
    seriesID == "ENU2600050553" ~ "NAICS 53 Real estate and rental and leasing",
    seriesID == "ENU2600050554" ~ "NAICS 54 Professional, scientific, and technical services",
    seriesID == "ENU2600050555" ~ "NAICS 55 Management of companies and enterprises",
    seriesID == "ENU2600050556" ~ "NAICS 56 Administrative and support and waste management",
    seriesID == "ENU2600050561" ~ "NAICS 61 Educational services",
    seriesID == "ENU2600050562" ~ "NAICS 62 Health care and social assistance",
    seriesID == "ENU2600050571" ~ "NAICS 71 Arts, entertainment, and recreation",
    seriesID == "ENU2600050572" ~ "NAICS 72 Accommodation and food services",
    seriesID == "ENU2600050581" ~ "NAICS 81 Other services (except public administration)",
    TRUE ~ seriesID  # Keep original name if not listed
  ))

averageAnnualWageMI_long <- averageAnnualWageMI_long %>%
  arrange(seriesID)

averageAnnualWageUS <- bls_api(c("ENUUS00050511", "ENUUS00050521", "ENUUS00050522", "ENUUS00050523", "ENUUS00050531-33",
                                 "ENUUS00050542", "ENUUS00050544-45", "ENUUS00050548-49", "ENUUS00050551", "ENUUS00050552",
                                 "ENUUS00050553", "ENUUS00050554", "ENUUS00050555", "ENUUS00050556", "ENUUS00050561", 
                                 "ENUUS00050562", "ENUUS00050571", "ENUUS00050572", "ENUUS00050581"), 
                               startyear = 2012, endyear = 2024, 
                               registrationKey = "c4c9bd6207ad4323a8d4fe18fd6960e0") %>%
  spread(seriesID, value) %>%
  dateCast()

averageAnnualWageUS_long <- averageAnnualWageUS %>%
  pivot_longer(
    cols = starts_with("ENU"),
    names_to = "seriesID",
    values_to = "value"
  )

averageAnnualWageUS_long <- averageAnnualWageUS_long %>%
  mutate(seriesID = case_when(
    seriesID == "ENUUS00050511" ~ "NAICS 11 Agriculture, forestry, fishing and hunting",
    seriesID == "ENUUS00050521" ~ "NAICS 21 Mining, quarrying, and oil and gas extraction",
    seriesID == "ENUUS00050522" ~ "NAICS 22 Utilities",
    seriesID == "ENUUS00050523" ~ "NAICS 23 Construction",
    seriesID == "ENUUS00050531-33" ~ "NAICS 31-33 Manufacturing",
    seriesID == "ENUUS00050542" ~ "NAICS 42 Wholesale trade",
    seriesID == "ENUUS00050544-45" ~ "NAICS 44-45 Retail trade",
    seriesID == "ENUUS00050548-49" ~ "NAICS 48-49 Transportation and warehousing",
    seriesID == "ENUUS00050551" ~ "NAICS 51 Information",
    seriesID == "ENUUS00050552" ~ "NAICS 52 Finance and insurance",
    seriesID == "ENUUS00050553" ~ "NAICS 53 Real estate and rental and leasing",
    seriesID == "ENUUS00050554" ~ "NAICS 54 Professional, scientific, and technical services",
    seriesID == "ENUUS00050555" ~ "NAICS 55 Management of companies and enterprises",
    seriesID == "ENUUS00050556" ~ "NAICS 56 Administrative and support and waste management",
    seriesID == "ENUUS00050561" ~ "NAICS 61 Educational services",
    seriesID == "ENUUS00050562" ~ "NAICS 62 Health care and social assistance",
    seriesID == "ENUUS00050571" ~ "NAICS 71 Arts, entertainment, and recreation",
    seriesID == "ENUUS00050572" ~ "NAICS 72 Accommodation and food services",
    seriesID == "ENUUS00050581" ~ "NAICS 81 Other services (except public administration)",
    TRUE ~ seriesID  # Keep original name if not listed
  ))

averageAnnualWageUS_long <- averageAnnualWageUS_long %>%
  arrange(seriesID)

joinedAnnualWageData <- inner_join(averageAnnualWageMI_long, averageAnnualWageUS_long, by = c("date", "seriesID"), suffix = c("_MI", "_US"))

saveRDS(joinedAnnualWageData, "data/averageAnnualWage.rds")