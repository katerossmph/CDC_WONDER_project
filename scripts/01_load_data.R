library(tidyverse)
library(readxl)
library(here)

# Load Raw mortality data
mort_raw <- read_excel(here("data", "raw", "CDC_Death_2018_2023.xlsx"))
mort_clean <- mort_raw[1:1930,] # Removing the extra information at the bottom of the XLSX

race_raw <- read_excel(here("data", "raw", "CDC_Race_Death_2018_2023.xlsx"))
race_clean <- race_raw[1:1838,]

# Quick Check
glimpse(mort_clean)
summary(mort_clean)

# Save as a processed CSV for downstream scripts
write_csv(mort_clean, here("data", "processed", "mort_clean.csv"))
write_csv(race_clean, here("data", "processed", "race_clean.csv"))
