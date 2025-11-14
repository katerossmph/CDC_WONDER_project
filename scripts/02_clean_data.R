library(tidyverse)
library(here)

# Load raw data
mort_clean <- read_csv(here("data", "processed", "mort_clean.csv"))

# Clean Data
clean_data <- clean_mortality(mort_clean)

# Filtering Out specific Case Codes (Heart, Diabetes, and Stroke)
selected_icd_codes <- c("I00", "I01","I02","I03","I04","I05","I06","I07","I08","I09", 
                        "I11.0", "I11.9", "I13.2", "I21.4", "I21.9", "I25.0", "I25.1", 
                        "I25.5", "I26.9", "I27.2", "I34.0", "I35.0", "I42.9", "I46.9",
                        "I48", "I49.9", "I50.0", "I50.9", "I51.6", "I51.9", # Heart Disease
                        "E11.9", "E14.2","E14.9",                           # Diabetes
                        "I61.9", "I63.9", "I64", "I67.9", "I69.4", "I69.8") # Stroke

# Recategorizing with factors
clean_data <- clean_mortality(mort_clean) %>%
  mutate(
    cause_category = case_when(
      # Heart Disease ICD Codes
      cause_code %in% c("I00", "I01","I02","I03","I04","I05","I06","I07","I08","I09", 
                        "I11.0", "I11.9", "I13.2", "I21.4", "I21.9", "I25.0", "I25.1", 
                        "I25.5", "I26.9", "I27.2", "I34.0", "I35.0", "I42.9", "I46.9",
                        "I48", "I49.9", "I50.0", "I50.9", "I51.6", "I51.9") ~ "Heart Disease",
      
      # Diabetes ICD Codes
      cause_code %in% c("E11.9", "E14.2","E14.9") ~ "Diabetes",
      
      # Stroke ICD Codes
      cause_code %in% c("I61.9", "I63.9", "I64", "I67.9", "I69.4", "I69.8") ~ "Stroke",
      
      # All other causes
      TRUE ~ "Other"
      
    )
  ) 


mort_selected <- clean_data %>%
  filter(cause_code %in% selected_icd_codes)


# Save
write_csv(clean_data, here("data", "processed", "clean_data.csv"))
write_csv(mort_selected, here("data", "processed", "mort_selected.csv"))
