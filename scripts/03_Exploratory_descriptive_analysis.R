# 03_exploratory_descriptive_analysis.R
# Purpose: Explore and describe cleaned Massachusetts chronic disease mortality data

# Load cleaned data
clean_data <- read_csv(here("data/processed/clean_data.csv"))
mort_selected <- read_csv(here("data/processed/mort_selected.csv"))

# -------------------------------
# 1. Quick overview of the data
# -------------------------------
dim(clean_data)         # Rows x columns
colnames(clean_data)    # Check variable names
str(clean_data)         # Check types
summary(clean_data)     # Quick summary of deaths, years, etc.

# -------------------------------
# 2. Overall deaths by cause
# -------------------------------

    # By Cause Category
overall_by_cause <- mort_selected %>%
  filter(cause_code %in% selected_icd_codes) %>%
  group_by(cause_category) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE)) %>%
  arrange(desc(total_deaths))

print(overall_by_cause)

# -------------------------------
# 3. Deaths by year
# -------------------------------
overall_by_year <- clean_data %>%
  group_by(year) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE))

print(overall_by_year)

# -------------------------------
# 4. Deaths by sex
# -------------------------------
overall_by_sex <- clean_data %>%
  group_by(sex) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE))

cat("=== Total Deaths per Sex ===\n")
print(overall_by_sex)

sex_by_cause <- clean_data %>%
  filter(cause_category %in% c("Heart Disease", "Diabetes", "Stroke")) %>%
  group_by(sex) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE))

print("Total Deaths per Sex of Heart Disease, Diabetes, and Stroke:")
print(sex_by_cause)

# -------------------------------
# 5. Deaths by age group
# -------------------------------
overall_by_age <- clean_data %>%
  group_by(age_group) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE))

print(overall_by_age)

# -------------------------------
# 6. Deaths by county
# -------------------------------
overall_by_county <- clean_data %>%
  group_by(county) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE)) %>%
  arrange(desc(total_deaths))

print(overall_by_county)

# -------------------------------
# 7. Cross-tab summaries
# -------------------------------
# Cause x Sex
cause_sex_table <- clean_data %>%
  group_by(cause, sex) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE)) %>%
  arrange(cause, sex)

  # How to filter by specific causes???

print(cause_sex_table)

# Cause x Age Group
cause_age_table <- clean_data %>%
  group_by(cause, age_group) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE)) %>%
  arrange(cause, age_group)

print(cause_age_table)

    # By specific ICD Codes
cause_age_table_ICD <- clean_data %>%
  filter(cause_category %in% c("Heart Disease", "Diabetes", "Stroke")) %>%
  group_by(cause_category, age_group) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE)) %>%
  arrange(cause_category, age_group)

cause_age_wider_ICD <- cause_age_table_ICD %>%
  pivot_wider(
    names_from = cause_category,  # Column names come from causes
    values_from = total_deaths,   # Values come from total_deaths
    values_fill = 0               # Fill missing values with 0
  )

print(cause_age_table_ICD)


# Cause x County
cause_county_table <- clean_data %>%
  group_by(cause, county) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE)) %>%
  arrange(cause, desc(total_deaths))

print(cause_county_table)

  # By specific ICD Codes

cause_county_table_ICD <- clean_data %>%
  filter(cause_category %in% c("Heart Disease", "Diabetes", "Stroke")) %>%
  group_by(cause_category, county) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE)) %>%
  arrange(cause_category, desc(total_deaths))

case_county_wide_ICD <- cause_county_table_ICD %>%
  pivot_wider(
    names_from = cause_category,  # Column names come from causes
    values_from = total_deaths,   # Values come from total_deaths
    values_fill = 0               # Fill missing values with 0
  )

print(case_county_wide_ICD)


overall_by_county_ICD <- clean_data %>%
  group_by(county) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE)) %>%
  arrange(desc(total_deaths))

print(overall_by_county_ICD)

# -------------------------------
# 8. Optional: Quick visualizations for exploration
# -------------------------------
library(ggplot2)

# Trend over years for all causes
ggplot(clean_data, aes(x = year, y = deaths, color = cause_category)) +
  geom_line(stat = "summary", fun = sum) +
  labs(title = "Total Deaths by Cause Over Years",
       x = "Year", y = "Total Deaths") +
  theme_minimal()

# Age distribution by cause
ggplot(clean_data, aes(x = age_group, y = deaths, fill = cause_category)) +
  geom_bar(stat = "summary", fun = sum, position = "dodge") +
  labs(title = "Deaths by Age Group and Cause",
       x = "Age Group", y = "Total Deaths") +
  theme_minimal()

# Sex distribution by cause
ggplot(clean_data, aes(x = sex, y = deaths, fill = cause_category)) +
  geom_bar(stat = "summary", fun = sum, position = "dodge") +
  labs(title = "Deaths by Sex and Cause",
       x = "Sex", y = "Total Deaths") +
  theme_minimal()


#### SAVE ####
write_csv(clean_data, here("data", "processed", "clean_data.csv"))
write_csv(mort_selected, here("data", "processed", "mort_selected.csv"))
