# 04_analysis.R
# Purpose: Formal analysis of Massachusetts chronic disease mortality
#          Focus: Heart Disease, Diabetes, Stroke

library(tidyverse)
library(here)
library(ggplot2)
library(tidyr)

# -------------------------------
# 1. Load cleaned and filtered data
# -------------------------------
clean_data <- read_csv(here("data/processed/clean_data.csv"))
mort_selected <- read_csv(here("data/processed/mort_selected.csv"))

# -------------------------------
# 2. Total deaths by cause (Heart Disease, Diabetes, Stroke)
# -------------------------------
total_deaths_by_cause <- mort_selected %>%
  group_by(cause_category) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_deaths))

cat("=== Total Deaths by Cause ===\n")
print(total_deaths_by_cause)

# -------------------------------
# 3. Deaths over time by cause
# -------------------------------
deaths_by_year <- mort_selected %>%
  group_by(cause_category, year) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop")

deaths_by_year_wide <- deaths_by_year %>%
  pivot_wider(
    names_from = cause_category,
    values_from = total_deaths,
    values_fill = 0
  )

# Plot trends
ggplot(deaths_by_year, aes(x = year, y = total_deaths, color = cause_category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Trends in Deaths by Cause (2018-2023)",
       x = "Year", y = "Total Deaths", color = "Cause") +
  theme_minimal()

# -------------------------------
# 4. Deaths by sex
# -------------------------------
deaths_by_sex <- mort_selected %>%
  group_by(year, cause_category, sex) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop")

deaths_by_sex_wide <- deaths_by_sex %>%
  pivot_wider(
    names_from = cause_category,
    values_from = total_deaths,
    values_fill = 0
  )

# Plot
ggplot(deaths_by_sex, aes(x = sex, y = total_deaths, fill = cause_category)) +
  geom_bar(stat = "identity", position = "dodge") +
#  facet_wrap(~year) + 
  labs(title = "Deaths by Sex and Cause",
       x = "Sex", y = "Total Deaths", fill = "Cause") +
  theme_minimal()

# -------------------------------
# 5. Deaths by age group
# -------------------------------
deaths_by_age <- mort_selected %>%
  group_by(cause_category, age_group) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop")

# Pivot wide for reporting
deaths_by_age_wide <- deaths_by_age %>%
  pivot_wider(names_from = cause_category, values_from = total_deaths, values_fill = 0)

print(deaths_by_age_wide)

# Plot
ggplot(deaths_by_age, aes(x = age_group, y = total_deaths, fill = cause_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Deaths by Age Group and Cause",
       x = "Age Group", y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------------
# 6. Deaths by county
# -------------------------------
deaths_by_county <- mort_selected %>%
  group_by(cause_category, county) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(cause_category, desc(total_deaths))

# Pivot wide for reporting
deaths_by_county_wide <- deaths_by_county %>%
  pivot_wider(names_from = cause_category, values_from = total_deaths, values_fill = 0)

print(deaths_by_county_wide)

# Optional plot: top 10 counties per cause
top_counties_plot <- deaths_by_county %>%
  group_by(cause_category) %>%
  slice_max(order_by = total_deaths, n = 10) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(county, total_deaths), y = total_deaths, fill = cause_category)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Top 10 Counties by Cause (Total Deaths)",
       x = "County", y = "Total Deaths", fill = "Cause") +
  theme_minimal()

print(top_counties_plot)

# Optional plot: top 10 counties per cause over time
top_counties_year <- mort_selected %>%
  group_by(year, cause_category, county) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  group_by(year, cause_category) %>%
  slice_max(order_by = total_deaths, n = 10) %>% # top 10 per year x cause
  ungroup()

print(top_counties_plot)

    # Plotting top counties faceted by year
ggplot(top_counties_year, aes(x = reorder(county, total_deaths), y = total_deaths, fill = cause_category)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_grid(cause_category ~ year, scales = "free_y") + # separate panel for each cause x year
  scale_y_continuous (labels = scales::label_comma()) + # Nicer y-axis formatting
  labs(title = "Top 10 Counties by Cause (Total Deaths)",
       x = "County", y = "Total Deaths", fill = "Cause") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

## GGANIMATE
install.packages(c("gganimate", "gifski", "transformr", "forcats"))
library(gganimate)
library(forcats)
library(ggplot2)
library(dplyr)

top_counties_by_year <- mort_selected %>%
  group_by(year, cause_category, county) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  group_by(year, cause_category) %>%
  slice_max(order_by = total_deaths, n = 10) %>% # Top 10
  ungroup()

top_counties_by_year <- top_counties_by_year %>%
  mutate(total_deaths = as.numeric(total_deaths))

animated_plot <- ggplot(top_counties_by_year,
                        aes(x = reorder(county, total_deaths), y = total_deaths, fill = cause_category)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(round(total_deaths, 0))),
            hjust = -0.1, size = 3) + # WHOLE Numbers on bars
  coord_flip() + 
  scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Top 10 Counties by Cause in Massachusetts",
       x = "County", y = "Total Deaths") +
  facet_wrap(~cause_category, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out')

animate(animated_plot, nframes = 100, fps = 10, width = 800, height = 600, renderer = gifski_renderer())

# -------------------------------
# 7. Deaths by Year x Cause x Sex
# -------------------------------
deaths_trend <- mort_selected %>%
  group_by(year, cause_category, sex) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop")

library(scales)

ggplot(deaths_trend, aes(x = year, y = total_deaths, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~cause_category, scales = "free_y") +
  scale_x_continuous(breaks = c(2021, 2022, 2023)) +
  scale_y_continuous(labels = label_comma()) + 
  labs(title = "Trends in Deaths by Cause and Sex (2018-2023)", 
       x = "Year",
       y = "Total Deaths",
       color = "Sex") +
  theme_minimal()

# -------------------------------
# 7. Optional: Save analysis outputs
# -------------------------------
write_csv(total_deaths_by_cause, here("outputs/tables/total_deaths_by_cause.csv"))
write_csv(deaths_by_age_wide, here("outputs/tables/deaths_by_age_wide.csv"))
write_csv(deaths_by_county_wide, here("outputs/tables/deaths_by_county_wide.csv"))
