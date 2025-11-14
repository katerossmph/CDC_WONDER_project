# ==================================================
# Functions: Data Cleaning and Utilities
# Author: Kate Ross
# ==================================================

# Function to clean claims data
clean_mortality <- function(df) {
  df_clean <- df %>%
    # Remove fully empty rows
    filter(if_any(everything(), ~!is.na(.))) %>%
    
    # Standardize column names
    rename(
      county = County,
      county_code = `County Code`,
      cause = `Cause of death`,
      cause_code = `Cause of death Code`,
      year = Year,
      year_code = `Year Code`,
      age_group = `Ten-Year Age Groups`,
      age_code = `Ten-Year Age Groups Code`,
      sex = Sex,
      sex_code = `Sex Code`,
      deaths = Deaths
    ) %>%
    
    # Convert types
    mutate(
      year = as.integer(year),
      deaths = as.numeric(deaths),
      age_group = as.factor(age_group),
      sex = as.factor(sex),
      county = as.factor(county),
      cause = as.factor(cause)
    ) %>%
    
    # Remove rows with missing deaths
    filter (!is.na(deaths))
  
  return(df_clean)
}

# Purpose: Calculate mortality rates by county, sex, age group, and cause
calc_mortality_rate <- function(df) {
  
  # currently just calculating ** crude mortality rates** (deaths per 100,000)
  
  df_rates <- df %>%
    group_by(county, sex, age_group, cause, year) %>%
    summarise(
      deaths_total = sum(deaths, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    # Placeholder crude rate; replace with population if available
      # Currently summing deaths per grouping
    # With population data, can calculate age-specific or age-standardized rates
    mutate(rate_per_100k = deaths_total / 1) # Replace 1 with population
  
  return(df_rates)
}


## Plotting trends and distributions of mortality
library(ggplot2)
library(here)

  # Trend over years for a specific cause
plot_trend <- function(df, cause) {
  df %>%
    filter(cause == !cause) %>%
    ggplot(aes(x = year, y = rate_per_100k, color = sex)) +
    geom_line() + 
    geom_point() + 
    facet_wrap(~county) +
    labs(title = paste("Mortality Trend:", cause),
         x = "Year", y = "Mortality Rate (per 100k)") +
    theme_minimal()
}

# Age Group distirbution across counties
plot_age_distribution <- function(df) {
  ggplot(df, aes(x = age_group, y = rate_per_100k, fill = sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~county) +
    labs(
      title = "Mortality Rate by Age Group",
      x = "Age Group",
      y = "Mortality Rate (per 100k)"
    ) +
    theme_minimal()
}