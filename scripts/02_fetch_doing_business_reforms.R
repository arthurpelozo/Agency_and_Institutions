# ============================================================================
# Create Regulatory Reform Shock Data for DiD
# ============================================================================
# Purpose: Create reform indicators for DiD analysis
# ============================================================================

library(tidyverse)

wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

data_processed_path <- "data_processed"

cat("\n=== Creating Reform Shock Data ===\n")

# Load APS data to get list of countries and years
aps_agg <- readRDS(file.path(data_processed_path, "aps_country_year_2005_2021.rds"))

# Get unique countries and years in APS data
countries_in_aps <- unique(aps_agg$country_id)
years_in_aps <- sort(unique(aps_agg$year))

cat(sprintf("Countries in APS: %d\n", length(countries_in_aps)))
cat(sprintf("Years in APS: %s\n", paste(years_in_aps, collapse = ", ")))

# Create a panel of all country-years
reform_data <- expand_grid(
  country_id = countries_in_aps,
  year = years_in_aps
) %>%
  mutate(
    reform = 0,  # Placeholder: 0 = no reform in that year
    reform_binary = 0  # Binary treatment indicator
  ) %>%
  arrange(country_id, year)

cat(sprintf("\nCreated reform panel: %d rows\n", nrow(reform_data)))
cat("Countries: %d, Years: %d\n\n", length(unique(reform_data$country_id)), 
    length(unique(reform_data$year)))

# Save reform data
reform_file <- file.path(data_processed_path, "regulatory_reforms_2005_2021.rds")
saveRDS(reform_data, reform_file)
cat(sprintf("Saved reform data to: %s\n\n", reform_file))

cat("NOTE: Reform indicators are currently set to 0 (placeholder).\n")
cat("In production, these should be populated with actual reform data from:\n")
cat("  - World Bank Doing Business reports\n")
cat("  - Country legislation and policy databases\n")
cat("  - Academic databases (e.g., Regulatory Change Index)\n")

