# ============================================================================
# Load and Prepare GEM APS Data for Difference-in-Differences Analysis
# ============================================================================
# Purpose: Load all individual-level APS data from 2005-2021, harmonize,
#          and prepare for DiD on regulatory reform shocks
# ============================================================================

library(haven)
library(tidyverse)
library(data.table)

# Set working directory and paths
wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

data_raw_path <- "data_raw"
data_processed_path <- "data_processed"

# Ensure output directory exists
if (!dir.exists(data_processed_path)) {
  dir.create(data_processed_path, showWarnings = FALSE)
}

cat("\n=== Loading GEM APS Individual-Level Data (2005-2021) ===\n")

# List all APS individual-level files
aps_files <- list.files(
  data_raw_path,
  pattern = "APS Global.*Individual.*Level.*\\.sav$",
  full.names = TRUE,
  ignore.case = TRUE
)

cat(sprintf("Found %d APS individual-level files\n", length(aps_files)))
print(basename(aps_files))

# Extract year from filename
extract_year <- function(filename) {
  # Extract 4-digit year from filename
  year_match <- str_extract(basename(filename), "\\d{4}")
  if (is.na(year_match)) {
    year_match <- str_extract(basename(filename), "([0-9]{4})")
  }
  return(as.numeric(year_match))
}

# Load and harmonize each file
aps_list <- list()

for (filepath in aps_files) {
  filename <- basename(filepath)
  year <- extract_year(filename)
  
  cat(sprintf("Loading %s (year %d)...\n", filename, year))
  
  tryCatch({
    # Load SPSS file
    df <- read_sav(filepath) %>%
      as_tibble()
    
    # Convert column names to lowercase
    names(df) <- tolower(names(df))
    
    # Add year column
    df$year <- year
    
    # Identify key variables (will be harmonized below)
    cat(sprintf("  Rows: %d, Cols: %d\n", nrow(df), ncol(df)))
    
    aps_list[[as.character(year)]] <- df
    
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
  })
}

cat(sprintf("\nSuccessfully loaded %d files\n", length(aps_list)))

# ============================================================================
# Harmonize variable names and structure across years
# ============================================================================
cat("\n=== Harmonizing Variables ===\n")

# Show structure of first few datasets to identify key variables
for (i in 1:min(2, length(aps_list))) {
  year <- names(aps_list)[i]
  df <- aps_list[[i]]
  cat(sprintf("\nYear %s structure:\n", year))
  cat("Columns:\n")
  print(head(names(df), 20))
}

# Common entrepreneurship indicator names across years
tea_patterns <- c("tea", "tealevel", "tea_level", "early_stage", "nascent")
agency_patterns <- c("agency", "fear", "confidence", "skills", "know")
female_patterns <- c("female", "gender", "sex", "m001q02")
age_patterns <- c("age", "agegroup", "b_age")
country_patterns <- c("country", "countryid", "nation", "ncode")
education_patterns <- c("education", "educ", "schooling")

# Function to find column matching pattern
find_col <- function(df, patterns) {
  cols <- tolower(names(df))
  for (p in patterns) {
    matches <- cols[grepl(p, cols)]
    if (length(matches) > 0) {
      return(matches[1])
    }
  }
  return(NA_character_)
}

# Harmonize each dataset
aps_harmonized <- list()

for (year_str in names(aps_list)) {
  year_num <- as.numeric(year_str)
  df <- aps_list[[year_str]]
  
  cat(sprintf("\nHarmonizing year %d...\n", year_num))
  
  # Find key variables
  tea_col <- find_col(df, tea_patterns)
  female_col <- find_col(df, female_patterns)
  age_col <- find_col(df, age_patterns)
  country_col <- find_col(df, country_patterns)
  education_col <- find_col(df, education_patterns)
  
  cat(sprintf("  TEA column: %s\n", ifelse(is.na(tea_col), "NOT FOUND", tea_col)))
  cat(sprintf("  Female column: %s\n", ifelse(is.na(female_col), "NOT FOUND", female_col)))
  cat(sprintf("  Age column: %s\n", ifelse(is.na(age_col), "NOT FOUND", age_col)))
  cat(sprintf("  Country column: %s\n", ifelse(is.na(country_col), "NOT FOUND", country_col)))
  cat(sprintf("  Education column: %s\n", ifelse(is.na(education_col), "NOT FOUND", education_col)))
  
  # Create harmonized dataset
  df_harm <- df %>%
    select(all_of(c(country_col, age_col, female_col, education_col, tea_col, "year"))) %>%
    rename(
      country_id = all_of(country_col),
      age = all_of(age_col),
      female = all_of(female_col),
      education = all_of(education_col),
      tea = all_of(tea_col)
    ) %>%
    mutate(
      year = year_num,
      country_id = as.numeric(country_id),
      age = as.numeric(age),
      female = as.numeric(female),
      education = as.numeric(education),
      tea = as.numeric(tea)
    )
  
  aps_harmonized[[year_str]] <- df_harm
  cat(sprintf("  Harmonized: %d rows\n", nrow(df_harm)))
}

# Combine all years into single dataset
cat("\n=== Combining all years ===\n")
aps_combined <- bind_rows(aps_harmonized) %>%
  arrange(year, country_id)

cat(sprintf("Combined dataset: %d rows, %d columns\n", nrow(aps_combined), ncol(aps_combined)))
cat(sprintf("Years covered: %s\n", paste(sort(unique(aps_combined$year)), collapse = ", ")))
cat(sprintf("Countries: %d unique\n", n_distinct(aps_combined$country_id)))

# Save combined dataset
aps_file <- file.path(data_processed_path, "aps_combined_2005_2021.rds")
saveRDS(aps_combined, aps_file)
cat(sprintf("\nSaved combined APS data to: %s\n", aps_file))

# ============================================================================
# Create country-year aggregates for DiD
# ============================================================================
cat("\n=== Creating Country-Year Aggregates ===\n")

aps_agg <- aps_combined %>%
  group_by(country_id, year) %>%
  summarise(
    n_obs = n(),
    tea_rate = mean(tea, na.rm = TRUE),
    tea_count = sum(tea == 1, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    female_pct = mean(female == 1, na.rm = TRUE) * 100,
    education_mean = mean(education, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country_id, year)

cat(sprintf("Aggregated: %d country-year combinations\n", nrow(aps_agg)))
cat(sprintf("Countries: %d\n", n_distinct(aps_agg$country_id)))
cat(sprintf("Years: %s\n", paste(sort(unique(aps_agg$year)), collapse = ", ")))

# Save aggregated dataset
agg_file <- file.path(data_processed_path, "aps_country_year_2005_2021.rds")
saveRDS(aps_agg, agg_file)
cat(sprintf("\nSaved aggregated APS data to: %s\n", agg_file))

# Summary statistics
cat("\n=== Summary Statistics ===\n")
cat("\nTEA Rate by Year:\n")
aps_agg %>%
  group_by(year) %>%
  summarise(
    mean_tea = mean(tea_rate, na.rm = TRUE),
    sd_tea = sd(tea_rate, na.rm = TRUE),
    min_tea = min(tea_rate, na.rm = TRUE),
    max_tea = max(tea_rate, na.rm = TRUE),
    n_countries = n_distinct(country_id)
  ) %>%
  print()

cat("\n=== Data preparation complete ===\n")
cat("Files created:\n")
cat(sprintf("  1. %s\n", aps_file))
cat(sprintf("  2. %s\n", agg_file))
