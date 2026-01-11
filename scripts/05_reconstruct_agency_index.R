# ============================================================================
# Reconstruct True Agency Index from Raw GEM Variables
# ============================================================================
# Purpose: Build proper agency index from fear, skills, know, opportunity
# ============================================================================

library(haven)
library(tidyverse)

wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

data_raw_path <- "data_raw"
data_processed_path <- "data_processed"

cat("\n=== Reconstructing Agency Index from Raw Variables ===\n\n")

# Extract year from filename
extract_year <- function(filename) {
  year_match <- str_extract(basename(filename), "\\d{4}")
  return(as.numeric(year_match))
}

# Find all APS individual-level files
aps_files <- list.files(
  data_raw_path,
  pattern = "APS Global.*Individual.*Level.*\\.sav$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Process each file
aps_agency_list <- list()

for (filepath in aps_files) {
  filename <- basename(filepath)
  year <- extract_year(filename)
  
  cat(sprintf("Processing %s (year %d)...\n", filename, year))
  
  tryCatch({
    df <- read_sav(filepath) %>% as_tibble()
    names(df) <- tolower(names(df))
    
    # Map variable names across years (handling slight variations)
    # 2019 has extra 'l' at end: fearfaill, suskilll, knowentr, opportl
    var_maps <- list(
      fear = c("fearfail", "fearfaill"),
      skill = c("suskill", "suskilll"),
      know = c("knowent", "knowentr"),
      opport = c("opport", "opportl")
    )
    
    # Find which variables exist in this year
    available_vars <- tolower(names(df))
    
    # Get correct variable names for this year
    fear_var <- intersect(var_maps$fear, available_vars)[1]
    skill_var <- intersect(var_maps$skill, available_vars)[1]
    know_var <- intersect(var_maps$know, available_vars)[1]
    opport_var <- intersect(var_maps$opport, available_vars)[1]
    
    cat(sprintf("  Found: fear=%s, skill=%s, know=%s, opport=%s\n",
                ifelse(is.na(fear_var), "NOT FOUND", fear_var),
                ifelse(is.na(skill_var), "NOT FOUND", skill_var),
                ifelse(is.na(know_var), "NOT FOUND", know_var),
                ifelse(is.na(opport_var), "NOT FOUND", opport_var)))
    
    # Extract relevant variables
    cols_to_select <- c("country", "gender", "age", "gemeduc")
    
    if (!is.na(fear_var)) cols_to_select <- c(cols_to_select, fear_var)
    if (!is.na(skill_var)) cols_to_select <- c(cols_to_select, skill_var)
    if (!is.na(know_var)) cols_to_select <- c(cols_to_select, know_var)
    if (!is.na(opport_var)) cols_to_select <- c(cols_to_select, opport_var)
    
    # Also get TEA variable
    tea_cols <- c("teayy", "ipteamsize")
    tea_var <- intersect(tea_cols, available_vars)[1]
    if (!is.na(tea_var)) cols_to_select <- c(cols_to_select, tea_var)
    
    df_subset <- df %>%
      select(all_of(cols_to_select)) %>%
      mutate(year = year)
    
    # Rename to standard names
    if (!is.na(fear_var)) df_subset <- df_subset %>% rename(fearfail = all_of(fear_var))
    if (!is.na(skill_var)) df_subset <- df_subset %>% rename(suskill = all_of(skill_var))
    if (!is.na(know_var)) df_subset <- df_subset %>% rename(knowent = all_of(know_var))
    if (!is.na(opport_var)) df_subset <- df_subset %>% rename(opport = all_of(opport_var))
    if (!is.na(tea_var)) df_subset <- df_subset %>% rename(tea = all_of(tea_var))
    
    aps_agency_list[[as.character(year)]] <- df_subset
    
    cat(sprintf("  Rows: %d\n", nrow(df_subset)))
    
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
  })
}

# Combine all years
cat("\n=== Combining Years ===\n")

aps_agency <- bind_rows(aps_agency_list) %>%
  mutate(
    # Standardize variable names to lowercase
    country = tolower(as.character(country)),
    gender = as.numeric(gender),
    age = as.numeric(age),
    gemeduc = as.numeric(gemeduc),
    year = as.numeric(year),
    # Binary indicators: convert to 0/1
    fear_binary = ifelse(fearfail == 1, 1, 0),  # 1 = yes, fear of failure
    skill_binary = ifelse(suskill == 1, 1, 0),  # 1 = yes, has skills
    know_binary = ifelse(knowent == 1, 1, 0),   # 1 = yes, knows entrepreneur
    opport_binary = ifelse(opport == 1, 1, 0),  # 1 = yes, sees opportunity
    tea_binary = ifelse(tea %in% c(1, 2, 3), 1, 0)  # Involved in TEA
  )

cat(sprintf("Combined: %d rows, %d columns\n", nrow(aps_agency), ncol(aps_agency)))
cat(sprintf("Years: %s\n", paste(sort(unique(aps_agency$year)), collapse = ", ")))

# ============================================================================
# Create Agency Index (0-1 scale)
# ============================================================================
# Agency index = mean of components (each normalized 0-1)

cat("\n=== Creating Agency Index ===\n")

aps_agency <- aps_agency %>%
  mutate(
    # Agency index: average of 4 components
    # Inverse fear (1 - fear): lower fear = higher agency
    agency_index = (
      (1 - fear_binary) +  # Inverse: not afraid
      skill_binary +        # Has skills
      know_binary +         # Knows entrepreneurs
      opport_binary         # Sees opportunity
    ) / 4,
    # Count non-missing agency items
    agency_items_valid = (!is.na(fear_binary)) + (!is.na(skill_binary)) +
                         (!is.na(know_binary)) + (!is.na(opport_binary))
  )

cat("Agency index created (0-1 scale, where 1 = highest agency)\n")
cat(sprintf("Mean agency: %.3f (SD: %.3f)\n", 
            mean(aps_agency$agency_index, na.rm = TRUE),
            sd(aps_agency$agency_index, na.rm = TRUE)))

# Show distribution
cat("\nAgency Index Distribution:\n")
print(quantile(aps_agency$agency_index, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))

# ============================================================================
# Aggregate to Country-Year Level
# ============================================================================

cat("\n=== Aggregating to Country-Year ===\n")

aps_agency_agg <- aps_agency %>%
  group_by(country, year) %>%
  summarise(
    n_obs = n(),
    n_valid_agency = sum(!is.na(agency_index)),
    agency_mean = mean(agency_index, na.rm = TRUE),
    agency_sd = sd(agency_index, na.rm = TRUE),
    fear_pct = mean(fear_binary, na.rm = TRUE) * 100,
    skill_pct = mean(skill_binary, na.rm = TRUE) * 100,
    know_pct = mean(know_binary, na.rm = TRUE) * 100,
    opport_pct = mean(opport_binary, na.rm = TRUE) * 100,
    tea_pct = mean(tea_binary, na.rm = TRUE) * 100,
    age_mean = mean(age, na.rm = TRUE),
    female_pct = mean(gender == 2, na.rm = TRUE) * 100,  # gender: 1=male, 2=female
    education_mean = mean(gemeduc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, year) %>%
  # Create country ID as numeric hash (consistent across years)
  mutate(
    country_id = as.integer(as.factor(country))
  ) %>%
  select(country, country_id, year, everything())

cat(sprintf("Aggregated: %d country-year combinations\n", nrow(aps_agency_agg)))
cat(sprintf("Countries: %d unique\n", n_distinct(aps_agency_agg$country)))

# Show summary by year
cat("\n=== Agency Metrics by Year ===\n")
summary_by_year <- aps_agency_agg %>%
  group_by(year) %>%
  summarise(
    n_countries = n_distinct(country_id),
    mean_agency = mean(agency_mean, na.rm = TRUE),
    mean_tea_pct = mean(tea_pct, na.rm = TRUE),
    mean_fear_pct = mean(fear_pct, na.rm = TRUE),
    mean_skill_pct = mean(skill_pct, na.rm = TRUE),
    mean_know_pct = mean(know_pct, na.rm = TRUE),
    mean_opport_pct = mean(opport_pct, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_by_year)

# ============================================================================
# Save Results
# ============================================================================

cat("\n=== Saving Results ===\n")

# Individual-level with agency
agency_file <- file.path(data_processed_path, "aps_agency_individual_2009_2020.rds")
saveRDS(aps_agency, agency_file)
cat(sprintf("Saved individual-level data: %s\n", agency_file))

# Aggregated country-year
agg_file <- file.path(data_processed_path, "aps_agency_country_year_2009_2020.rds")
saveRDS(aps_agency_agg, agg_file)
cat(sprintf("Saved aggregated data: %s\n", agg_file))

# Summary by year
summary_file <- file.path(data_processed_path, "agency_summary_by_year.csv")
write_csv(summary_by_year, summary_file)
cat(sprintf("Saved summary: %s\n\n", summary_file))

cat("=== Agency Index Reconstruction Complete ===\n")
