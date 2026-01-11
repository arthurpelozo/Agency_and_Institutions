# ============================================================
# Unified Youth Value-Creation Panel (APS 2005-2021)
# Purpose: Stack all APS individual-level files, build youth (18-30) panel,
#          compute agency index, and aggregate to country-year for models.
# ============================================================

library(tidyverse)
library(haven)

# Paths
wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

data_raw_path <- "data_raw"
data_processed_path <- "data_processed"
output_path <- "output"

if (!dir.exists(data_processed_path)) dir.create(data_processed_path, recursive = TRUE)
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

cat("\n=== Building unified youth panel (APS 2005-2021) ===\n")

# Helper to extract year from filename
extract_year <- function(filename) {
  year_match <- stringr::str_extract(basename(filename), "\\d{4}")
  as.numeric(year_match)
}

# Find all APS individual-level files across years
aps_files <- list.files(
  data_raw_path,
  pattern = "APS Global.*Individual.*Level.*\\.sav$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(aps_files) == 0) {
  stop("No APS individual-level files found in data_raw.")
}

cat(sprintf("Found %d APS files\n", length(aps_files)))

# Variable name maps (handle year-specific quirks)
var_maps <- list(
  fear = c("fearfail", "fearfaill"),
  skill = c("suskill", "suskilll"),
  know = c("knowent", "knowentr"),
  opport = c("opport", "opportl"),
  tea = c("teayy", "ipteamsize")
)

# Optional controls (only added if present)
control_vars <- c("hhsize", "gemhhinc", "gemoccu", "gemwork", "gemeduc")

aps_list <- list()

for (filepath in aps_files) {
  filename <- basename(filepath)
  year <- extract_year(filename)
  cat(sprintf("Processing %s (year %d)...\n", filename, year))

  tryCatch({
    df <- read_sav(filepath) %>% as_tibble()
    names(df) <- tolower(names(df))

    # Locate variables for this year
    available <- names(df)
    fear_var <- intersect(var_maps$fear, available)[1]
    skill_var <- intersect(var_maps$skill, available)[1]
    know_var <- intersect(var_maps$know, available)[1]
    opport_var <- intersect(var_maps$opport, available)[1]
    tea_var <- intersect(var_maps$tea, available)[1]

    # Build column list (mandatory core + detected)
    cols <- c("country", "gender", "age")
    cols <- unique(c(cols, intersect(control_vars, available)))
    if (!is.na(fear_var)) cols <- c(cols, fear_var)
    if (!is.na(skill_var)) cols <- c(cols, skill_var)
    if (!is.na(know_var)) cols <- c(cols, know_var)
    if (!is.na(opport_var)) cols <- c(cols, opport_var)
    if (!is.na(tea_var)) cols <- c(cols, tea_var)

    df_sub <- df %>%
      select(all_of(cols)) %>%
      mutate(year = year)

    # Standardize names
    if (!is.na(fear_var)) df_sub <- df_sub %>% rename(fearfail = all_of(fear_var))
    if (!is.na(skill_var)) df_sub <- df_sub %>% rename(suskill = all_of(skill_var))
    if (!is.na(know_var)) df_sub <- df_sub %>% rename(knowent = all_of(know_var))
    if (!is.na(opport_var)) df_sub <- df_sub %>% rename(opport = all_of(opport_var))
    if (!is.na(tea_var)) df_sub <- df_sub %>% rename(tea = all_of(tea_var))

    # Youth filter 18-30
    df_sub <- df_sub %>% filter(age >= 18, age <= 30)

    aps_list[[as.character(year)]] <- df_sub
    cat(sprintf("  rows (youth 18-30): %d\n", nrow(df_sub)))

  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
  })
}

# Combine years
cat("\n=== Combining years ===\n")
aps_youth <- bind_rows(aps_list) %>%
  mutate(
    country = tolower(as.character(country)),
    gender = as.numeric(gender),
    age = as.numeric(age),
    year = as.numeric(year),
    gemeduc = as.numeric(gemeduc),
    # Binary recodes
    fear_binary = ifelse(fearfail == 1, 1, 0),
    skill_binary = ifelse(suskill == 1, 1, 0),
    know_binary = ifelse(knowent == 1, 1, 0),
    opport_binary = ifelse(opport == 1, 1, 0),
    tea_binary = ifelse(tea %in% c(1, 2, 3), 1, 0)
  )

cat(sprintf("Combined youth rows: %d\n", nrow(aps_youth)))
cat(sprintf("Years: %s\n", paste(sort(unique(aps_youth$year)), collapse = ", ")))

# Agency index (0-1)
aps_youth <- aps_youth %>%
  mutate(
    agency_index = ((1 - fear_binary) + skill_binary + know_binary + opport_binary) / 4,
    agency_items_valid = (!is.na(fear_binary)) + (!is.na(skill_binary)) +
                         (!is.na(know_binary)) + (!is.na(opport_binary))
  )

cat(sprintf("Agency mean: %.3f (SD %.3f)\n",
            mean(aps_youth$agency_index, na.rm = TRUE),
            sd(aps_youth$agency_index, na.rm = TRUE)))

# Aggregate to country-year
cat("\n=== Aggregating to country-year ===\n")
aps_youth_agg <- aps_youth %>%
  group_by(country, year) %>%
  summarise(
    n_obs = n(),
    agency_mean = mean(agency_index, na.rm = TRUE),
    agency_sd = sd(agency_index, na.rm = TRUE),
    fear_pct = mean(fear_binary, na.rm = TRUE) * 100,
    skill_pct = mean(skill_binary, na.rm = TRUE) * 100,
    know_pct = mean(know_binary, na.rm = TRUE) * 100,
    opport_pct = mean(opport_binary, na.rm = TRUE) * 100,
    tea_pct = mean(tea_binary, na.rm = TRUE) * 100,
    age_mean = mean(age, na.rm = TRUE),
    female_pct = mean(gender == 2, na.rm = TRUE) * 100,
    education_mean = mean(gemeduc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(country_id = as.integer(as.factor(country))) %>%
  select(country, country_id, year, everything()) %>%
  arrange(country, year)

cat(sprintf("Aggregated country-years: %d\n", nrow(aps_youth_agg)))
cat(sprintf("Countries: %d\n", dplyr::n_distinct(aps_youth_agg$country)))

# Save outputs
ind_file <- file.path(data_processed_path, "aps_youth_panel_2005_2021.rds")
agg_file <- file.path(data_processed_path, "aps_youth_country_year_2005_2021.rds")

saveRDS(aps_youth, ind_file)
saveRDS(aps_youth_agg, agg_file)

cat("\nSaved:\n")
cat(sprintf("  %s\n", ind_file))
cat(sprintf("  %s\n", agg_file))

# Quick summary export for sanity checks
summary_file <- file.path(output_path, "youth_panel_summary.csv")
aps_youth_agg %>%
  group_by(year) %>%
  summarise(
    n_countries = n_distinct(country_id),
    mean_agency = mean(agency_mean, na.rm = TRUE),
    mean_tea = mean(tea_pct, na.rm = TRUE),
    mean_skill = mean(skill_pct, na.rm = TRUE),
    mean_know = mean(know_pct, na.rm = TRUE),
    mean_opport = mean(opport_pct, na.rm = TRUE),
    mean_fear = mean(fear_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  write_csv(summary_file)

cat(sprintf("Summary by year written to %s\n", summary_file))

cat("\n=== Done. Run models next using this youth panel. ===\n")
