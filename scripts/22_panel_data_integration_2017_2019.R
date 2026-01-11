################################################################################
# PHASE 22B: PANEL DATA INTEGRATION (2017-2019) - SIMPLIFIED
################################################################################
# Purpose: Start with 2017-2019 (more consistent variables) for immediate analysis
# Input:   3 years × APS individual data + NES country data  
# Output:  Panel dataset with ~120,000+ individuals
# Note:    2015-2016 integration deferred due to variable name changes
################################################################################

cat("\n")
cat("================================================================================\n")
cat("PHASE 22B: PANEL DATA INTEGRATION (2017-2019)\n")
cat("================================================================================\n")
cat("Building 3-year panel dataset for immediate causal analysis...\n\n")

# Load libraries
suppressPackageStartupMessages({
  library(haven)
  library(tidyverse)
})

# Create output directory
dir.create("data_processed/panel", recursive = TRUE, showWarnings = FALSE)

################################################################################
# STEP 1: LOAD 2017-2019 DATA (Known to be consistent)
################################################################################

cat("[1] Loading APS individual-level data (2017-2019)...\n")
cat("================================================================================\n")

# Load 2017 and 2018 raw data
cat("  Loading 2017 APS...")
aps_2017_raw <- read_sav("data_raw/GEM 2017 APS Global Individual Level Data_1April2021.sav")
cat(sprintf(" %d rows\n", nrow(aps_2017_raw)))

cat("  Loading 2018 APS...")
aps_2018_raw <- tryCatch({
  read_sav("data_raw/GEM 2018 APS Global Individual Level Data.sav", encoding = "UTF-8")
}, error = function(e) {
  read_sav("data_raw/GEM 2018 APS Global Individual Level Data.sav", encoding = "latin1")
})
cat(sprintf(" %d rows\n", nrow(aps_2018_raw)))

cat("  Loading 2019 APS...")
aps_2019_raw <- read_sav("data_raw/GEM 2019 APS Global Individual Level Data_30Jan2021.sav")
cat(sprintf(" %d rows\n", nrow(aps_2019_raw)))


################################################################################
# STEP 2: HARMONIZE 2017-2018 TO MATCH 2019 STRUCTURE
################################################################################

cat("\n[2] Harmonizing 2017-2018 to match 2019 structure...\n")
cat("================================================================================\n")

# Function to process 2017-2018 data
process_year <- function(data, year) {
  
  df <- as.data.frame(data)
  
  # Extract agency components (2017-2018 use: opport, suskill, fearfail, knowent)
  opstart <- as.numeric(df$opport)
  opskill <- as.numeric(df$suskill)
  fearfail <- as.numeric(df$fearfail)
  knowent <- as.numeric(df$knowent)
  
  # Construct agency index (binary components)
  opstart_binary <- ifelse(!is.na(opstart) & opstart == 1, 1, 0)
  opskill_binary <- ifelse(!is.na(opskill) & opskill == 1, 1, 0)
  fearfail_binary <- ifelse(!is.na(fearfail) & fearfail == 2, 1, 0)  # 2=no fear
  knowent_binary <- ifelse(!is.na(knowent) & knowent == 1, 1, 0)
  
  # Calculate mean (set to NA if all components missing)
  n_valid <- (!is.na(opstart) + !is.na(opskill) + !is.na(fearfail) + !is.na(knowent))
  agency_index <- ifelse(n_valid > 0,
                         (opstart_binary + opskill_binary + fearfail_binary + knowent_binary) / 4,
                         NA)
  
  # Extract TEA variable (different names by year)
  if (year == 2017) {
    tea_raw <- as.numeric(df$TEAyy)
  } else if (year == 2018) {
    tea_raw <- as.numeric(df$TEA18)
  } else {
    tea_raw <- NA
  }
  
  # TEA binary: 1=TEA active, 0=not active
  tea_binary <- ifelse(!is.na(tea_raw) & tea_raw == 1, 1, 0)
  
  # Demographics
  country_vec <- as.numeric(df$country)
  age_vec <- as.numeric(df$age)
  gender_vec <- as.numeric(df$gender)
  female_vec <- ifelse(!is.na(gender_vec) & gender_vec == 2, 1, 0)
  hhsize_vec <- as.numeric(df$hhsize)
  
  # Handle case variations
  gemeduc_vec <- if("gemeduc" %in% names(df)) {
    as.numeric(df$gemeduc)
  } else {
    as.numeric(df$GEMEDUC)
  }
  
  gemhhinc_vec <- if("gemhhinc" %in% names(df)) {
    as.numeric(df$gemhhinc)
  } else {
    as.numeric(df$GEMHHINC)
  }
  
  gemwork_vec <- if("gemwork" %in% names(df)) {
    as.numeric(df$gemwork)
  } else {
    as.numeric(df$GEMWORK)
  }
  
  weight_vec <- as.numeric(df$weight)
  
  # Debug lengths
  cat("      Lengths: country=", length(country_vec), 
      " age=", length(age_vec),
      " gender=", length(gender_vec),
      " female=", length(female_vec),
      " hhsize=", length(hhsize_vec),
      " gemeduc=", length(gemeduc_vec),
      " gemhhinc=", length(gemhhinc_vec),
      " gemwork=", length(gemwork_vec),
      " tea=", length(tea_binary),
      " agency=", length(agency_index),
      " weight=", length(weight_vec), "\n")
  
  # Create harmonized dataset
  harmonized <- data.frame(
    year = year,
    country = country_vec,
    age = age_vec,
    gender = gender_vec,
    female = female_vec,
    hhsize = hhsize_vec,
    gemeduc = gemeduc_vec,
    gemhhinc = gemhhinc_vec,
    gemwork = gemwork_vec,
    tea_binary = tea_binary,
    agency_index = agency_index,
    weight = weight_vec,
    stringsAsFactors = FALSE
  )
  
  # Age restriction
  harmonized_filtered <- harmonized[!is.na(harmonized$age) & 
                                     harmonized$age >= 18 & 
                                     harmonized$age <= 30, ]
  
  return(harmonized_filtered)
}

# Function to process 2019 data (Likert scales)
process_2019 <- function(data) {
  df <- as.data.frame(data)

  # Likert recode: treat agree/strongly agree (>=4) as positive
  likert_pos <- function(x) ifelse(!is.na(x) & x >= 4, 1, 0)

  opstart <- likert_pos(as.numeric(df$opportL))
  opskill <- likert_pos(as.numeric(df$oppismL))
  suskill <- likert_pos(as.numeric(df$suskillL))
  knowent_raw <- as.numeric(df$knowentR)
  knowent <- ifelse(!is.na(knowent_raw) & knowent_raw > 0, 1, 0)

  # Fear of failure: higher values = less fear (reverse-coded to positive)
  fearfail_val <- as.numeric(df$fearfailL)
  fearfail <- likert_pos(fearfail_val)

  agency_index <- (opstart + opskill + fearfail + knowent) / 4

  tea_raw <- as.numeric(df$TEAyy)
  tea_binary <- ifelse(!is.na(tea_raw) & tea_raw == 1, 1, 0)

  country_vec <- as.numeric(df$country)
  age_vec <- as.numeric(df$age)
  gender_vec <- as.numeric(df$gender)
  female_vec <- ifelse(!is.na(gender_vec) & gender_vec == 2, 1, 0)
  hhsize_vec <- as.numeric(df$hhsize)

  gemeduc_vec <- if("gemeduc" %in% names(df)) as.numeric(df$gemeduc) else as.numeric(df$GEMEDUC)
  gemhhinc_vec <- if("gemhhinc" %in% names(df)) as.numeric(df$gemhhinc) else as.numeric(df$GEMHHINC)
  gemwork_vec <- if("gemwork" %in% names(df)) as.numeric(df$gemwork) else as.numeric(df$GEMWORK)
  weight_vec <- as.numeric(df$weight)

  harmonized <- data.frame(
    year = 2019,
    country = country_vec,
    age = age_vec,
    gender = gender_vec,
    female = female_vec,
    hhsize = hhsize_vec,
    gemeduc = gemeduc_vec,
    gemhhinc = gemhhinc_vec,
    gemwork = gemwork_vec,
    tea_binary = tea_binary,
    agency_index = agency_index,
    weight = weight_vec,
    stringsAsFactors = FALSE
  )

  harmonized_filtered <- harmonized[!is.na(harmonized$age) & harmonized$age >= 18 & harmonized$age <= 30, ]
  return(harmonized_filtered)
}

# Process each year
cat("  Processing 2017...")
data_2017 <- process_year(aps_2017_raw, 2017)
cat(sprintf(" %d rows after age restriction\n", nrow(data_2017)))

cat("  Processing 2018...")
data_2018 <- process_year(aps_2018_raw, 2018)
cat(sprintf(" %d rows after age restriction\n", nrow(data_2018)))

cat("  Processing 2019...")
data_2019 <- process_2019(aps_2019_raw)
cat(sprintf(" %d rows after age restriction\n", nrow(data_2019)))

rm(aps_2017_raw, aps_2018_raw, aps_2019_raw)
gc()


################################################################################
# STEP 3: STACK DATA
################################################################################

cat("\n[3] Creating panel dataset...\n")
cat("================================================================================\n")

panel_aps <- bind_rows(data_2017, data_2018, data_2019)

cat(sprintf("  Panel created: %d individuals\n", nrow(panel_aps)))

year_dist <- table(panel_aps$year)
cat("\n  By year:\n")
for (y in c(2017, 2018, 2019)) {
  cat(sprintf("    %d: %s\n", y, format(year_dist[as.character(y)], big.mark=",")))
}


################################################################################
# STEP 4: COUNTRY PARTICIPATION
################################################################################

cat("\n[4] Country participation analysis...\n")
cat("================================================================================\n")

country_year <- panel_aps %>%
  group_by(country, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0)

country_participation <- country_year %>%
  mutate(
    years_present = (`2017` > 0) + (`2018` > 0) + (`2019` > 0)
  )

cat(sprintf("  Balanced panel (all 3 years): %d countries\n",
            sum(country_participation$years_present == 3)))
cat(sprintf("  2 years: %d countries\n",
            sum(country_participation$years_present == 2)))
cat(sprintf("  1 year: %d countries\n",
            sum(country_participation$years_present == 1)))

balanced_countries <- country_participation %>%
  filter(years_present == 3) %>%
  pull(country)


################################################################################
# STEP 5: MERGE COUNTRY-LEVEL DATA (NES)
################################################################################

cat("\n[5] Merging country-level data...\n")
cat("================================================================================\n")

# Load NES for each year
load_nes_year <- function(file_path, year_val, level_col = "level") {
  nes <- read_sav(file_path) %>%
    as.data.frame()
  
  # Filter to national level if level column exists
  if (level_col %in% names(nes)) {
    nes <- nes %>% filter(get(level_col) == 1)
  }
  
  # Find country variable (try year-specific patterns first, then generic)
  year_short <- year_val %% 100  # e.g., 2017 -> 17
  country_candidates <- c(
    sprintf("NES%dCOUNTRY", year_short),      # NES17COUNTRY, NES18COUNTRY, etc.
    sprintf("NES%sCOUNTRY", year_val),
    "country", "COUNTRY", "Country", "COUNTRY_1",  # 2019 uses COUNTRY_1
    "cou_code", "ID_CODE"
  )
  country_var <- intersect(names(nes), country_candidates)[1]
  
  if (is.na(country_var)) {
    cat("    Warning: No country variable found in", year_val, "\n")
    cat("      Tried:", paste(country_candidates, collapse = ", "), "\n")
    cat("      Available (first 20):", paste(head(names(nes), 20), collapse = ", "), "\n")
    return(NULL)
  }
  
  cat("    ", year_val, "- using country variable:", country_var, "\n")
  
  nes <- nes %>%
    mutate(
      year = year_val,
      country = as.numeric(get(country_var))
    ) %>%
    select(year, country, starts_with("nes_"), starts_with("neci"), starts_with("NES"))
  
  return(nes)
}

# Load each year
nes_2017 <- load_nes_year("data_raw/GEM 2017 NES ALL LEVELS.sav", 2017)
nes_2018 <- load_nes_year("data_raw/GEM 2018 NES ALL LEVELS_FINAL_03122018.sav", 2018)
nes_2019 <- load_nes_year("data_raw/GEM 2019 NES ALL LEVELS_NECI UPDATED_13012020.sav", 2019)

# Stack NES
nes_panel <- bind_rows(nes_2017, nes_2018, nes_2019)
cat(sprintf("  NES data: %d country-year observations\n", nrow(nes_panel)))

# Merge
panel_complete <- panel_aps %>%
  left_join(nes_panel, by = c("year", "country"))

cat(sprintf("  Merged: %d rows\n", nrow(panel_complete)))


################################################################################
# STEP 6: FINAL CLEANING
################################################################################

cat("\n[6] Final preparation...\n")
cat("================================================================================\n")

# Find available NECI variables and standardize if they exist
neci_vars <- grep("^neci", names(panel_complete), ignore.case = TRUE, value = TRUE)

if (length(neci_vars) > 0) {
  cat(sprintf("  Found %d NECI variables\n", length(neci_vars)))
  
  # Try to find neci1, neci2, neci3
  neci1_candidates <- grep("neci1", names(panel_complete), ignore.case = TRUE, value = TRUE)
  neci2_candidates <- grep("neci2", names(panel_complete), ignore.case = TRUE, value = TRUE)
  neci3_candidates <- grep("neci3", names(panel_complete), ignore.case = TRUE, value = TRUE)
  nes_inq_candidates <- grep("nes_inq", names(panel_complete), ignore.case = TRUE, value = TRUE)
  
  panel_final <- panel_complete %>%
    mutate(
      neci1_std = if(length(neci1_candidates) > 0) scale(get(neci1_candidates[1]))[,1] else NA_real_,
      neci2_std = if(length(neci2_candidates) > 0) scale(get(neci2_candidates[1]))[,1] else NA_real_,
      neci3_std = if(length(neci3_candidates) > 0) scale(get(neci3_candidates[1]))[,1] else NA_real_,
      nes_inq_std = if(length(nes_inq_candidates) > 0) scale(get(nes_inq_candidates[1]))[,1] else NA_real_
    )
} else {
  cat("  Warning: No NECI variables found in merged data\n")
  panel_final <- panel_complete %>%
    mutate(
      neci1_std = NA_real_,
      neci2_std = NA_real_,
      neci3_std = NA_real_,
      nes_inq_std = NA_real_
    )
}

# Summary stats
summary_stats <- panel_final %>%
  group_by(year) %>%
  summarise(
    n = n(),
    tea_rate = mean(tea_binary, na.rm = TRUE) * 100,
    mean_agency = mean(agency_index, na.rm = TRUE),
    pct_female = mean(female, na.rm = TRUE) * 100,
    mean_age = mean(age, na.rm = TRUE)
  )

cat("\n  Summary by year:\n")
print(summary_stats)


################################################################################
# STEP 7: SAVE DATASETS
################################################################################

cat("\n[7] Saving panel datasets...\n")
cat("================================================================================\n")

# Full panel
write_csv(panel_final, "data_processed/panel/panel_2017_2019_full.csv")
cat(sprintf("  ✓ Full panel: %s rows\n", format(nrow(panel_final), big.mark=",")))

# Balanced panel
panel_balanced <- panel_final %>%
  filter(country %in% balanced_countries)

write_csv(panel_balanced, "data_processed/panel/panel_2017_2019_balanced.csv")
cat(sprintf("  ✓ Balanced panel: %s rows (%d countries)\n",
            format(nrow(panel_balanced), big.mark=","), length(balanced_countries)))

# Save summaries
write_csv(summary_stats, "output/tables/table22_panel_summary.csv")
write_csv(country_participation, "output/tables/table22_country_participation.csv")


################################################################################
# SUMMARY
################################################################################

cat("\n")
cat("================================================================================\n")
cat("✓ PANEL DATA INTEGRATION COMPLETE (2017-2019)\n")
cat("================================================================================\n")
cat(sprintf("  Full panel: %s individuals\n", format(nrow(panel_final), big.mark=",")))
cat(sprintf("  Balanced panel: %s individuals in %d countries\n",
            format(nrow(panel_balanced), big.mark=","), length(balanced_countries)))
cat(sprintf("  Years: 2017, 2018, 2019\n"))
cat(sprintf("  Age range: 18-30\n"))
cat("\n")
cat("  READY FOR CAUSAL ANALYSIS:\n")
cat("    → Country fixed effects (Phase 24)\n")
cat("    → Lagged analysis (Phase 25)\n")
cat("    → Difference-in-differences if policy shock identified\n")
cat("\n")
cat("  Note: 2015-2016 data deferred due to variable naming inconsistencies\n")
cat("================================================================================\n")
