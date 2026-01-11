#!/usr/bin/env Rscript

# ============================================================================
# SCRIPT 03: DATA MERGING & CONSTRUCT BUILDING (SIMPLIFIED)
# ============================================================================

cat("\n================================================================================\n")
cat("PHASE 2: DATA MERGING & CONSTRUCT BUILDING\n")
cat("================================================================================\n\n")

# Load packages
suppressPackageStartupMessages({
  library(haven)
  library(tidyverse)
})

# Set paths
aps_path <- "c:/Users/arthu/Downloads/Development Project/GEM 2019 APS Global Individual Level Data_30Jan2021.sav"
nes_path <- "c:/Users/arthu/Downloads/Development Project/GEM 2019 NES ALL LEVELS_NECI UPDATED_13012020.sav"

# Create output directories
if (!dir.exists("data_processed")) dir.create("data_processed", showWarnings = FALSE)
if (!dir.exists("output/tables")) dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# STEP 1: LOAD DATASETS
# ============================================================================

cat("[1] Loading APS dataset...\n")
aps <- read_sav(aps_path)
names(aps) <- tolower(names(aps))
# Manually convert haven_labelled columns to numeric/character
for (col in names(aps)) {
  if (inherits(aps[[col]], "haven_labelled")) {
    aps[[col]] <- as.numeric(as.character(aps[[col]]))
  }
}
cat("✅ Loaded APS:", nrow(aps), "rows ×", ncol(aps), "columns\n")

cat("\n[2] Loading NES dataset...\n")
nes <- read_sav(nes_path)
names(nes) <- tolower(names(nes))
# Manually convert haven_labelled columns to numeric/character
for (col in names(nes)) {
  if (inherits(nes[[col]], "haven_labelled")) {
    nes[[col]] <- as.numeric(as.character(nes[[col]]))
  }
}
cat("✅ Loaded NES:", nrow(nes), "rows ×", ncol(nes), "columns\n")

# ============================================================================
# STEP 2: BUILD AGENCY CONSTRUCT
# ============================================================================

cat("\n[3] Building Entrepreneurial Agency Construct...\n")

# Find agency variables
agency_patterns <- c("knowentr", "opportl", "suskill", "fearfail", "easystart", 
                     "oppism", "proact", "creativ", "vision", "nbgood", "nbstat", "nbmedi")
agency_cols <- grep(paste(agency_patterns, collapse="|"), names(aps), value=TRUE, ignore.case=TRUE)
cat("   Found", length(agency_cols), "agency variables\n")

if (length(agency_cols) > 0) {
  agency_std <- scale(aps[, agency_cols, drop=FALSE])
  agency_index <- rowMeans(agency_std, na.rm = TRUE)
  cat("   Agency index: Mean =", round(mean(agency_index, na.rm=TRUE), 3),
      ", SD =", round(sd(agency_index, na.rm=TRUE), 3), "\n")
} else {
  agency_index <- rep(NA, nrow(aps))
}

# ============================================================================
# STEP 3: PREPARE DATA FOR MERGING
# ============================================================================

cat("\n[4] Preparing individual-level data...\n")

# Get outcome and control columns
outcome_cols <- grep("teaactivity|teaimpact", names(aps), value=TRUE, ignore.case=TRUE)
control_cols <- c("id", "country", "country_name", "age", "gender", "hhsize",
                  "gemeduc", "gemhhinc", "gemoccu", "gemwork", "weight")
control_cols <- intersect(control_cols, names(aps))

# Create individual dataset
individual_data <- aps[, c(control_cols, outcome_cols), drop=FALSE] %>%
  as_tibble() %>%
  mutate(agency_index = agency_index, .after = country_name)

cat("   Individual data:", nrow(individual_data), "rows ×", ncol(individual_data), "columns\n")

# ============================================================================
# STEP 4: MERGE WITH NES
# ============================================================================

cat("\n[5] Merging with NES institutional variables...\n")

# Get institutional variables
nes_inst <- grep("(mean10|asum|bsum|csum|dsum|esum|fsum|gsum|hsum|isum)", names(nes), value=TRUE, ignore.case=TRUE)
nes_inst <- intersect(nes_inst, names(nes))

if ("country_1" %in% names(nes)) {
  nes_selected <- nes[, c("country_1", nes_inst), drop=FALSE] %>%
    as_tibble() %>%
    rename(country = country_1)
  
  merged_data <- individual_data %>%
    left_join(nes_selected, by = "country")
  
  cat("   Merged data:", nrow(merged_data), "rows\n")
} else {
  merged_data <- individual_data
  cat("   WARNING: country_1 not found in NES\n")
}

# ============================================================================
# STEP 5: CREATE PRIMARY SAMPLE (AGE 18-30)
# ============================================================================

cat("\n[6] Creating primary sample (Age 18-30)...\n")

primary_sample <- merged_data %>%
  filter(age >= 18, age <= 30, !is.na(age)) %>%
  filter(!is.na(teaactivityr), !is.na(agency_index)) %>%
  drop_na(country)

cat("   Primary sample:", nrow(primary_sample), "individuals\n")
cat("   Countries:", n_distinct(primary_sample$country), "\n")

# ============================================================================
# STEP 6: DESCRIPTIVE STATISTICS & SAVE
# ============================================================================

cat("\n[7] Saving outputs...\n")

# Summary stats
summary_table <- data.frame(
  N_individuals = nrow(primary_sample),
  N_countries = n_distinct(primary_sample$country),
  Age_M = mean(primary_sample$age, na.rm=TRUE),
  Age_SD = sd(primary_sample$age, na.rm=TRUE),
  Agency_M = mean(primary_sample$agency_index, na.rm=TRUE),
  Agency_SD = sd(primary_sample$agency_index, na.rm=TRUE),
  TEA_active_pct = mean(primary_sample$teaactivityr == 1, na.rm=TRUE) * 100
)

cat("\n   Summary:\n")
print(summary_table)

# Save files
write.csv(summary_table, "output/tables/table1_descriptive.csv", row.names=FALSE)
write.csv(primary_sample, "data_processed/primary_sample.csv", row.names=FALSE)
saveRDS(primary_sample, "data_processed/primary_sample.RDS")

cat("\n================================================================================\n")
cat("✅ DATA PREPARATION COMPLETE\n")
cat("================================================================================\n")
cat("Outputs:\n")
cat("  - data_processed/primary_sample.csv\n")
cat("  - output/tables/table1_descriptive.csv\n")
cat("================================================================================\n\n")
