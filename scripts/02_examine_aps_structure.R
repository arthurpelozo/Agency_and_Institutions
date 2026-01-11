#!/usr/bin/env Rscript

# ============================================================================
# EXAMINE APS STRUCTURE - FIRST 100 VARIABLES
# ============================================================================
# Purpose: Identify control variables, agency items, and complete variable list
# Run in R Terminal: source("scripts/02_examine_aps_structure.R")
# ============================================================================

cat("\n================================================================================\n")
cat("EXAMINING APS STRUCTURE: FIRST 100 VARIABLES\n")
cat("================================================================================\n\n")

if (!require("haven", quietly = TRUE)) {
  install.packages("haven", quiet = TRUE, repos="https://cran.r-project.org")
  library(haven, quietly = TRUE)
}

aps_path <- "c:/Users/arthu/Downloads/Development Project/GEM 2019 APS Global Individual Level Data_30Jan2021.sav"

cat("[1] Loading APS dataset (first row only for speed)...\n")

# Create output directory
if (!dir.exists("output")) dir.create("output", showWarnings = FALSE)
output_file <- "output/aps_structure_output.txt"

tryCatch({
  aps <- read_sav(aps_path, n_max = 1)
  aps_vars <- names(aps)
  cat("✅ SUCCESS: Loaded APS with", length(aps_vars), "variables\n\n")
  
  # Redirect output to file
  sink(output_file, split = TRUE)
  
  cat("APS VARIABLES 1-100 (for identifying controls and agency items):\n")
  cat("================================================================================\n")
  for (i in 1:min(100, length(aps_vars))) {
    cat(sprintf("%3d. %-40s\n", i, aps_vars[i]))
  }
  
  cat("\nAPS VARIABLES 100-150 (if available):\n")
  cat("================================================================================\n")
  if (length(aps_vars) > 100) {
    for (i in 101:min(150, length(aps_vars))) {
      cat(sprintf("%3d. %-40s\n", i, aps_vars[i]))
    }
  }
  
  cat("\nAPS VARIABLES 200-250 (middle section):\n")
  cat("================================================================================\n")
  if (length(aps_vars) > 200) {
    for (i in 200:min(250, length(aps_vars))) {
      cat(sprintf("%3d. %-40s\n", i, aps_vars[i]))
    }
  }
  
  cat("\nAPS VARIABLES 300-350 (third section):\n")
  cat("================================================================================\n")
  if (length(aps_vars) > 300) {
    for (i in 300:min(350, length(aps_vars))) {
      cat(sprintf("%3d. %-40s\n", i, aps_vars[i]))
    }
  }
  
  cat("\nAPS VARIABLES 400-419 (END - Outcomes):\n")
  cat("================================================================================\n")
  if (length(aps_vars) > 400) {
    for (i in 400:length(aps_vars)) {
      cat(sprintf("%3d. %-40s\n", i, aps_vars[i]))
    }
  }
  
  cat("\n================================================================================\n")
  cat("COMPLETE APS VARIABLE LIST (ALL", length(aps_vars), "VARIABLES):\n")
  cat("================================================================================\n")
  for (i in 1:length(aps_vars)) {
    cat(sprintf("%3d. %s\n", i, aps_vars[i]))
  }
  
  cat("\n================================================================================\n")
  cat("SUMMARY:\n")
  cat(sprintf("Total APS variables: %d\n", length(aps_vars)))
  cat("Variables identified for analysis:\n")
  cat("  - Controls: age, gender, education, income, employment\n")
  cat("  - Agency items: skills, risk, motivation, knowledge\n")
  cat("  - Outcomes: teaactivityr, teaimpact4cat\n")
  cat("================================================================================\n\n")
  
  sink()
  cat("✅ Output saved to: output/aps_structure_output.txt\n")
  
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
})
