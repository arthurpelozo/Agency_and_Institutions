################################################################################
# PROJECT: Institutional Quality and Entrepreneurial Agency Conversion
# SCRIPT: 01_data_audit.R (SIMPLIFIED)
# PURPOSE: Load and describe all three GEM datasets
# AUTHOR: Econometric Research Team
# DATE: January 2025
################################################################################

# Clear environment
rm(list = ls())
gc()

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("GEM 2019 DATA AUDIT - LOADING AND EXPLORING DATASETS\n")
cat(strrep("=", 80) %+% "\n\n")

# Install and load packages as needed
load_or_install <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, quiet = TRUE, dependencies = TRUE)
    require(pkg, character.only = TRUE, quietly = TRUE)
  }
}

# Load essential packages
load_or_install("haven")
load_or_install("readxl")

# Set project root
project_root <- "c:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
data_processed_path <- file.path(project_root, "data_processed")

# Set seed for reproducibility
set.seed(2025)

# ============================================================================
# 1. LOAD APS DATASET (Individual-level)
# ============================================================================

cat("\n" %+% strrep("-", 80) %+% "\n")
cat("LOADING GEM APS 2019 DATASET (Individual-level)\n")
cat(strrep("-", 80) %+% "\n\n")

aps_path <- "c:/Users/arthu/Downloads/Development Project/GEM 2019 APS Global Individual Level Data_30Jan2021.sav"

tryCatch({
  aps_raw <- haven::read_sav(aps_path)
  
  cat("✅ APS loaded successfully\n")
  cat(sprintf("   Dimensions: %d rows × %d columns\n", nrow(aps_raw), ncol(aps_raw)))
  cat(sprintf("   Memory: %.2f MB\n\n", object.size(aps_raw) / (1024^2)))
  
  # Lowercase variable names
  names(aps_raw) <- tolower(names(aps_raw))
  
  # Store for later
  aps_raw <<- aps_raw
  
}, error = function(e) {
  cat(sprintf("❌ Error loading APS: %s\n", e$message))
  aps_raw <<- NULL
})

# ============================================================================
# 2. LOAD NES DATASET (Country/Expert-level)
# ============================================================================

cat("\n--- LOADING GEM NES 2019 DATASET ---\n")

nes_raw <- read_sav(
  "c:/Users/arthu/Downloads/Development Project/GEM 2019 NES ALL LEVELS_NECI UPDATED_13012020.sav"
)

cat("NES Dimensions:", nrow(nes_raw), "observations,", ncol(nes_raw), "variables\n")

# Convert to tibble and lowercase variable names
nes_raw <- nes_raw %>%
  as_tibble() %>%
  janitor::clean_names()

cat("Variable names standardized to lowercase.\n")
cat("Sample of variable names:", paste(names(nes_raw)[1:10], collapse = ", "), "\n\n")

# ============================================================================
# 3. LOAD NECI DATASET (Composite Index)
# ============================================================================

cat("\n--- LOADING GEM NECI 2019 DATASET ---\n")

neci_raw <- read_excel(
  "c:/Users/arthu/Downloads/Development Project/NECI 2019 FINAL VERSION_16012020.xlsx"
)

cat("NECI Dimensions:", nrow(neci_raw), "country-year observations,", ncol(neci_raw), "variables\n")

# Convert to tibble and lowercase variable names
neci_raw <- neci_raw %>%
  as_tibble() %>%
  janitor::clean_names()

cat("Variable names standardized to lowercase.\n")
cat("Sample of variable names:", paste(names(neci_raw)[1:10], collapse = ", "), "\n\n")

# ============================================================================
# 4. DATA AUDIT — APS
# ============================================================================

cat("\n========== DATA AUDIT: APS ==========\n\n")

# Structure and summary
cat("STRUCTURE:\n")
print(str(aps_raw, max.level = 1))

cat("\nMISSINGNESS REPORT:\n")
missing_aps <- naniar::miss_var_summary(aps_raw)
print(missing_aps)

# Export missingness map
png(file.path(output_path, "figures", "aps_missingness_map.png"), width = 1200, height = 600)
naniar::vis_miss(aps_raw, warn_large_data = FALSE, show_perc = TRUE)
dev.off()

cat("\nMissingness map saved to output/figures/aps_missingness_map.png\n")

# ============================================================================
# 5. DATA AUDIT — NES
# ============================================================================

cat("\n========== DATA AUDIT: NES ==========\n\n")

# Structure and summary
cat("STRUCTURE:\n")
print(str(nes_raw, max.level = 1))

cat("\nMISSINGNESS REPORT:\n")
missing_nes <- naniar::miss_var_summary(nes_raw)
print(missing_nes)

# Export missingness map
png(file.path(output_path, "figures", "nes_missingness_map.png"), width = 1200, height = 600)
naniar::vis_miss(nes_raw, warn_large_data = FALSE, show_perc = TRUE)
dev.off()

cat("\nMissingness map saved to output/figures/nes_missingness_map.png\n")

# ============================================================================
# 6. DATA AUDIT — NECI
# ============================================================================

cat("\n========== DATA AUDIT: NECI ==========\n\n")

# Structure and summary
cat("STRUCTURE:\n")
print(str(neci_raw, max.level = 1))

cat("\nMISSINGNESS REPORT:\n")
missing_neci <- naniar::miss_var_summary(neci_raw)
print(missing_neci)

# Preview data
cat("\nFirst 10 rows of NECI:\n")
print(head(neci_raw, 10))

# ============================================================================
# 7. EXPLORATORY VARIABLE IDENTIFICATION
# ============================================================================

cat("\n========== VARIABLE IDENTIFICATION FOR CONSTRUCT BUILDING ==========\n\n")

# APS: Look for agency, motivation, and outcome variables
cat("APS VARIABLES RELATED TO ENTREPRENEURIAL AGENCY:\n")
cat("(Searching for: suskilll, suskill, motiv, oppor, neces, fear, fail, intend, age, educ, gender, employ)\n\n")

agency_keywords <- c("suskilll", "suskill", "motiv", "oppor", "neces", "fear", "fail", "intend")
aps_agency_vars <- names(aps_raw)[grep(paste(agency_keywords, collapse = "|"), names(aps_raw), ignore.case = TRUE)]
cat("Matched agency-related variables:", paste(aps_agency_vars, collapse = ", "), "\n\n")

cat("APS VARIABLES RELATED TO OUTCOMES:\n")
cat("(Searching for: esea, teajob, grow, innov, social, impact, empl, employee)\n\n")

outcome_keywords <- c("esea", "teajob", "grow", "innov", "social", "impact", "empl", "employee")
aps_outcome_vars <- names(aps_raw)[grep(paste(outcome_keywords, collapse = "|"), names(aps_raw), ignore.case = TRUE)]
cat("Matched outcome-related variables:", paste(aps_outcome_vars, collapse = ", "), "\n\n")

cat("APS DEMOGRAPHIC VARIABLES:\n")
cat("(Searching for: age, educ, gender, employ, income, education, sex)\n\n")

demo_keywords <- c("age", "educ", "gender", "employ", "income", "sex")
aps_demo_vars <- names(aps_raw)[grep(paste(demo_keywords, collapse = "|"), names(aps_raw), ignore.case = TRUE)]
cat("Matched demographic variables:", paste(aps_demo_vars, collapse = ", "), "\n\n")

# NES: Look for institutional variables
cat("NES VARIABLES RELATED TO INSTITUTIONS:\n")
cat("(Searching for: educat, finance, norms, support, govern, policy, regul, legal)\n\n")

inst_keywords <- c("educat", "finance", "norms", "support", "govern", "policy", "regul", "legal")
nes_inst_vars <- names(nes_raw)[grep(paste(inst_keywords, collapse = "|"), names(nes_raw), ignore.case = TRUE)]
cat("Matched institutional variables:", paste(nes_inst_vars, collapse = ", "), "\n\n")

# NECI: Check all variables
cat("NECI AVAILABLE VARIABLES:\n")
print(names(neci_raw))

# ============================================================================
# 8. SAVE AUDIT OUTPUTS
# ============================================================================

# Create comprehensive audit report
audit_report <- data.frame(
  dataset = c("APS", "NES", "NECI"),
  n_obs = c(nrow(aps_raw), nrow(nes_raw), nrow(neci_raw)),
  n_vars = c(ncol(aps_raw), ncol(nes_raw), ncol(neci_raw)),
  level = c("Individual", "Country/Expert", "Country (Composite)")
)

cat("\n========== SUMMARY ==========\n")
print(audit_report)

# Save raw datasets to processed folder for next script
saveRDS(aps_raw, file.path(data_processed_path, "aps_raw.rds"))
saveRDS(nes_raw, file.path(data_processed_path, "nes_raw.rds"))
saveRDS(neci_raw, file.path(data_processed_path, "neci_raw.rds"))

cat("\n✓ Raw datasets saved to data_processed/\n")
cat("\nNEXT STEP: Run 02_construct_building.R to build latent indices.\n")

################################################################################
# END OF SCRIPT
################################################################################
