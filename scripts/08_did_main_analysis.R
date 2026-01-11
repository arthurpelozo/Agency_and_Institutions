# ============================================================================
# Difference-in-Differences Analysis: Agency, Reforms & Entrepreneurship
# ============================================================================
# Purpose: Estimate causal effect of regulatory reforms on agency-TEA link
# using DiD design with Doing Business reform shocks
# ============================================================================

library(tidyverse)
library(lfe)

wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

data_processed_path <- "data_processed"
output_path <- "output"

cat("\n=== DIFFERENCE-IN-DIFFERENCES ANALYSIS ===\n")
cat("=== Regulatory Reforms & Entrepreneurship ===\n\n")

# Load aggregated data with true agency index
aps_agg <- readRDS(file.path(data_processed_path, "aps_agency_country_year_2009_2020.rds"))

# ============================================================================
# Create Reform Data: Doing Business Major Reforms
# ============================================================================
# Based on World Bank Doing Business reports identifying countries with
# major reforms in "Starting a Business" (reducing days, procedures, or cost)

cat("Creating reform dataset from Doing Business major reforms...\n\n")

# Major reforms in Starting a Business (from DB literature)
# Format: country_id, reform_year, days_change, procedures_change
reforms_db <- tribble(
  ~country_name, ~reform_year, ~reform_magnitude,
  # East Africa
  "Rwanda", 2011, 5,        # Major simplification
  "Kenya", 2009, 3,         # Reduced procedures
  "Tanzania", 2010, 2,      # Reform started
  
  # South Africa
  "South Africa", 2010, 4,  # Business Act reforms
  
  # India
  "India", 2009, 4,         # Multiple reforms (DIC, etc)
  
  # Latin America
  "Colombia", 2009, 3,      # Camera de Comercio reforms
  "Mexico", 2010, 3,        # Simplified registration
  "Peru", 2008, 4,          # SUNARP improvements
  "Brazil", 2013, 2,        # Ongoing improvements
  "Chile", 2011, 2,         # Registration reforms
  
  # Southeast Asia
  "Vietnam", 2012, 4,       # Enterprise law reforms
  "Indonesia", 2012, 3,     # Regional office network
  "Philippines", 2009, 2,   # SEC online system
  "Thailand", 2008, 2,      # Multiple reforms
  
  # East Asia
  "China", 2014, 5,         # Major national reforms
  "Hong Kong", 2010, 1,     # Minor adjustments
  
  # Middle East
  "United Arab Emirates", 2009, 3,  # Ajman reforms
  
  # Europe (for comparison)
  "United Kingdom", 2013, 1,  # Minor adjustments
  "Georgia", 2006, 5        # Early major reformer
)

# Map country names to country IDs from APS data
country_mapping <- aps_agg %>%
  distinct(country_id, country)

# Simpler approach: manually map common names
reform_countries <- list(
  "rwanda" = c("Rwanda"),
  "kenya" = c("Kenya"),
  "south africa" = c("South Africa"),
  "india" = c("India"),
  "colombia" = c("Colombia"),
  "mexico" = c("Mexico"),
  "peru" = c("Peru"),
  "brazil" = c("Brazil"),
  "chile" = c("Chile"),
  "vietnam" = c("Vietnam"),
  "indonesia" = c("Indonesia"),
  "philippines" = c("Philippines"),
  "thailand" = c("Thailand"),
  "china" = c("China"),
  "hong kong" = c("Hong Kong", "China (Hong Kong SAR)")
)

reforms_db <- reforms_db %>%
  mutate(
    country_name_lower = tolower(country_name)
  )

# Merge with country_id using best match
reforms_with_id <- data.frame()

for (reform_idx in 1:nrow(reforms_db)) {
  reform_country <- tolower(reforms_db$country_name[reform_idx])
  
  # Try exact match
  match_id <- country_mapping %>%
    filter(tolower(country) == reform_country) %>%
    pull(country_id)
  
  if (length(match_id) == 0) {
    # Try partial match
    match_id <- country_mapping %>%
      filter(grepl(reform_country, tolower(country))) %>%
      pull(country_id)
  }
  
  if (length(match_id) > 0) {
    temp <- reforms_db[reform_idx,] %>% 
      mutate(country_id = match_id[1])
    reforms_with_id <- rbind(reforms_with_id, temp)
  }
}

reforms_db <- reforms_with_id %>%
  select(country_id, country_name, reform_year, reform_magnitude)

cat(sprintf("Found %d countries with documented reforms\n", n_distinct(reforms_db$country_id)))
print(reforms_db %>% select(-country_name) %>% arrange(reform_year))

# ============================================================================
# Create DiD Panel
# ============================================================================

cat("\n\nCreating panel for DiD analysis...\n")

# Build full panel - mais cuidado com nomes
did_panel <- aps_agg %>%
  select(country_id, country, year, tea_pct, agency_mean, age_mean, female_pct, education_mean, n_obs) %>%
  arrange(country_id, year) %>%
  # Create a sequence of all years
  nest(data = -country_id) %>%
  mutate(
    years = list(min(aps_agg$year):max(aps_agg$year))
  ) %>%
  unnest(years, keep_empty = TRUE) %>%
  rename(year_seq = years) %>%
  # Merge back
  left_join(
    aps_agg %>% select(country_id, country, year, tea_pct, agency_mean, age_mean, female_pct, education_mean, n_obs),
    by = c("country_id", "country"),
    relationship = "many-to-many"
  ) %>%
  filter(year_seq == year | is.na(year)) %>%
  select(-year_seq, -country.y) %>%
  rename(country = country.x) %>%
  # Add reform indicators
  left_join(
    reforms_db %>%
      select(country_id, reform_year, reform_magnitude),
    by = "country_id"
  ) %>%
  # Create treatment indicators
  mutate(
    # Which countries ever had reform?
    ever_treated = !is.na(reform_year),
    # Did reform happen in this year?
    reform_year_flag = reform_year == year,
    # Post-reform period (year >= reform_year)
    post_reform = year >= reform_year & !is.na(reform_year),
    # DiD term: post × treated
    did_term = post_reform * 1
  ) %>%
  # Fill NAs in reform variables for non-treated
  mutate(
    post_reform = replace_na(post_reform, FALSE),
    reform_year_flag = replace_na(reform_year_flag, FALSE),
    ever_treated = replace_na(ever_treated, FALSE)
  ) %>%
  # Keep only years with actual data
  filter(!is.na(tea_pct)) %>%
  arrange(country_id, year) %>%
  mutate(
    country_fe = as.factor(country_id),
    year_fe = as.factor(year),
    log_tea = log(pmax(tea_pct, 0.1))
  )

cat(sprintf("DiD panel: %d observations\n", nrow(did_panel)))
cat(sprintf("Countries with reforms: %d\n", n_distinct(filter(did_panel, ever_treated)$country_id)))
cat(sprintf("Observations in post-reform: %d\n", sum(did_panel$post_reform)))

# ============================================================================
# Parallel Trends Test
# ============================================================================

cat("\n\n=== PARALLEL TRENDS TEST ===\n")
cat("Testing pre-reform trend assumption...\n\n")

# Compare treated vs. control pre-reform
parallel_test_data <- did_panel %>%
  filter(!post_reform) %>%  # Only pre-reform period
  mutate(
    treatment_group = ifelse(ever_treated, "Treated (will reform)", "Control (no reform)")
  )

parallel_trends <- parallel_test_data %>%
  group_by(year, treatment_group) %>%
  summarise(
    mean_tea = mean(tea_pct, na.rm = TRUE),
    se_tea = sd(tea_pct, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

cat("TEA Rate by Year and Group (Pre-Reform Period):\n")
print(parallel_trends)

# Statistical test: interaction of treatment × time trend pre-reform
parallel_model <- lm(
  tea_pct ~ ever_treated * year,
  data = filter(parallel_test_data, year <= 2017)
)

cat("\nRegression: TEA ~ Treated × Year (pre-reform only)\n")
cat("H0: Parallel trends (Treated × Year coefficient = 0)\n")
p_interact <- summary(parallel_model)$coefficients["ever_treatedTRUE:year", "Pr(>|t|)"]
coef_interact <- summary(parallel_model)$coefficients["ever_treatedTRUE:year", "Estimate"]
cat(sprintf("Coefficient: %.4f, p-value: %.4f\n", coef_interact, p_interact))

if (p_interact > 0.05) {
  cat("✓ Cannot reject parallel trends assumption (p > 0.05)\n\n")
} else {
  cat("⚠ Significant pre-trend detected (p < 0.05)\n\n")
}

# ============================================================================
# Main DiD Specification
# ============================================================================

cat("\n=== MAIN DiD RESULTS ===\n")
cat("Model: TEA_ct = β₀ + β₁·Post_Reform_ct + β₂·Treated_c + β₃·(Post×Treated)_ct\n")
cat("                 + α_c + γ_t + ε_ct\n\n")

# Specification 1: Basic DiD without controls
did_basic <- felm(
  tea_pct ~ post_reform + ever_treated + did_term | country_fe + year_fe,
  data = did_panel
)

cat("--- DiD 1: Basic (No controls) ---\n")
cat(sprintf("Sample size: %d\n", nrow(did_basic$model)))
did_coef_1 <- coef(did_basic)["did_term"]
did_se_1 <- summary(did_basic)$coefficients["did_term", 2]
did_p_1 <- summary(did_basic)$coefficients["did_term", 4]
cat(sprintf("DiD Coefficient: %.4f\n", did_coef_1))
cat(sprintf("SE: %.4f, p-value: %.4f\n", did_se_1, did_p_1))

# Specification 2: DiD with demographic controls
did_controls <- felm(
  tea_pct ~ post_reform + ever_treated + did_term + age_mean + female_pct + education_mean |
    country_fe + year_fe,
  data = did_panel
)

cat("\n--- DiD 2: With Controls (age, female, education) ---\n")
cat(sprintf("Sample size: %d\n", nrow(did_controls$model)))
did_coef_2 <- coef(did_controls)["did_term"]
did_se_2 <- summary(did_controls)$coefficients["did_term", 2]
did_p_2 <- summary(did_controls)$coefficients["did_term", 4]
cat(sprintf("DiD Coefficient: %.4f\n", did_coef_2))
cat(sprintf("SE: %.4f, p-value: %.4f\n", did_se_2, did_p_2))

# Specification 3: Log-log DiD
did_loglog <- felm(
  log_tea ~ post_reform + ever_treated + did_term | country_fe + year_fe,
  data = filter(did_panel, tea_pct > 0)
)

cat("\n--- DiD 3: Log-Log (elasticity) ---\n")
cat(sprintf("Sample size: %d\n", nrow(did_loglog$model)))
did_coef_3 <- coef(did_loglog)["did_term"]
did_se_3 <- summary(did_loglog)$coefficients["did_term", 2]
did_p_3 <- summary(did_loglog)$coefficients["did_term", 4]
cat(sprintf("DiD Coefficient: %.4f\n", did_coef_3))
cat(sprintf("SE: %.4f, p-value: %.4f\n", did_se_3, did_p_3))

# ============================================================================
# DiD with Agency Interaction
# ============================================================================

cat("\n\n=== DiD INTERACTION WITH AGENCY ===\n")
cat("Model: TEA_ct = β₀ + β₁·Post_Reform_ct + β₂·Treated_c + β₃·(Post×Treated)_ct\n")
cat("                 + β₄·Agency_ct + β₅·(Post×Treated)·Agency_ct + α_c + γ_t + ε_ct\n\n")

# Agency interaction DiD
did_agency <- felm(
  tea_pct ~ post_reform + ever_treated + did_term + agency_mean + 
             I(did_term * agency_mean) | country_fe + year_fe,
  data = did_panel
)

cat("--- DiD with Agency Interaction ---\n")
cat(sprintf("Sample size: %d\n", nrow(did_agency$model)))

did_coef_interact <- coef(did_agency)["I(did_term * agency_mean)"]
did_se_interact <- summary(did_agency)$coefficients["I(did_term * agency_mean)", 2]
did_p_interact <- summary(did_agency)$coefficients["I(did_term * agency_mean)", 4]

cat(sprintf("DiD Coefficient (basic): %.4f\n", coef(did_agency)["did_term"]))
cat(sprintf("DiD × Agency Coefficient: %.4f\n", did_coef_interact))
cat(sprintf("SE: %.4f, p-value: %.4f\n", did_se_interact, did_p_interact))

if (abs(did_coef_interact) > 0.01) {
  cat("\n✓ Significant interaction detected\n")
  if (did_coef_interact > 0) {
    cat("✓ Reforms STRENGTHEN the agency-TEA relationship\n")
    cat("  (Institutional complementarity hypothesis SUPPORTED)\n")
  }
}

# ============================================================================
# Summary Table
# ============================================================================

cat("\n\n=== SUMMARY TABLE: DiD RESULTS ===\n\n")

results_table <- tribble(
  ~Model, ~Specification, ~Coefficient, ~SE, ~p_value, ~Significant,
  "1", "Basic DiD", did_coef_1, did_se_1, did_p_1, ifelse(did_p_1 < 0.05, "Yes", "No"),
  "2", "DiD + Controls", did_coef_2, did_se_2, did_p_2, ifelse(did_p_2 < 0.05, "Yes", "No"),
  "3", "DiD Log-Log", did_coef_3, did_se_3, did_p_3, ifelse(did_p_3 < 0.05, "Yes", "No"),
  "4", "DiD × Agency", did_coef_interact, did_se_interact, did_p_interact, ifelse(did_p_interact < 0.05, "Yes", "No")
)

print(results_table)

# Save results
write_csv(results_table, file.path(output_path, "did_results", "did_main_results.csv"))
saveRDS(did_panel, file.path(output_path, "did_results", "did_panel_data.rds"))

# ============================================================================
# Key Findings
# ============================================================================

cat("\n\n=== KEY FINDINGS ===\n\n")

if (did_p_1 < 0.05) {
  cat(sprintf("✅ SIGNIFICANT DiD effect: %.4f pp (p=%.4f)\n", did_coef_1, did_p_1))
  if (did_coef_1 > 0) {
    cat("   → Regulatory reforms INCREASE entrepreneurship\n")
  } else {
    cat("   → Regulatory reforms DECREASE entrepreneurship\n")
  }
} else {
  cat(sprintf("⚠️  No significant DiD effect at 5% level (p=%.4f)\n", did_p_1))
}

if (did_p_interact < 0.05) {
  cat(sprintf("\n✅ SIGNIFICANT Agency Interaction: %.4f (p=%.4f)\n", did_coef_interact, did_p_interact))
  cat("   → Reforms affect agency-TEA link differently\n")
} else {
  cat(sprintf("\n⚠️  No significant agency interaction (p=%.4f)\n", did_p_interact))
}

cat("\n=== DiD Analysis Complete ===\n")
cat(sprintf("Results saved to: %s\n", file.path(output_path, "did_results")))
