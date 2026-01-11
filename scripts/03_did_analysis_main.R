# ============================================================================
# Difference-in-Differences Analysis: Regulatory Reforms & Entrepreneurship
# ============================================================================
# Purpose: Implement DiD design with baseline APS data
# ============================================================================

library(tidyverse)
library(lfe)  # Fixed effects regression

wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

data_processed_path <- "data_processed"
output_path <- "output"

if (!dir.exists(output_path)) dir.create(output_path)
if (!dir.exists(file.path(output_path, "did_results"))) {
  dir.create(file.path(output_path, "did_results"))
}

cat("\n=== Baseline Analysis: Agency & TEA (without DiD) ===\n")
cat("(DiD will be implemented once reform indicators are populated)\n\n")

# Load prepared data
cat("Loading data...\n")
aps_combined <- readRDS(file.path(data_processed_path, "aps_combined_2005_2021.rds"))
aps_agg <- readRDS(file.path(data_processed_path, "aps_country_year_2005_2021.rds"))

cat(sprintf("Loaded: %d individual-level observations\n", nrow(aps_combined)))
cat(sprintf("        %d country-year aggregates\n", nrow(aps_agg)))

# ============================================================================
# Aggregate individual data with agency index (reconstruction)
# ============================================================================
cat("\n=== Reconstructing Agency Index ===\n")

# Since we don't have raw agency items, we'll use a proxy based on available variables
# In production, would use raw agency items (fear, confidence, skills, etc.)

aps_agg_enhanced <- aps_combined %>%
  # Create simple agency proxy: higher education + positive labor market status
  mutate(
    proxy_agency = (education >= 4) * 0.5 +  # Tertiary education
                   (female == 0) * 0.25 +     # Gender (male bias in entrepreneurship)
                   0.25  # Baseline
  ) %>%
  group_by(country_id, year) %>%
  summarise(
    n_obs = n(),
    tea_rate = mean(tea, na.rm = TRUE),
    agency_proxy = mean(proxy_agency, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    female_pct = mean(female == 1, na.rm = TRUE) * 100,
    education_mean = mean(education, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country_id, year) %>%
  mutate(
    country_fe = as.factor(country_id),
    year_fe = as.factor(year)
  )

cat(sprintf("Aggregated: %d country-year combinations\n", nrow(aps_agg_enhanced)))

# ============================================================================
# Descriptive Statistics
# ============================================================================
cat("\n=== Summary Statistics by Year ===\n\n")

desc_year <- aps_agg_enhanced %>%
  group_by(year) %>%
  summarise(
    n_countries = n_distinct(country_id),
    mean_tea = mean(tea_rate, na.rm = TRUE),
    sd_tea = sd(tea_rate, na.rm = TRUE),
    mean_agency = mean(agency_proxy, na.rm = TRUE),
    sd_agency = sd(agency_proxy, na.rm = TRUE),
    .groups = "drop"
  )

print(desc_year)

# ============================================================================
# Baseline Fixed Effects Regression: Agency on TEA
# ============================================================================
cat("\n=== Baseline FE Regression ===\n")
cat("Model: log(TEA_rate) = β₀ + β₁·Agency + α_c + γ_t + ε_ct\n\n")

aps_agg_enhanced <- aps_agg_enhanced %>%
  mutate(
    log_tea = log(pmax(tea_rate, 0.001)),
    log_agency = log(pmax(agency_proxy, 0.001))
  )

# Simple FE model with country and year effects
fe_basic <- felm(
  log_tea ~ log_agency | country_fe + year_fe,
  data = aps_agg_enhanced,
  weights = aps_agg_enhanced$n_obs  # Weight by sample size
)

cat("\nModel Summary:\n")
print(summary(fe_basic))

# Extract coefficient
fe_coef <- coef(fe_basic)[1]
fe_se <- summary(fe_basic)$coefficients[1, 2]
fe_t <- summary(fe_basic)$coefficients[1, 3]
fe_p <- summary(fe_basic)$coefficients[1, 4]

cat("\n=== KEY FINDING ===\n")
cat(sprintf("Agency Coefficient: %.4f\n", fe_coef))
cat(sprintf("Std. Error: %.4f\n", fe_se))
cat(sprintf("t-statistic: %.3f\n", fe_t))
cat(sprintf("p-value: %.4f\n", fe_p))

if (fe_p < 0.05) {
  cat("\n✓ SIGNIFICANT at 5% level\n")
  if (fe_coef > 0) {
    cat("✓ Positive effect: Higher agency is associated with HIGHER entrepreneurship\n")
  } else {
    cat("✗ Negative effect: Higher agency is associated with LOWER entrepreneurship\n")
  }
} else {
  cat("\n✗ NOT significant at 5% level\n")
}

# ============================================================================
# Alternative Specification: Linear Probability Model
# ============================================================================
cat("\n=== Linear Specification (Raw TEA Rate) ===\n")

fe_linear <- felm(
  tea_rate ~ agency_proxy | country_fe + year_fe,
  data = aps_agg_enhanced,
  weights = aps_agg_enhanced$n_obs
)

cat("\nModel Summary:\n")
print(summary(fe_linear))

fe_lin_coef <- coef(fe_linear)[1]
fe_lin_se <- summary(fe_linear)$coefficients[1, 2]

cat(sprintf("\nAgency Effect (percentage points): %.4f\n", fe_lin_coef))
cat("Interpretation: One-unit increase in agency index is associated with\n")
cat(sprintf("                %.4f percentage point increase in TEA rate\n\n", fe_lin_coef))

# ============================================================================
# Create Results Summary Table
# ============================================================================
cat("\n=== Summary: Baseline Results ===\n\n")

summary_table <- tibble(
  Model = c("Log-Log", "Linear"),
  Coefficient = c(fe_coef, fe_lin_coef),
  SE = c(fe_se, fe_lin_se),
  t_stat = c(fe_t, summary(fe_linear)$coefficients[1, 3]),
  p_value = c(fe_p, summary(fe_linear)$coefficients[1, 4]),
  Significant = c(
    ifelse(fe_p < 0.05, "Yes***", "No"),
    ifelse(summary(fe_linear)$coefficients[1, 4] < 0.05, "Yes***", "No")
  )
) %>%
  mutate(
    `95% CI` = sprintf("[%.4f, %.4f]", 
                       Coefficient - 1.96*SE, 
                       Coefficient + 1.96*SE)
  )

print(summary_table)

# Save results
write_csv(summary_table, file.path(output_path, "did_results", "baseline_results_summary.csv"))
cat(sprintf("\nResults saved to: %s\n", file.path(output_path, "did_results", "baseline_results_summary.csv")))

# ============================================================================
# Data Check: Correlation Matrix
# ============================================================================
cat("\n=== Correlations ===\n")

corr_vars <- aps_agg_enhanced %>%
  select(tea_rate, agency_proxy, age_mean, female_pct, education_mean) %>%
  cor(use = "complete.obs") %>%
  round(3)

print(corr_vars)

# Save data for next steps
saveRDS(aps_agg_enhanced, file.path(output_path, "did_results", "aps_agg_for_did.rds"))

cat("\n=== Baseline Analysis Complete ===\n")
cat("\nNext Steps:\n")
cat("1. Populate reform indicators in regulatory_reforms_2005_2021.rds\n")
cat("2. Implement full DiD analysis with reform shocks\n")
cat("3. Test parallel trends assumption\n")
cat("4. Estimate dynamic treatment effects (event study)\n")

