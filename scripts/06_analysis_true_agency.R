# ============================================================================
# Reanalysis with True Agency Index
# ============================================================================
# Purpose: Estimate agency → TEA relationship using reconstructed agency index
# ============================================================================

library(tidyverse)
library(lfe)

wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

data_processed_path <- "data_processed"
output_path <- "output"

if (!dir.exists(file.path(output_path, "did_results"))) {
  dir.create(file.path(output_path, "did_results"), showWarnings = FALSE)
}

cat("\n=== Analysis with True Agency Index (Fear, Skills, Know, Opportunity) ===\n\n")

# Load data with true agency index
aps_agg <- readRDS(file.path(data_processed_path, "aps_agency_country_year_2009_2020.rds"))

cat(sprintf("Data loaded: %d country-year observations\n", nrow(aps_agg)))
cat(sprintf("Countries: %d\n", n_distinct(aps_agg$country_id)))
cat(sprintf("Years: %s\n\n", paste(sort(unique(aps_agg$year)), collapse = ", ")))

# ============================================================================
# Descriptive Statistics
# ============================================================================
cat("=== Summary Statistics by Year ===\n\n")

desc_by_year <- aps_agg %>%
  group_by(year) %>%
  summarise(
    n_countries = n_distinct(country_id),
    n_obs = sum(n_obs),
    mean_agency = mean(agency_mean, na.rm = TRUE),
    sd_agency = sd(agency_mean, na.rm = TRUE),
    mean_tea_pct = mean(tea_pct, na.rm = TRUE),
    sd_tea_pct = sd(tea_pct, na.rm = TRUE),
    corr_agency_tea = cor(agency_mean, tea_pct, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(
    mean_tea_pct = round(mean_tea_pct, 2),
    corr_agency_tea = round(corr_agency_tea, 3)
  )

print(desc_by_year)

# ============================================================================
# Agency Components: Year-to-Year Trends
# ============================================================================
cat("\n=== Agency Components Trends ===\n\n")

components <- aps_agg %>%
  group_by(year) %>%
  summarise(
    fear_free = 100 - mean(fear_pct, na.rm = TRUE),  # % not afraid
    skill_pct = mean(skill_pct, na.rm = TRUE),
    know_pct = mean(know_pct, na.rm = TRUE),
    opport_pct = mean(opport_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 1)))

print(components)

cat("\nNote: Clear drop in 2019-2020 across all components (especially fear free, skills, know)\n")
cat("      This suggests measurement change or survey methodology shift.\n")

# ============================================================================
# Main Analysis: Agency → TEA with Fixed Effects
# ============================================================================
cat("\n=== Main FE Analysis: Agency → TEA ===\n")
cat("Model: TEA_ct = β₀ + β₁·Agency_ct + α_c + γ_t + ε_ct\n\n")

# Prepare data for FE regression
aps_agg <- aps_agg %>%
  mutate(
    country_fe = as.factor(country_id),
    year_fe = as.factor(year),
    log_tea_pct = log(pmax(tea_pct, 0.1))  # log scale
  )

# Model 1: Linear (raw percentages)
m1 <- felm(
  tea_pct ~ agency_mean | country_fe + year_fe,
  data = aps_agg
)

# Model 2: Log-log
m2 <- felm(
  log_tea_pct ~ log(agency_mean) | country_fe + year_fe,
  data = aps_agg
)

# Model 3: Split analysis - 2009-2018 vs 2019-2020 (separate codebook?)
# 2009-2017: All components measured similarly
m3 <- felm(
  tea_pct ~ agency_mean | country_fe + year_fe,
  data = filter(aps_agg, year <= 2017)
)

# Model 4: 2019-2020 only
m4 <- felm(
  tea_pct ~ agency_mean | country_fe + year_fe,
  data = filter(aps_agg, year >= 2019)
)

cat("\n--- Model 1: Linear (TEA % ~ Agency) ---\n")
cat(sprintf("Sample size: %d\n", nrow(m1$model))

)
cat(sprintf("Agency Coefficient: %.4f\n", coef(m1)[1]))
cat(sprintf("SE: %.4f\n", summary(m1)$coefficients[1, 2]))
cat(sprintf("p-value: %.4f\n", summary(m1)$coefficients[1, 4]))

cat("\n--- Model 2: Log-Log ---\n")
cat(sprintf("Sample size: %d\n", nrow(m2$model)))
cat(sprintf("Agency Coefficient: %.4f\n", coef(m2)[1]))
cat(sprintf("SE: %.4f\n", summary(m2)$coefficients[1, 2]))
cat(sprintf("p-value: %.4f\n\n", summary(m2)$coefficients[1, 4]))

cat("\n--- Model 3: 2009-2017 Only (consistent measurement) ---\n")
cat(sprintf("Sample size: %d\n", nrow(m3$model)))
cat(sprintf("Agency Coefficient: %.4f\n", coef(m3)[1]))
cat(sprintf("SE: %.4f\n", summary(m3)$coefficients[1, 2]))
cat(sprintf("p-value: %.4f\n", summary(m3)$coefficients[1, 4]))

cat("\n--- Model 4: 2019-2020 Only (new measurement) ---\n")
cat(sprintf("Sample size: %d\n", nrow(m4$model)))
cat(sprintf("Agency Coefficient: %.4f\n", coef(m4)[1]))
cat(sprintf("SE: %.4f\n", summary(m4)$coefficients[1, 2]))
cat(sprintf("p-value: %.4f\n", summary(m4)$coefficients[1, 4]))

# ============================================================================
# Agency Component Analysis
# ============================================================================
cat("\n=== Agency Components: Individual Effects ===\n\n")

# Model 5: Decompose agency into components
m5 <- felm(
  tea_pct ~ fear_pct + skill_pct + know_pct + opport_pct | country_fe + year_fe,
  data = aps_agg
)

cat("Full component model (with all 4 components):\n")
print(summary(m5))

# ============================================================================
# Create Summary Table
# ============================================================================
cat("\n=== Summary: Main Results ===\n\n")

results_table <- tibble(
  Model = c(
    "1. Linear (All years)",
    "2. Log-Log (All years)",
    "3. Linear (2009-2017)",
    "4. Linear (2019-2020)",
    "5. Components (All years)"
  ),
  Specification = c(
    "TEA_ct ~ Agency + FE",
    "log(TEA) ~ log(Agency) + FE",
    "TEA_ct ~ Agency + FE",
    "TEA_ct ~ Agency + FE",
    "TEA_ct ~ Fear+Skill+Know+Opport+FE"
  ),
  Sample = c(
    sprintf("N=%d", nrow(m1$model)),
    sprintf("N=%d", nrow(m2$model)),
    sprintf("N=%d", nrow(m3$model)),
    sprintf("N=%d", nrow(m4$model)),
    sprintf("N=%d", nrow(m5$model))
  ),
  Agency_Coef = c(
    sprintf("%.4f", coef(m1)[1]),
    sprintf("%.4f", coef(m2)[1]),
    sprintf("%.4f", coef(m3)[1]),
    sprintf("%.4f", coef(m4)[1]),
    "See components"
  ),
  SE = c(
    sprintf("%.4f", summary(m1)$coefficients[1, 2]),
    sprintf("%.4f", summary(m2)$coefficients[1, 2]),
    sprintf("%.4f", summary(m3)$coefficients[1, 2]),
    sprintf("%.4f", summary(m4)$coefficients[1, 2]),
    ""
  ),
  p_value = c(
    sprintf("%.4f", summary(m1)$coefficients[1, 4]),
    sprintf("%.4f", summary(m2)$coefficients[1, 4]),
    sprintf("%.4f", summary(m3)$coefficients[1, 4]),
    sprintf("%.4f", summary(m4)$coefficients[1, 4]),
    ""
  ),
  Sig = c(
    ifelse(summary(m1)$coefficients[1, 4] < 0.05, "***", ""),
    ifelse(summary(m2)$coefficients[1, 4] < 0.05, "***", ""),
    ifelse(summary(m3)$coefficients[1, 4] < 0.05, "***", ""),
    ifelse(summary(m4)$coefficients[1, 4] < 0.05, "***", ""),
    ""
  )
)

print(results_table)

# Save
write_csv(results_table, file.path(output_path, "did_results", "true_agency_results.csv"))

# ============================================================================
# Correlations
# ============================================================================
cat("\n=== Correlations: Agency Components with TEA ===\n\n")

corr_matrix <- aps_agg %>%
  select(tea_pct, agency_mean, fear_pct, skill_pct, know_pct, opport_pct) %>%
  cor(use = "complete.obs") %>%
  round(3)

print(corr_matrix)

# ============================================================================
# Save data for next step (DiD)
# ============================================================================

saveRDS(aps_agg, file.path(output_path, "did_results", "aps_agg_true_agency_for_did.rds"))

cat("\n\n=== KEY FINDINGS ===\n\n")

m1_p <- summary(m1)$coefficients[1, 4]
m1_coef <- coef(m1)[1]

if (m1_p < 0.05) {
  cat(sprintf("✓ SIGNIFICANT agency effect detected (p=%.4f)\n", m1_p))
  if (m1_coef > 0) {
    cat(sprintf("✓ POSITIVE: One-point increase in agency → %.2f pp increase in TEA\n", m1_coef))
  } else {
    cat(sprintf("✗ NEGATIVE effect (interpretation issue?)\n"))
  }
} else {
  cat(sprintf("✗ No significant agency effect (p=%.4f)\n", m1_p))
}

cat("\nNote: Drop in 2019-2020 suggests need to check measurement consistency.\n")
cat("      Analysis 2009-2017 (Model 3) may be more reliable.\n")

cat("\n=== Analysis Complete ===\n")
