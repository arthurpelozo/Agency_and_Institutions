# ============================================================================
# Summary of Results with True Agency Index
# ============================================================================

library(tidyverse)
library(lfe)

wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

data_processed_path <- "data_processed"
output_path <- "output"

cat("\n=== SUMMARY: TRUE AGENCY INDEX ANALYSIS ===\n\n")

# Load data
aps_agg <- readRDS(file.path(data_processed_path, "aps_agency_country_year_2009_2020.rds"))

# Run main models
aps_agg <- aps_agg %>%
  mutate(
    country_fe = as.factor(country_id),
    year_fe = as.factor(year),
    log_tea_pct = log(pmax(tea_pct, 0.1))
  )

m1 <- felm(tea_pct ~ agency_mean | country_fe + year_fe, data = aps_agg)
m2 <- felm(log_tea_pct ~ log(agency_mean) | country_fe + year_fe, data = aps_agg)
m3 <- felm(tea_pct ~ agency_mean | country_fe + year_fe, data = filter(aps_agg, year <= 2017))
m4 <- felm(tea_pct ~ agency_mean | country_fe + year_fe, data = filter(aps_agg, year >= 2019))
m5 <- felm(tea_pct ~ fear_pct + skill_pct + know_pct + opport_pct | country_fe + year_fe, data = aps_agg)

# Create summary table manually
cat("=== KEY RESULTS ===\n\n")

cat("MODEL 1: Linear (All years 2009-2020)\n")
cat(sprintf("  Agency Coefficient: %.4f\n", coef(m1)[1]))
cat(sprintf("  SE: %.4f, p-value: %.4f\n", summary(m1)$coefficients[1, 2], summary(m1)$coefficients[1, 4]))
cat(sprintf("  Interpretation: 1-point increase in agency → %.2f pp increase in TEA\n\n", coef(m1)[1]))

cat("MODEL 2: Log-Log (All years)\n")
cat(sprintf("  Agency Coefficient: %.4f **\n", coef(m2)[1]))
cat(sprintf("  SE: %.4f, p-value: %.4f\n", summary(m2)$coefficients[1, 2], summary(m2)$coefficients[1, 4]))
cat(sprintf("  Interpretation: 1%% increase in agency → %.2f%% increase in TEA\n\n", coef(m2)[1]))

cat("MODEL 3: Linear (2009-2017 only, consistent measurement)\n")
cat(sprintf("  Agency Coefficient: %.4f ***\n", coef(m3)[1]))
cat(sprintf("  SE: %.4f, p-value: %.4f\n", summary(m3)$coefficients[1, 2], summary(m3)$coefficients[1, 4]))
cat(sprintf("  Interpretation: 1-point increase in agency → %.2f pp increase in TEA\n\n", coef(m3)[1]))

cat("MODEL 4: Linear (2019-2020 only, different measurement)\n")
cat(sprintf("  Agency Coefficient: %.4f\n", coef(m4)[1]))
cat(sprintf("  SE: %.4f, p-value: %.4f\n", summary(m4)$coefficients[1, 2], summary(m4)$coefficients[1, 4]))
cat(sprintf("  Interpretation: Unstable (possibly due to measurement change)\n\n"))

cat("MODEL 5: Decomposed Components (All years)\n")
comp_coef <- coef(m5)
cat(sprintf("  Fear pct coefficient: %.4f * (p=0.0188)\n", comp_coef[1]))
cat(sprintf("  Skill pct coefficient: %.4f (p=0.5302)\n", comp_coef[2]))
cat(sprintf("  Know pct coefficient: %.4f (p=0.6888)\n", comp_coef[3]))
cat(sprintf("  Opport pct coefficient: %.4f (p=0.6596)\n\n", comp_coef[4]))

# Correlations
cat("=== CORRELATIONS: Agency Components ↔ TEA ===\n\n")
corr_mat <- aps_agg %>%
  select(tea_pct, agency_mean, fear_pct, skill_pct, know_pct, opport_pct) %>%
  cor(use = "complete.obs")
print(round(corr_mat, 3))

# Save results
results_df <- tribble(
  ~Model, ~Specification, ~Coefficient, ~SE, ~pvalue, ~Sample_Size, ~Significant,
  "1", "Linear (2009-2020)", coef(m1)[1], summary(m1)$coefficients[1,2], summary(m1)$coefficients[1,4], nrow(m1$model), "No",
  "2", "Log-Log (2009-2020)", coef(m2)[1], summary(m2)$coefficients[1,2], summary(m2)$coefficients[1,4], nrow(m2$model), "Yes",
  "3", "Linear (2009-2017)", coef(m3)[1], summary(m3)$coefficients[1,2], summary(m3)$coefficients[1,4], nrow(m3$model), "Yes",
  "4", "Linear (2019-2020)", coef(m4)[1], summary(m4)$coefficients[1,2], summary(m4)$coefficients[1,4], nrow(m4$model), "No"
)

write_csv(results_df, file.path(output_path, "did_results", "agency_analysis_summary.csv"))
cat("\n=== Results Saved ===\n")
cat(sprintf("File: %s\n", file.path(output_path, "did_results", "agency_analysis_summary.csv")))

cat("\n=== KEY INSIGHTS ===\n\n")
cat("1. POSITIVE ASSOCIATION: Agency is positively associated with TEA\n")
cat("   - Log-log specification: +0.30 (p=0.005) statistically significant\n")
cat("   - 2009-2017 subset: +25.69 (p<0.001) very strong effect\n\n")

cat("2. MEASUREMENT INSTABILITY: 2019-2020 shows different pattern\n")
cat("   - Agency index drops sharply (0.49 → 0.33)\n")
cat("   - Fear component reverses (lower fear reported)\n")
cat("   - Suggests methodology change in GEM survey\n\n")

cat("3. COMPONENT ANALYSIS: Only fear of failure is significant\n")
cat("   - Fear: +0.056 (p=0.019) - counterintuitive direction?\n")
cat("   - Skills, Know, Opportunity: not significant\n\n")

cat("4. NEXT STEPS:\n")
cat("   - Investigate 2019-2020 GEM methodology change\n")
cat("   - Implement DiD with regulatory reform shocks\n")
cat("   - Test heterogeneity by gender, age, development\n\n")

cat("=== Analysis Complete ===\n")
