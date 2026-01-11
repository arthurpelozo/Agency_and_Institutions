################################################################################
# PHASE 24: COUNTRY FIXED EFFECTS BASELINE
################################################################################
# Purpose: Estimate within-country effects of agency on TEA
# Input:   Panel dataset (2017-2019)
# Output:  Fixed effects regression results, comparison tables
# Causal upgrade: From cross-sectional to within-country variation
################################################################################

cat("\n")
cat("================================================================================\n")
cat("PHASE 24: COUNTRY FIXED EFFECTS ANALYSIS\n")
cat("================================================================================\n")
cat("Estimating within-country effects to remove time-invariant confounding...\n\n")

# Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(lfe)  # For fixed effects
  library(stargazer)
  library(broom)
})

# Create output directory
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

################################################################################
# STEP 1: LOAD PANEL DATA
################################################################################

cat("[1] Loading panel data...\n")
cat("================================================================================\n")

# Use balanced panel for cleaner fixed effects
panel <- read_csv("data_processed/panel/panel_2017_2019_balanced.csv", show_col_types = FALSE)

cat(sprintf("  Balanced panel: %s observations\n", format(nrow(panel), big.mark=",")))
cat(sprintf("  Countries: %d\n", n_distinct(panel$country)))
cat(sprintf("  Years: %d (%s)\n", n_distinct(panel$year), 
            paste(sort(unique(panel$year)), collapse=", ")))
cat(sprintf("  Age range: %d-%d\n", min(panel$age, na.rm=TRUE), max(panel$age, na.rm=TRUE)))

# Check completeness
missing_summary <- panel %>%
  summarise(
    n_total = n(),
    pct_tea_missing = mean(is.na(tea_binary)) * 100,
    pct_agency_missing = mean(is.na(agency_index)) * 100,
    pct_any_control_missing = mean(is.na(age) | is.na(female) | is.na(gemeduc)) * 100
  )

cat("\n  Missing data:\n")
cat(sprintf("    TEA: %.1f%%\n", missing_summary$pct_tea_missing))
cat(sprintf("    Agency: %.1f%%\n", missing_summary$pct_agency_missing))
cat(sprintf("    Controls: %.1f%%\n", missing_summary$pct_any_control_missing))


################################################################################
# STEP 2: PREPARE ANALYTIC SAMPLE
################################################################################

cat("\n[2] Preparing analytic sample...\n")
cat("================================================================================\n")

# Create analytic dataset with complete cases (2017-2019, harmonized agency)
panel_analytic <- panel %>%
  filter(year %in% c(2017, 2018, 2019)) %>%
  filter(!is.na(tea_binary), 
         !is.na(agency_index),
         !is.na(age), 
         !is.na(female),
         !is.na(gemeduc),
         !is.na(weight),
         weight > 0) %>%  # Remove missing or negative weights
  mutate(
    country_factor = factor(country),
    year_factor = factor(year)
  )

cat(sprintf("  Analytic sample: %s observations\n", format(nrow(panel_analytic), big.mark=",")))
cat(sprintf("  Countries: %d\n", n_distinct(panel_analytic$country)))
cat(sprintf("  Years: %s\n", paste(sort(unique(panel_analytic$year)), collapse=", ")))


################################################################################
# STEP 3: DESCRIPTIVE STATISTICS
################################################################################

cat("\n[3] Descriptive statistics...\n")
cat("================================================================================\n")

desc_stats <- panel_analytic %>%
  summarise(
    n = n(),
    tea_rate = mean(tea_binary, na.rm = TRUE) * 100,
    mean_agency = mean(agency_index, na.rm = TRUE),
    sd_agency = sd(agency_index, na.rm = TRUE),
    pct_female = mean(female, na.rm = TRUE) * 100,
    mean_age = mean(age, na.rm = TRUE),
    mean_educ = mean(gemeduc, na.rm = TRUE)
  )

cat("\n  Sample characteristics:\n")
cat(sprintf("    TEA rate: %.1f%%\n", desc_stats$tea_rate))
cat(sprintf("    Agency: M=%.3f, SD=%.3f\n", desc_stats$mean_agency, desc_stats$sd_agency))
cat(sprintf("    Female: %.1f%%\n", desc_stats$pct_female))
cat(sprintf("    Age: M=%.1f\n", desc_stats$mean_age))
cat(sprintf("    Education: M=%.2f\n", desc_stats$mean_educ))


################################################################################
# STEP 4: POOLED OLS BASELINE (FOR COMPARISON)
################################################################################

cat("\n[4] Pooled OLS baseline...\n")
cat("================================================================================\n")

# Simple pooled OLS with year dummies
model_pooled <- lm(tea_binary ~ agency_index + age + I(age^2) + female + 
                   gemeduc + factor(year),
                   data = panel_analytic,
                   weights = weight)

summary_pooled <- summary(model_pooled)

cat("\n  Pooled OLS results:\n")
cat(sprintf("    Agency coefficient: %.4f\n", coef(model_pooled)["agency_index"]))
cat(sprintf("    Std. error: %.4f\n", summary_pooled$coefficients["agency_index", "Std. Error"]))
cat(sprintf("    t-statistic: %.2f\n", summary_pooled$coefficients["agency_index", "t value"]))
cat(sprintf("    p-value: %.4f\n", summary_pooled$coefficients["agency_index", "Pr(>|t|)"]))
cat(sprintf("    R-squared: %.4f\n", summary_pooled$r.squared))


################################################################################
# STEP 5: COUNTRY FIXED EFFECTS MODEL
################################################################################

cat("\n[5] Country fixed effects model...\n")
cat("================================================================================\n")

# Fixed effects using felm (absorbs country dummies)
model_fe <- felm(tea_binary ~ agency_index + age + I(age^2) + female + gemeduc | 
                 country + year | 0 | country,
                 data = panel_analytic,
                 weights = panel_analytic$weight)

summary_fe <- summary(model_fe)

cat("\n  Country FE results:\n")
cat(sprintf("    Agency coefficient: %.4f\n", coef(model_fe)["agency_index"]))
cat(sprintf("    Clustered SE: %.4f\n", summary_fe$coefficients["agency_index", "Cluster s.e."]))
cat(sprintf("    t-statistic: %.2f\n", summary_fe$coefficients["agency_index", "t value"]))
cat(sprintf("    p-value: %.4f\n", summary_fe$coefficients["agency_index", "Pr(>|t|)"]))
cat(sprintf("    R-squared: %.4f\n", summary_fe$r.squared))
cat(sprintf("    Within R-squared: %.4f\n", summary_fe$r.squared))

# Calculate effect size
agency_sd <- sd(panel_analytic$agency_index, na.rm = TRUE)
fe_effect <- coef(model_fe)["agency_index"]
effect_1sd <- fe_effect * agency_sd * 100  # Convert to percentage points

cat(sprintf("\n  Effect interpretation:\n"))
cat(sprintf("    1 SD increase in agency → %.2f pp increase in TEA\n", effect_1sd))


################################################################################
# STEP 6: COMPARISON TABLE
################################################################################

cat("\n[6] Creating comparison table...\n")
cat("================================================================================\n")

# Extract coefficients
pooled_coef <- coef(model_pooled)["agency_index"]
pooled_se <- summary_pooled$coefficients["agency_index", "Std. Error"]
pooled_p <- summary_pooled$coefficients["agency_index", "Pr(>|t|)"]

fe_coef <- coef(model_fe)["agency_index"]
fe_se <- summary_fe$coefficients["agency_index", "Cluster s.e."]
fe_p <- summary_fe$coefficients["agency_index", "Pr(>|t|)"]

# Comparison
comparison <- data.frame(
  Model = c("Pooled OLS", "Country FE"),
  Agency_Coef = c(pooled_coef, fe_coef),
  Std_Error = c(pooled_se, fe_se),
  p_value = c(pooled_p, fe_p),
  Significance = c(
    ifelse(pooled_p < 0.001, "***", ifelse(pooled_p < 0.01, "**", ifelse(pooled_p < 0.05, "*", ""))),
    ifelse(fe_p < 0.001, "***", ifelse(fe_p < 0.01, "**", ifelse(fe_p < 0.05, "*", "")))
  ),
  Country_FE = c("No", "Yes"),
  Year_FE = c("Yes", "Yes"),
  N = c(nrow(panel_analytic), nrow(panel_analytic))
)

cat("\n  Comparison of models:\n")
print(comparison, row.names = FALSE)

# Calculate attenuation
attenuation_pct <- ((pooled_coef - fe_coef) / pooled_coef) * 100
cat(sprintf("\n  Attenuation: %.1f%% reduction when adding country FE\n", attenuation_pct))

if (fe_coef > 0 & fe_p < 0.05) {
  cat("  ✓ RESULT: Effect remains positive and significant within countries\n")
  cat("  ✓ CAUSAL UPGRADE: Within-country variation supports causal interpretation\n")
} else if (fe_coef > 0 & fe_p >= 0.05) {
  cat("  ⚠ RESULT: Effect positive but not significant within countries\n")
  cat("  → Cross-country differences may drive association\n")
} else {
  cat("  ✗ RESULT: Effect not robust to country fixed effects\n")
  cat("  → Association driven by cross-country variation only\n")
}


################################################################################
# STEP 7: SAVE OUTPUTS
################################################################################

cat("\n[7] Saving outputs...\n")
cat("================================================================================\n")

# Save comparison table
write_csv(comparison, "output/tables/table24_fe_comparison.csv")
cat("  ✓ Comparison table saved\n")

# Save full regression output
stargazer(model_pooled, model_fe,
          type = "text",
          out = "output/tables/table24_fe_regression.txt",
          title = "Country Fixed Effects Analysis",
          column.labels = c("Pooled OLS", "Country FE"),
          dep.var.labels = "TEA Binary",
          covariate.labels = c("Agency Index", "Age", "Age²", "Female", "Education"),
          omit = "factor",
          omit.stat = c("f", "ser"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Standard errors clustered by country for FE model")

cat("  ✓ Regression table saved\n")

# Save model objects for future reference
saveRDS(list(pooled = model_pooled, fe = model_fe), 
        "output/tables/models24_fe.rds")
cat("  ✓ Model objects saved\n")


################################################################################
# SUMMARY
################################################################################

cat("\n")
cat("================================================================================\n")
cat("✓ PHASE 24: COUNTRY FIXED EFFECTS COMPLETE\n")
cat("================================================================================\n")
cat(sprintf("  Sample: %s individuals, %d countries, 2017-2019\n",
            format(nrow(panel_analytic), big.mark=","), 
            n_distinct(panel_analytic$country)))
cat("\n")
cat("  KEY FINDINGS:\n")
cat(sprintf("    Pooled OLS: β=%.4f (p=%.4f)\n", pooled_coef, pooled_p))
cat(sprintf("    Country FE: β=%.4f (p=%.4f)\n", fe_coef, fe_p))
cat(sprintf("    Attenuation: %.1f%%\n", attenuation_pct))
cat("\n")

if (fe_coef > 0 & fe_p < 0.05) {
  cat("  CAUSAL INTERPRETATION:\n")
  cat("    Within-country increases in agency predict higher TEA,\n")
  cat("    controlling for time-invariant country characteristics.\n")
  cat("    This strengthens causal claims beyond cross-sectional evidence.\n")
  cat("\n")
  cat("  NEXT STEPS:\n")
  cat("    → Phase 25: Lagged analysis (test reverse causality)\n")
  cat("    → Policy shock research (identify natural experiments)\n")
}

cat("================================================================================\n")
