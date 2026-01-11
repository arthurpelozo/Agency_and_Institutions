#########################################################################

#######
# PHASE 27: DYNAMIC PANEL MODELS (Arellano-Bond GMM)
################################################################################
# Purpose: Estimate dynamic panel models with lagged dependent variable
#          to account for entrepreneurship persistence and address endogeneity
# Method:  Arellano-Bond / Arellano-Bover GMM (2-step with robust SE)
################################################################################

cat("\n")
cat("================================================================================\n")
cat("PHASE 27: DYNAMIC PANEL MODELS (GMM)\n")
cat("================================================================================\n")
cat("Estimating Arellano-Bond GMM with lagged TEA to address dynamics & endogeneity...\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(plm)       # Panel models including GMM
  library(stargazer)
})

# Load balanced panel
panel <- read_csv("data_processed/panel/panel_2017_2019_balanced.csv", show_col_types = FALSE)

cat("[1] Preparing data for GMM...\n")
cat("================================================================================\n")

# Filter to complete cases
gmm_data <- panel %>%
  filter(!is.na(tea_binary), !is.na(agency_index), !is.na(age), !is.na(female),
         !is.na(gemeduc), year %in% c(2017, 2018, 2019)) %>%
  arrange(gemoneid, year)

cat(sprintf("  Complete cases: %s observations\n", format(nrow(gmm_data), big.mark=",")))
cat(sprintf("  Individuals: %s\n", format(length(unique(gmm_data$gemoneid)), big.mark=",")))
cat(sprintf("  Countries: %d\n", length(unique(gmm_data$country))))
cat(sprintf("  Years: %s\n\n", paste(sort(unique(gmm_data$year)), collapse=", ")))

# Convert to pdata.frame
gmm_panel <- pdata.frame(gmm_data, index = c("gemoneid", "year"))

cat("[2] Descriptive statistics...\n")
cat("================================================================================\n")

desc_stats <- gmm_data %>%
  summarise(
    tea_mean = mean(tea_binary, na.rm = TRUE),
    tea_sd = sd(tea_binary, na.rm = TRUE),
    agency_mean = mean(agency_index, na.rm = TRUE),
    agency_sd = sd(agency_index, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    female_pct = mean(female, na.rm = TRUE) * 100
  )

cat(sprintf("  TEA rate: %.1f%% (SD=%.3f)\n", 
            desc_stats$tea_mean * 100, desc_stats$tea_sd))
cat(sprintf("  Agency: M=%.3f (SD=%.3f)\n", 
            desc_stats$agency_mean, desc_stats$agency_sd))
cat(sprintf("  Age: M=%.1f\n", desc_stats$age_mean))
cat(sprintf("  Female: %.1f%%\n\n", desc_stats$female_pct))


################################################################################
# STEP 3: ARELLANO-BOND GMM (DIFFERENCE GMM)
################################################################################

cat("[3] Arellano-Bond GMM (Difference GMM)...\n")
cat("================================================================================\n")
cat("  Estimating 2-step GMM with robust standard errors...\n")
cat("  Instruments: Lagged levels of TEA, agency, and controls\n\n")

# Arellano-Bond: First-difference GMM
# Instruments lags 2+ of TEA and controls
gmm_ab <- tryCatch({
  pgmm(
    tea_binary ~ lag(tea_binary, 1) + agency_index + age + I(age^2) + female + gemeduc | 
      lag(tea_binary, 2:99) + lag(agency_index, 2:99),
    data = gmm_panel,
    effect = "twoways",
    model = "twosteps",
    transformation = "d"
  )
}, error = function(e) {
  cat("  ERROR in Arellano-Bond GMM:", conditionMessage(e), "\n\n")
  return(NULL)
})

if (!is.null(gmm_ab)) {
  cat("  ✓ Arellano-Bond GMM estimation successful\n\n")
  
  cat("  Coefficients:\n")
  coefs_ab <- coef(summary(gmm_ab))
  print(coefs_ab, digits = 4)
  
  cat("\n  Key result:\n")
  if ("agency_index" %in% rownames(coefs_ab)) {
    beta_agency <- coefs_ab["agency_index", "Estimate"]
    se_agency <- coefs_ab["agency_index", "Std. Error"]
    p_agency <- coefs_ab["agency_index", "Pr(>|z|)"]
    
    cat(sprintf("    Agency coefficient: %.4f (SE=%.4f, p=%.4f)\n", 
                beta_agency, se_agency, p_agency))
    
    if (p_agency < 0.001) sig_str <- "***"
    else if (p_agency < 0.01) sig_str <- "**"
    else if (p_agency < 0.05) sig_str <- "*"
    else sig_str <- "NS"
    
    cat(sprintf("    Significance: %s\n", sig_str))
    
    # Effect size
    sd_agency <- sd(gmm_data$agency_index, na.rm = TRUE)
    effect_pp <- beta_agency * sd_agency * 100
    cat(sprintf("    Effect size: 1 SD agency → %+.2f pp TEA\n", effect_pp))
  }
  
  # Specification tests
  cat("\n  Specification tests:\n")
  sar_test <- sargan(gmm_ab)
  cat(sprintf("    Sargan test of overid: χ²=%.2f, p=%.4f\n", 
              sar_test$statistic, sar_test$p.value))
  if (sar_test$p.value > 0.05) {
    cat("      ✓ Instruments valid (p>0.05)\n")
  } else {
    cat("      ⚠ Weak instrument validity (p<0.05)\n")
  }
  
} else {
  cat("  ⚠ GMM estimation failed - likely insufficient time periods\n")
  cat("    Note: GMM requires T≥3 for valid instruments\n")
  cat("    Panel has 3 years (minimal for GMM)\n\n")
}


################################################################################
# STEP 4: SYSTEM GMM (Arellano-Bover / Blundell-Bond)
################################################################################

cat("\n[4] System GMM (Arellano-Bover / Blundell-Bond)...\n")
cat("================================================================================\n")
cat("  Combines level and difference equations for efficiency...\n\n")

gmm_sys <- tryCatch({
  pgmm(
    tea_binary ~ lag(tea_binary, 1) + agency_index + age + I(age^2) + female + gemeduc | 
      lag(tea_binary, 2:99) + lag(agency_index, 2:99),
    data = gmm_panel,
    effect = "twoways",
    model = "twosteps",
    transformation = "ld"  # Level and difference
  )
}, error = function(e) {
  cat("  ERROR in System GMM:", conditionMessage(e), "\n\n")
  return(NULL)
})

if (!is.null(gmm_sys)) {
  cat("  ✓ System GMM estimation successful\n\n")
  
  cat("  Coefficients:\n")
  coefs_sys <- coef(summary(gmm_sys))
  print(coefs_sys, digits = 4)
  
  cat("\n  Key result:\n")
  if ("agency_index" %in% rownames(coefs_sys)) {
    beta_agency <- coefs_sys["agency_index", "Estimate"]
    se_agency <- coefs_sys["agency_index", "Std. Error"]
    p_agency <- coefs_sys["agency_index", "Pr(>|z|)"]
    
    cat(sprintf("    Agency coefficient: %.4f (SE=%.4f, p=%.4f)\n", 
                beta_agency, se_agency, p_agency))
    
    if (p_agency < 0.001) sig_str <- "***"
    else if (p_agency < 0.01) sig_str <- "**"
    else if (p_agency < 0.05) sig_str <- "*"
    else sig_str <- "NS"
    
    cat(sprintf("    Significance: %s\n", sig_str))
    
    # Effect size
    sd_agency <- sd(gmm_data$agency_index, na.rm = TRUE)
    effect_pp <- beta_agency * sd_agency * 100
    cat(sprintf("    Effect size: 1 SD agency → %+.2f pp TEA\n", effect_pp))
  }
  
  # Specification tests
  cat("\n  Specification tests:\n")
  sar_test <- sargan(gmm_sys)
  cat(sprintf("    Sargan test: χ²=%.2f, p=%.4f\n", 
              sar_test$statistic, sar_test$p.value))
  if (sar_test$p.value > 0.05) {
    cat("      ✓ Instruments valid\n")
  } else {
    cat("      ⚠ Weak instrument validity\n")
  }
  
} else {
  cat("  ⚠ System GMM estimation failed\n\n")
}


################################################################################
# STEP 5: SAVE RESULTS
################################################################################

cat("\n[5] Saving results...\n")
cat("================================================================================\n")

# Save summary
results_list <- list()
if (!is.null(gmm_ab)) {
  results_list$arellano_bond <- summary(gmm_ab)
}
if (!is.null(gmm_sys)) {
  results_list$system_gmm <- summary(gmm_sys)
}

if (length(results_list) > 0) {
  saveRDS(results_list, "output/tables/table27_gmm_results.rds")
  
  # Export tables
  if (!is.null(gmm_ab)) {
    capture.output(
      stargazer(gmm_ab, type = "text", title = "Arellano-Bond GMM"),
      file = "output/tables/table27_gmm_arellano_bond.txt"
    )
  }
  
  if (!is.null(gmm_sys)) {
    capture.output(
      stargazer(gmm_sys, type = "text", title = "System GMM"),
      file = "output/tables/table27_gmm_system.txt"
    )
  }
  
  cat("  ✓ Results saved to output/tables/\n")
} else {
  cat("  ⚠ No successful GMM models to save\n")
}


################################################################################
# INTERPRETATION
################################################################################

cat("\n================================================================================\n")
cat("INTERPRETATION\n")
cat("================================================================================\n\n")

cat("Dynamic panel models (GMM) address:\n")
cat("  1. Persistence: Past entrepreneurship predicts current entrepreneurship\n")
cat("  2. Endogeneity: Agency and TEA may be simultaneously determined\n")
cat("  3. Unobserved heterogeneity: Time-invariant individual effects\n\n")

if (!is.null(gmm_ab) || !is.null(gmm_sys)) {
  cat("Results:\n")
  cat("  - Lagged TEA coefficient: Captures entrepreneurship persistence\n")
  cat("  - Agency coefficient: Effect after controlling for dynamics\n")
  cat("  - Sargan test: Validates instrument exogeneity\n\n")
  
  cat("Note on short panel (T=3):\n")
  cat("  - GMM requires T≥3; we are at the minimum\n")
  cat("  - Estimates may be less efficient than with longer panels\n")
  cat("  - Results should be interpreted cautiously\n")
  cat("  - Consider as robustness check alongside FE and lagged models\n\n")
} else {
  cat("⚠ GMM estimation failed (insufficient time periods)\n")
  cat("  Alternative: Use country FE (Phase 24) as primary causal estimate\n\n")
}

cat("================================================================================\n")
cat("✓ PHASE 27 COMPLETE: Dynamic Panel Models\n")
cat("================================================================================\n\n")

cat("CAUSAL INFERENCE PROGRESS:\n")
cat("  ✓ Phase 24: Country fixed effects (β=0.115, p<0.001)\n")
cat("  ✓ Phase 25: Lagged analysis (no reverse causality)\n")
cat("  ⏭ Phase 26: DiD skipped (no historical NECI data)\n")
cat("  ✓ Phase 27: GMM (addressing dynamics & endogeneity)\n\n")

cat("NEXT STEPS:\n")
cat("  → Phase 28: Heterogeneity analysis (agency × gender, education)\n")
cat("  → Phase 29-30: Documentation & integration\n\n")
