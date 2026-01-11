################################################################################
# PHASE 28: HETEROGENEITY ANALYSIS
################################################################################
# Purpose: Test if agency effects vary by gender and education level
# Method:  Country FE with interaction terms (agency × gender, agency × educ)
# Theory:  - Women may face additional barriers despite high agency
#          - Education may amplify or substitute for agency effects
################################################################################

cat("\n")
cat("================================================================================\n")
cat("PHASE 28: HETEROGENEITY ANALYSIS\n")
cat("================================================================================\n")
cat("Testing agency × gender and agency × education interactions...\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(lfe)
  library(stargazer)
})

# Load balanced panel
panel <- read_csv("data_processed/panel/panel_2017_2019_balanced.csv", show_col_types = FALSE)

cat("[1] Preparing data...\n")
cat("================================================================================\n")

# Filter complete cases and create analysis sample
analysis_data <- panel %>%
  filter(!is.na(tea_binary), !is.na(agency_index), !is.na(age), !is.na(female),
         !is.na(gemeduc), year %in% c(2017, 2018, 2019)) %>%
  mutate(
    # Center agency for interpretability
    agency_centered = agency_index - mean(agency_index, na.rm = TRUE),
    # Create age squared
    age_sq = age^2,
    # Education: Binary split (post-secondary vs less)
    # Note: Very few people with low education in this young sample
    educ_postsec = ifelse(!is.na(gemeduc) & gemeduc >= 4, 1, 0)
  )

cat(sprintf("  Complete cases: %s observations\n", format(nrow(analysis_data), big.mark=",")))
cat(sprintf("  Countries: %d\n", length(unique(analysis_data$country))))
cat(sprintf("  Years: %s\n\n", paste(sort(unique(analysis_data$year)), collapse=", ")))

# Sample composition
cat("  Sample composition:\n")
cat(sprintf("    Female: %.1f%%\n", mean(analysis_data$female, na.rm=TRUE) * 100))
cat(sprintf("    Post-secondary education: %.1f%%\n", mean(analysis_data$educ_postsec, na.rm=TRUE) * 100))
cat(sprintf("    Mean agency: %.3f (SD=%.3f)\n", 
            mean(analysis_data$agency_index, na.rm=TRUE),
            sd(analysis_data$agency_index, na.rm=TRUE)))
cat(sprintf("    TEA rate: %.1f%%\n\n", mean(analysis_data$tea_binary, na.rm=TRUE) * 100))


################################################################################
# STEP 2: BASELINE MODEL (FROM PHASE 24)
################################################################################

cat("[2] Baseline model (no interactions)...\n")
cat("================================================================================\n")

baseline_fe <- felm(
  tea_binary ~ agency_index + age + age_sq + female + gemeduc | 
    country + factor(year) | 0 | country,
  data = analysis_data
)

cat("  ✓ Baseline estimated\n")
coef_baseline <- summary(baseline_fe)$coefficients
cat(sprintf("    Agency: β=%.4f (SE=%.4f, p=%.4f)\n",
            coef_baseline["agency_index", "Estimate"],
            coef_baseline["agency_index", "Cluster s.e."],
            coef_baseline["agency_index", "Pr(>|t|)"]))


################################################################################
# STEP 3: GENDER INTERACTION
################################################################################

cat("\n[3] Agency × Gender interaction...\n")
cat("================================================================================\n")
cat("  Testing: Do women benefit differently from agency?\n\n")

gender_fe <- felm(
  tea_binary ~ agency_centered + female + agency_centered:female + 
    age + age_sq + gemeduc | 
    country + factor(year) | 0 | country,
  data = analysis_data
)

cat("  ✓ Gender interaction model estimated\n\n")

coef_gender <- summary(gender_fe)$coefficients
cat("  Coefficients:\n")
cat(sprintf("    Agency (men): β=%.4f (SE=%.4f, p=%.4f)\n",
            coef_gender["agency_centered", "Estimate"],
            coef_gender["agency_centered", "Cluster s.e."],
            coef_gender["agency_centered", "Pr(>|t|)"]))

if ("agency_centered:female" %in% rownames(coef_gender)) {
  beta_int <- coef_gender["agency_centered:female", "Estimate"]
  se_int <- coef_gender["agency_centered:female", "Cluster s.e."]
  p_int <- coef_gender["agency_centered:female", "Pr(>|t|)"]
  
  cat(sprintf("    Agency × Female: β=%.4f (SE=%.4f, p=%.4f)\n", beta_int, se_int, p_int))
  
  # Total effect for women
  beta_women <- coef_gender["agency_centered", "Estimate"] + beta_int
  cat(sprintf("    Agency (women): β=%.4f\n", beta_women))
  
  # Interpretation
  cat("\n  Interpretation:\n")
  if (p_int < 0.05) {
    if (beta_int > 0) {
      cat("    ✓ Women benefit MORE from agency than men (positive interaction)\n")
      pct_boost <- (beta_int / coef_gender["agency_centered", "Estimate"]) * 100
      cat(sprintf("      Women's agency effect is %.0f%% larger\n", pct_boost))
    } else {
      cat("    ⚠ Women benefit LESS from agency than men (negative interaction)\n")
      pct_penalty <- (abs(beta_int) / coef_gender["agency_centered", "Estimate"]) * 100
      cat(sprintf("      Women's agency effect is %.0f%% smaller\n", pct_penalty))
    }
  } else {
    cat("    • No significant gender difference in agency effects (p>0.05)\n")
    cat("      Agency promotes entrepreneurship equally for men and women\n")
  }
}


################################################################################
# STEP 4: EDUCATION INTERACTION
################################################################################

cat("\n[4] Agency × Education interaction...\n")
cat("================================================================================\n")
cat("  Testing: Does post-secondary education amplify or substitute for agency?\n\n")

educ_fe <- felm(
  tea_binary ~ agency_centered + educ_postsec + 
    agency_centered:educ_postsec +
    age + age_sq + female | 
    country + factor(year) | 0 | country,
  data = analysis_data
)

cat("  ✓ Education interaction model estimated\n\n")

coef_educ <- summary(educ_fe)$coefficients
cat("  Coefficients:\n")
cat(sprintf("    Agency (no post-sec): β=%.4f (SE=%.4f, p=%.4f)\n",
            coef_educ["agency_centered", "Estimate"],
            coef_educ["agency_centered", "Cluster s.e."],
            coef_educ["agency_centered", "Pr(>|t|)"]))

if ("agency_centered:educ_postsec" %in% rownames(coef_educ)) {
  beta_int_educ <- coef_educ["agency_centered:educ_postsec", "Estimate"]
  se_int_educ <- coef_educ["agency_centered:educ_postsec", "Cluster s.e."]
  p_int_educ <- coef_educ["agency_centered:educ_postsec", "Pr(>|t|)"]
  
  cat(sprintf("    Agency × Post-sec: β=%.4f (SE=%.4f, p=%.4f)\n", 
              beta_int_educ, se_int_educ, p_int_educ))
  cat(sprintf("    Agency (post-sec): β=%.4f\n", 
              coef_educ["agency_centered", "Estimate"] + beta_int_educ))
  
  # Interpretation
  cat("\n  Interpretation:\n")
  if (p_int_educ < 0.05) {
    if (beta_int_educ > 0) {
      cat("    ✓ Post-secondary education AMPLIFIES agency effects\n")
      cat("      Education and agency are complements\n")
      cat("      → Entrepreneurship education should include agency-building\n")
    } else {
      cat("    • Post-secondary education SUBSTITUTES for agency\n")
      cat("      Education provides alternative pathway\n")
      cat("      → For highly educated, focus on market/finance access\n")
    }
  } else {
    cat("    • No significant education interaction (p>0.05)\n")
    cat("      Agency effects are consistent across education levels\n")
  }
}


################################################################################
# STEP 5: COMBINED MODEL (GENDER + EDUCATION)
################################################################################

cat("\n[5] Combined model (gender + education interactions)...\n")
cat("================================================================================\n")

combined_fe <- felm(
  tea_binary ~ agency_centered + female + educ_postsec +
    agency_centered:female + agency_centered:educ_postsec +
    age + age_sq | 
    country + factor(year) | 0 | country,
  data = analysis_data
)

cat("  ✓ Combined model estimated\n\n")

coef_combined <- summary(combined_fe)$coefficients
cat("  Key coefficients:\n")
print(coef_combined[grep("agency", rownames(coef_combined)), c("Estimate", "Cluster s.e.", "Pr(>|t|)")], 
      digits = 4)


################################################################################
# STEP 6: EFFECT SIZE CALCULATIONS
################################################################################

cat("\n[6] Effect size summary...\n")
cat("================================================================================\n")

# Calculate effects for different groups
sd_agency <- sd(analysis_data$agency_index, na.rm = TRUE)

# Baseline effect (from gender model, centered at mean)
beta_base <- coef_gender["agency_centered", "Estimate"]
effect_base_pp <- beta_base * sd_agency * 100

cat(sprintf("  1 SD increase in agency (SD=%.3f):\n", sd_agency))
cat(sprintf("    Men (baseline): %+.2f pp TEA\n", effect_base_pp))

if (exists("beta_int") && "agency_centered:female" %in% rownames(coef_gender)) {
  effect_women_pp <- (beta_base + beta_int) * sd_agency * 100
  cat(sprintf("    Women: %+.2f pp TEA", effect_women_pp))
  if (coef_gender["agency_centered:female", "Pr(>|t|)"] < 0.05) {
    cat(" *\n")
  } else {
    cat(" (NS diff)\n")
  }
}

if (exists("beta_int_educ") && "agency_centered:educ_postsec" %in% rownames(coef_educ)) {
  cat("\n  By education level:\n")
  
  beta_nosec <- coef_educ["agency_centered", "Estimate"]
  effect_nosec_pp <- beta_nosec * sd_agency * 100
  effect_postsec_pp <- (beta_nosec + beta_int_educ) * sd_agency * 100
  
  cat(sprintf("    No post-secondary: %+.2f pp TEA\n", effect_nosec_pp))
  
  cat(sprintf("    Post-secondary: %+.2f pp TEA", effect_postsec_pp))
  if (coef_educ["agency_centered:educ_postsec", "Pr(>|t|)"] < 0.05) cat(" *")
  cat("\n")
}


################################################################################
# STEP 7: SAVE RESULTS
################################################################################

cat("\n[7] Saving results...\n")
cat("================================================================================\n")

# Save model objects
saveRDS(list(
  baseline = baseline_fe,
  gender_interaction = gender_fe,
  education_interaction = educ_fe,
  combined = combined_fe
), "output/tables/table28_heterogeneity_models.rds")

# Export tables
stargazer(baseline_fe, gender_fe, educ_fe, combined_fe,
          type = "text",
          title = "Heterogeneity Analysis: Agency Effects by Gender and Education",
          column.labels = c("Baseline", "Gender", "Education", "Combined"),
          dep.var.labels = "TEA (0/1)",
          covariate.labels = c("Agency", "Female", "Post-secondary educ",
                               "Agency × Female", "Agency × Post-sec",
                               "Age", "Age squared"),
          omit.stat = c("ser", "f"),
          out = "output/tables/table28_heterogeneity.txt")

# Export CSV of coefficients
heterogeneity_results <- data.frame(
  model = c("Baseline", "Gender", "Education", "Combined"),
  agency_coef = c(
    coef_baseline["agency_index", "Estimate"],
    coef_gender["agency_centered", "Estimate"],
    coef_educ["agency_centered", "Estimate"],
    coef_combined["agency_centered", "Estimate"]
  ),
  agency_se = c(
    coef_baseline["agency_index", "Cluster s.e."],
    coef_gender["agency_centered", "Cluster s.e."],
    coef_educ["agency_centered", "Cluster s.e."],
    coef_combined["agency_centered", "Cluster s.e."]
  ),
  agency_p = c(
    coef_baseline["agency_index", "Pr(>|t|)"],
    coef_gender["agency_centered", "Pr(>|t|)"],
    coef_educ["agency_centered", "Pr(>|t|)"],
    coef_combined["agency_centered", "Pr(>|t|)"]
  )
)

write.csv(heterogeneity_results, 
          "output/tables/table28_heterogeneity_summary.csv",
          row.names = FALSE)

cat("  ✓ Results saved to output/tables/\n")


################################################################################
# INTERPRETATION & POLICY IMPLICATIONS
################################################################################

cat("\n================================================================================\n")
cat("INTERPRETATION & POLICY IMPLICATIONS\n")
cat("================================================================================\n\n")

cat("HETEROGENEITY FINDINGS:\n\n")

cat("1. GENDER:\n")
if (exists("beta_int") && coef_gender["agency_centered:female", "Pr(>|t|)"] < 0.05) {
  if (beta_int > 0) {
    cat("   ✓ Women benefit MORE from agency than men\n")
    cat("   → Policy: Prioritize agency-building programs for women\n")
    cat("   → Women with high agency overcome additional barriers\n\n")
  } else {
    cat("   ⚠ Women benefit LESS from agency than men\n")
    cat("   → Policy: Agency alone insufficient for women\n")
    cat("   → Need to address structural barriers (finance, networks, discrimination)\n\n")
  }
} else {
  cat("   • Agency effects are EQUAL across genders\n")
  cat("   → Policy: Gender-neutral agency interventions are appropriate\n")
  cat("   → Focus resources on universal agency development\n\n")
}

cat("2. EDUCATION:\n")
if (exists("beta_int_educ")) {
  if (coef_educ["agency_centered:educ_postsec", "Pr(>|t|)"] < 0.05) {
    if (beta_int_educ > 0) {
      cat("   ✓ Post-secondary education AMPLIFIES agency effects (complements)\n")
      cat("   → Policy: Combined education + agency programs most effective\n")
      cat("   → Entrepreneurship education should include agency-building\n\n")
    } else {
      cat("   • Post-secondary education SUBSTITUTES for agency\n")
      cat("   → Policy: Education provides alternative pathway\n")
      cat("   → For highly educated, focus on market/finance access\n\n")
    }
  } else {
    cat("   • Agency effects are CONSISTENT across education levels\n")
    cat("   → Policy: Agency interventions work regardless of education\n")
    cat("   → Inclusive programs appropriate for diverse populations\n\n")
  }
}

cat("\n================================================================================\n")
cat("✓ PHASE 28 COMPLETE: Heterogeneity Analysis\n")
cat("================================================================================\n\n")

cat("CAUSAL INFERENCE TIMELINE - FINAL STATUS:\n")
cat("  ✓ Phase 22: Panel Integration (2017-2019)\n")
cat("  ✓ Phase 24: Country Fixed Effects (PRIMARY CAUSAL ESTIMATE)\n")
cat("  ✓ Phase 25: Lagged Analysis (no reverse causality)\n")
cat("  ⏭ Phase 26: DiD (NECI only available for 2019)\n")
cat("  ⏭ Phase 27: GMM (repeated cross-sections, not true panel)\n")
cat("  ✓ Phase 28: Heterogeneity Analysis (gender × education)\n\n")

cat("READY FOR:\n")
cat("  → Documentation synthesis\n")
cat("  → Policy brief development\n")
cat("  → Academic paper integration\n\n")
