#!/usr/bin/env Rscript

# ============================================================================
# SCRIPT 08: GENDER HETEROGENEITY ANALYSIS
# ============================================================================

cat("\n================================================================================\n")
cat("PHASE 7: GENDER HETEROGENEITY - FOR WHOM DOES AGENCY MATTER MOST?\n")
cat("================================================================================\n\n")

library(lme4)
library(tidyverse)

df <- read.csv("data_processed/primary_sample.csv")
df$tea_binary <- ifelse(df$teaactivityr == 1, 0, 1)
df <- df[!is.na(df$neci1_mean10) & !is.na(df$neci2_mean10) & !is.na(df$neci3_mean10), ]
df$nes_inq_std <- scale(df$nes_inq_mean10)[,1]

cat("[1] Sample composition by gender...\n")
cat("================================================================================\n")

gender_summary <- table(df$gender, useNA = "always")
cat("  Gender distribution:\n")
print(gender_summary)
cat("\n")

# ============================================================================
# ANALYSIS 1: Stratified Models (Men vs Women)
# ============================================================================

cat("[2] Running stratified models by gender...\n")
cat("================================================================================\n")

# Model for Men (assuming gender = 1 is male, 2 is female - verify this)
df_men <- df[df$gender == 1, ]
df_women <- df[df$gender == 2, ]

cat("\nMEN'S MODEL (N =", nrow(df_men), ")\n")
cat("─────────────────────────────────\n")

m_men <- glmer(tea_binary ~ agency_index + nes_inq_std + agency_index:nes_inq_std +
               age + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + (1|country),
               data = df_men, family = binomial, nAGQ = 1)

coef_men <- coef(summary(m_men))
agency_effect_men <- coef_men["agency_index", ]

cat("  Agency Effect: β =", round(agency_effect_men[1], 4), 
    ", SE =", round(agency_effect_men[2], 4),
    ", p =", format.pval(agency_effect_men[4], digits = 3), "\n")

cat("\nWOMEN'S MODEL (N =", nrow(df_women), ")\n")
cat("───────────────────────────────────\n")

m_women <- glmer(tea_binary ~ agency_index + nes_inq_std + agency_index:nes_inq_std +
                 age + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + (1|country),
                 data = df_women, family = binomial, nAGQ = 1)

coef_women <- coef(summary(m_women))
agency_effect_women <- coef_women["agency_index", ]

cat("  Agency Effect: β =", round(agency_effect_women[1], 4), 
    ", SE =", round(agency_effect_women[2], 4),
    ", p =", format.pval(agency_effect_women[4], digits = 3), "\n")

# ============================================================================
# ANALYSIS 2: Interaction Model (Agency × Gender)
# ============================================================================

cat("\n\n[3] Testing Agency × Gender interaction...\n")
cat("================================================================================\n")

m_interaction <- glmer(tea_binary ~ agency_index + gender + agency_index:gender + 
                       nes_inq_std + age + hhsize + gemeduc + gemhhinc + 
                       gemoccu + gemwork + (1|country),
                       data = df, family = binomial, nAGQ = 1)

coef_int <- coef(summary(m_interaction))

cat("\nInteraction term (Agency × Gender):\n")
if ("agency_index:gender" %in% rownames(coef_int)) {
  int_effect <- coef_int["agency_index:gender", ]
  cat("  β =", round(int_effect[1], 4), 
      ", SE =", round(int_effect[2], 4),
      ", p =", format.pval(int_effect[4], digits = 3), "\n")
  
  if (int_effect[4] < 0.05) {
    cat("\n  ✓ SIGNIFICANT interaction: Agency effect DIFFERS by gender\n")
  } else {
    cat("\n  ✗ NO significant interaction: Agency effect SIMILAR across genders\n")
  }
}

# ============================================================================
# ANALYSIS 3: TEA Participation Rates by Agency Quartiles & Gender
# ============================================================================

cat("\n\n[4] TEA rates by agency level and gender...\n")
cat("================================================================================\n")

df$agency_quartile <- cut(df$agency_index, 
                          breaks = quantile(df$agency_index, probs = 0:4/4),
                          labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)"),
                          include.lowest = TRUE)

tea_by_agency_gender <- df %>%
  group_by(agency_quartile, gender) %>%
  summarise(
    N = n(),
    TEA_rate = mean(tea_binary, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\n  TEA Participation Rates (%):\n")
cat("  ════════════════════════════\n")
tea_wide <- tea_by_agency_gender %>%
  pivot_wider(names_from = gender, values_from = TEA_rate, names_prefix = "Gender_")

print(tea_wide)

# ============================================================================
# SUMMARY TABLE
# ============================================================================

gender_results <- data.frame(
  Group = c("Men", "Women", "Interaction"),
  N = c(nrow(df_men), nrow(df_women), nrow(df)),
  Agency_Beta = c(agency_effect_men[1], agency_effect_women[1], 
                  ifelse("agency_index:gender" %in% rownames(coef_int), 
                         coef_int["agency_index:gender", 1], NA)),
  Agency_SE = c(agency_effect_men[2], agency_effect_women[2],
                ifelse("agency_index:gender" %in% rownames(coef_int), 
                       coef_int["agency_index:gender", 2], NA)),
  Agency_Pval = c(agency_effect_men[4], agency_effect_women[4],
                  ifelse("agency_index:gender" %in% rownames(coef_int), 
                         coef_int["agency_index:gender", 4], NA)),
  Significant = c(
    ifelse(agency_effect_men[4] < 0.05, "***", ""),
    ifelse(agency_effect_women[4] < 0.05, "***", ""),
    ifelse("agency_index:gender" %in% rownames(coef_int) && 
           coef_int["agency_index:gender", 4] < 0.05, "***", "")
  )
)

write.csv(gender_results, "output/tables/table8_gender_heterogeneity.csv", row.names = FALSE)
write.csv(tea_by_agency_gender, "output/tables/table8_tea_by_agency_gender.csv", row.names = FALSE)

# ============================================================================
# INTERPRETATION
# ============================================================================

cat("\n\n[5] INTERPRETATION\n")
cat("================================================================================\n")

effect_diff <- agency_effect_women[1] - agency_effect_men[1]
larger_for <- ifelse(effect_diff > 0, "WOMEN", "MEN")

cat("\nKEY FINDINGS:\n")
cat("─────────────\n")
cat(sprintf("1. Agency effect for MEN: β = %.4f (p %s)\n", 
            agency_effect_men[1], format.pval(agency_effect_men[4], digits=3)))
cat(sprintf("2. Agency effect for WOMEN: β = %.4f (p %s)\n", 
            agency_effect_women[1], format.pval(agency_effect_women[4], digits=3)))
cat(sprintf("3. Effect difference: %.4f (larger for %s)\n", abs(effect_diff), larger_for))

if ("agency_index:gender" %in% rownames(coef_int)) {
  if (coef_int["agency_index:gender", 4] < 0.05) {
    cat("4. Interaction IS statistically significant\n")
  } else {
    cat("4. Interaction is NOT statistically significant\n")
  }
}

cat("\nIMPLICATIONS:\n")
cat("─────────────\n")
if (larger_for == "WOMEN") {
  cat("→ Agency has STRONGER effect for women (compensatory mechanism)\n")
  cat("→ High-agency women overcome gender-based barriers\n")
  cat("→ Women's entrepreneurship programs should focus on agency-building\n")
  cat("→ Potential selection effect: Only high-agency women attempt TEA\n")
} else {
  cat("→ Agency has STRONGER effect for men\n")
  cat("→ May reflect structural advantages men have in entrepreneurship\n")
  cat("→ Women face additional barriers beyond individual agency\n")
}

cat("\n================================================================================\n")
cat("✓ GENDER HETEROGENEITY ANALYSIS COMPLETE\n")
cat("Output files:\n")
cat("  - output/tables/table8_gender_heterogeneity.csv\n")
cat("  - output/tables/table8_tea_by_agency_gender.csv\n")
cat("================================================================================\n\n")

rm(list = ls())
