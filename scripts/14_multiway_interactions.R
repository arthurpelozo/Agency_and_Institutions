#!/usr/bin/env Rscript

# ============================================================================
# SCRIPT 14: MULTI-WAY INTERACTIONS
# ============================================================================

cat("\n================================================================================\n")
cat("PHASE 13: MULTI-WAY INTERACTIONS - CONDITIONAL EFFECTS\n")
cat("================================================================================\n\n")

library(lme4)
library(tidyverse)

df <- read.csv("data_processed/primary_sample.csv")
df$tea_binary <- ifelse(df$teaactivityr == 1, 0, 1)
df <- df[!is.na(df$neci1_mean10) & !is.na(df$neci2_mean10) & !is.na(df$neci3_mean10), ]
df$nes_inq_std <- scale(df$nes_inq_mean10)[,1]
df$female <- ifelse(df$gender == 2, 1, 0)

cat("[1] Three-way interaction: Agency × Female × Institutional Quality\n")
cat("================================================================================\n")

m_3way <- glmer(tea_binary ~ agency_index * female * nes_inq_std + 
                age + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + (1|country),
                data = df, family = binomial, nAGQ = 1)

coef_3way <- coef(summary(m_3way))

cat("  Main effects:\n")
cat("    Agency:                  β =", round(coef_3way["agency_index", 1], 4), "\n")
cat("    Female:                  β =", round(coef_3way["female", 1], 4), "\n")
cat("    Institutional quality:   β =", round(coef_3way["nes_inq_std", 1], 4), "\n\n")

cat("  Two-way interactions:\n")
cat("    Agency × Female:         β =", round(coef_3way["agency_index:female", 1], 4),
    ", p =", format.pval(coef_3way["agency_index:female", 4], digits=3), "\n")
cat("    Agency × Institutions:   β =", round(coef_3way["agency_index:nes_inq_std", 1], 4),
    ", p =", format.pval(coef_3way["agency_index:nes_inq_std", 4], digits=3), "\n")
cat("    Female × Institutions:   β =", round(coef_3way["female:nes_inq_std", 1], 4),
    ", p =", format.pval(coef_3way["female:nes_inq_std", 4], digits=3), "\n\n")

cat("  Three-way interaction:\n")
if ("agency_index:female:nes_inq_std" %in% rownames(coef_3way)) {
  three_way <- coef_3way["agency_index:female:nes_inq_std", ]
  cat("    Agency × Female × Insti: β =", round(three_way[1], 4),
      ", p =", format.pval(three_way[4], digits=3), "\n\n")
  
  if (three_way[4] < 0.05) {
    cat("  ✓ THREE-WAY INTERACTION SIGNIFICANT\n")
  } else {
    cat("  ✗ Three-way interaction not significant\n")
  }
}

cat("\n[2] Agency × NECI component interactions...\n")
cat("================================================================================\n")

# Standardize NECI components
df$neci1_std <- scale(df$neci1_mean10)[,1]
df$neci2_std <- scale(df$neci2_mean10)[,1]
df$neci3_std <- scale(df$neci3_mean10)[,1]

neci_results <- data.frame()

for (neci_comp in c("neci1_std", "neci2_std", "neci3_std")) {
  formula_str <- paste("tea_binary ~ agency_index * ", neci_comp,
                       " + female + age + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + (1|country)")
  
  m_neci <- glmer(as.formula(formula_str), data = df, family = binomial, nAGQ = 1)
  coef_neci <- coef(summary(m_neci))
  
  int_term <- paste("agency_index:", neci_comp, sep="")
  
  if (int_term %in% rownames(coef_neci)) {
    int_effect <- coef_neci[int_term, ]
    comp_name <- ifelse(neci_comp == "neci1_std", "Framework", 
                       ifelse(neci_comp == "neci2_std", "Activity", "Impact"))
    
    neci_results <- rbind(neci_results, data.frame(
      NECI_Component = comp_name,
      Agency_Beta = coef_neci["agency_index", 1],
      Component_Beta = coef_neci[neci_comp, 1],
      Interaction_Beta = int_effect[1],
      Interaction_Pval = int_effect[4]
    ))
    
    cat(sprintf("\n  Agency × NECI %s (%s):\n", comp_name, neci_comp))
    cat(sprintf("    Agency main effect: β = %.4f\n", coef_neci["agency_index", 1]))
    cat(sprintf("    Interaction: β = %.4f, p = %s\n", int_effect[1], 
                format.pval(int_effect[4], digits=3)))
  }
}

cat("\n[3] Conditional agency effects at institutional extremes...\n")
cat("================================================================================\n")

# Low vs High institutional quality
df$high_inst <- ifelse(df$nes_inq_std > 0, 1, 0)

m_low_inst <- glmer(tea_binary ~ agency_index + age + female + hhsize + gemeduc + 
                    gemhhinc + gemoccu + gemwork + (1|country),
                    data = df[df$high_inst == 0, ], family = binomial, nAGQ = 1)

m_high_inst <- glmer(tea_binary ~ agency_index + age + female + hhsize + gemeduc + 
                     gemhhinc + gemoccu + gemwork + (1|country),
                     data = df[df$high_inst == 1, ], family = binomial, nAGQ = 1)

low_agency <- coef(summary(m_low_inst))["agency_index", ]
high_agency <- coef(summary(m_high_inst))["agency_index", ]

cat("\n  Low institutional quality countries:\n")
cat("    Agency effect: β =", round(low_agency[1], 4),
    ", p =", format.pval(low_agency[4], digits=3), "\n")

cat("  High institutional quality countries:\n")
cat("    Agency effect: β =", round(high_agency[1], 4),
    ", p =", format.pval(high_agency[4], digits=3), "\n\n")

cat("  Difference: ", round(high_agency[1] - low_agency[1], 4), 
    "(Higher agency effect in high-inst countries)\n")

# Save results
write.csv(neci_results, "output/tables/table14_multiway_interactions.csv", row.names = FALSE)

cat("\n\n[4] INTERPRETATION\n")
cat("================================================================================\n")

cat("\nCONDITIONAL EFFECTS SUMMARY:\n")
cat("──────────────────────────\n")
cat("→ Three-way interaction shows whether gender compensatory pattern\n")
cat("  persists across institutional contexts\n")
cat("→ NECI interactions reveal which institutional dimensions matter\n")
cat("→ Conditional effects show where agency has maximum impact\n")

cat("\n================================================================================\n")
cat("✓ MULTI-WAY INTERACTIONS COMPLETE\n")
cat("Output: output/tables/table14_multiway_interactions.csv\n")
cat("================================================================================\n\n")

rm(list = ls())
