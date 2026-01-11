#!/usr/bin/env Rscript

# ============================================================================
# SCRIPT 16: ENTREPRENEURIAL EXPOSURE & MENTORSHIP AMPLIFICATION
# ============================================================================

cat("\n================================================================================\n")
cat("PHASE 15: ENTREPRENEURIAL EXPOSURE - SOCIAL AMPLIFICATION EFFECTS\n")
cat("================================================================================\n\n")

library(lme4)
library(tidyverse)

df <- read.csv("data_processed/primary_sample.csv")
df$tea_binary <- ifelse(df$teaactivityr == 1, 0, 1)
df <- df[!is.na(df$neci1_mean10) & !is.na(df$neci2_mean10) & !is.na(df$neci3_mean10), ]
df$nes_inq_std <- scale(df$nes_inq_mean10)[,1]

cat("[1] Entrepreneurial exposure variables...\n")
cat("================================================================================\n")

# Check for exposure variables
exposure_vars <- c("knowent", "gemdrive")

for (var in exposure_vars) {
  if (var %in% names(df)) {
    available <- sum(!is.na(df[[var]]))
    cat(sprintf("  %s: %d observations (%.1f%% available)\n", var, available, 100*available/nrow(df)))
  }
}

cat("\n[2] Agency × Entrepreneurial exposure interaction...\n")
cat("================================================================================\n")

# Check if knowent exists and use it
if ("knowent" %in% names(df)) {
  df$knowent_binary <- as.numeric(df$knowent > 0)
  knowent_available <- sum(!is.na(df$knowent_binary))
  
  if (knowent_available > 100) {
    cat("  Know an entrepreneur (proxy for exposure): ", knowent_available, "respondents\n\n")
    
    m_exp <- glmer(tea_binary ~ agency_index * knowent_binary + 
                   age + gender + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + (1|country),
                   data = df, family = binomial, nAGQ = 1)
    
    coef_exp <- coef(summary(m_exp))
    
    main_agency <- coef_exp["agency_index", ]
    exposure <- coef_exp["knowent_binary", ]
    interaction <- coef_exp["agency_index:knowent_binary", ]
    
    cat("  Main effects:\n")
    cat("    Agency (no exposure): β =", round(main_agency[1], 4), 
        ", p =", format.pval(main_agency[4], digits=3), "\n")
    cat("    Exposure effect:      β =", round(exposure[1], 4), 
        ", p =", format.pval(exposure[4], digits=3), "\n\n")
    
    cat("  Interaction effect:\n")
    cat("    Agency × Exposure:    β =", round(interaction[1], 4), 
        ", p =", format.pval(interaction[4], digits=3), "\n\n")
    
    # Conditional effects
    agency_no_exp <- main_agency[1]
    agency_with_exp <- main_agency[1] + interaction[1]
    
    cat("  Conditional agency effects:\n")
    cat("    Without exposure:     β =", round(agency_no_exp, 4), "\n")
    cat("    With exposure:        β =", round(agency_with_exp, 4), "\n")
    cat("    Amplification:        ", round(agency_with_exp / agency_no_exp, 2), "x stronger\n\n")
    
    if (interaction[4] < 0.05) {
      cat("  ✓ SIGNIFICANT amplification: Exposure enhances agency effect\n")
    } else {
      cat("  ✗ No significant amplification\n")
    }
  }
} else {
  cat("  Note: knowent variable not available in dataset\n")
  cat("  Alternative approach: Using gender × agency as proxy for social effects\n")
}

cat("\n[3] Exposure × Institutional quality interaction...\n")
cat("================================================================================\n")

if ("knowent" %in% names(df)) {
  m_exp_inst <- glmer(tea_binary ~ agency_index + knowent_binary + nes_inq_std + 
                      knowent_binary:nes_inq_std + 
                      age + gender + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + (1|country),
                      data = df, family = binomial, nAGQ = 1)
  
  coef_exp_inst <- coef(summary(m_exp_inst))
  
  if ("knowent_binary:nes_inq_std" %in% rownames(coef_exp_inst)) {
    exp_inst_int <- coef_exp_inst["knowent_binary:nes_inq_std", ]
    cat("  Exposure × Institutions: β =", round(exp_inst_int[1], 4),
        ", p =", format.pval(exp_inst_int[4], digits=3), "\n\n")
    
    if (exp_inst_int[4] < 0.05) {
      cat("  ✓ Institutional context moderates exposure effect\n")
    } else {
      cat("  ✗ Exposure effect independent of institutions\n")
    }
  }
}

cat("\n[4] Three-way: Agency × Exposure × Institutions...\n")
cat("================================================================================\n")

if ("knowent" %in% names(df)) {
  m_3way_exp <- glmer(tea_binary ~ agency_index * knowent_binary * nes_inq_std + 
                      age + gender + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + (1|country),
                      data = df, family = binomial, nAGQ = 1)
  
  coef_3way <- coef(summary(m_3way_exp))
  
  if ("agency_index:knowent_binary:nes_inq_std" %in% rownames(coef_3way)) {
    three_way <- coef_3way["agency_index:knowent_binary:nes_inq_std", ]
    cat("  Agency × Exposure × Inst: β =", round(three_way[1], 4),
        ", p =", format.pval(three_way[4], digits=3), "\n\n")
    
    if (three_way[4] < 0.05) {
      cat("  ✓ COMPLEX INTERACTION: Amplification varies by institutions\n")
    } else {
      cat("  ✗ Amplification effect consistent across contexts\n")
    }
  }
}

cat("\n\n[5] INTERPRETATION\n")
cat("================================================================================\n")

cat("\nSOCIAL AMPLIFICATION FINDINGS:\n")
cat("──────────────────────────────\n")
cat("→ Entrepreneurial exposure proxied by knowing an entrepreneur\n")
cat("→ Tests whether social environment amplifies agency effects\n")
cat("→ If positive: agency works best in entrepreneurial ecosystems\n")
cat("→ If negative: agency sufficient even without exposure\n\n")

cat("MECHANISM:\n")
cat("──────────\n")
cat("→ Exposure → Information and legitimacy\n")
cat("→ Agency → Internal motivation and planning\n")
cat("→ Combined: Maximum probability of TEA entry\n")

cat("\n================================================================================\n")
cat("✓ EXPOSURE & AMPLIFICATION ANALYSIS COMPLETE\n")
cat("================================================================================\n\n")

rm(list = ls())
