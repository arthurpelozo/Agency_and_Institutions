#!/usr/bin/env Rscript

# ============================================================================
# SCRIPT 09: SOCIOECONOMIC HETEROGENEITY ANALYSIS
# ============================================================================

cat("\n================================================================================\n")
cat("PHASE 8: SOCIOECONOMIC STRATIFICATION - EDUCATION & INCOME EFFECTS\n")
cat("================================================================================\n\n")

library(lme4)
library(tidyverse)

df <- read.csv("data_processed/primary_sample.csv")
df$tea_binary <- ifelse(df$teaactivityr == 1, 0, 1)
df <- df[!is.na(df$neci1_mean10) & !is.na(df$neci2_mean10) & !is.na(df$neci3_mean10), ]
df$nes_inq_std <- scale(df$nes_inq_mean10)[,1]

# ============================================================================
# PART A: EDUCATION STRATIFICATION
# ============================================================================

cat("[1] Education Stratification Analysis\n")
cat("================================================================================\n")

cat("\nEducation distribution:\n")
print(table(df$gemeduc, useNA = "always"))

# Create education groups
# gemeduc: 1=no education, 2=some secondary, 3=secondary, 4=post-secondary, 5=graduate
df$educ_group <- cut(df$gemeduc, 
                     breaks = c(0, 2, 3, 5),
                     labels = c("Low (No/Some Secondary)", "Medium (Secondary)", "High (Post-sec+)"),
                     include.lowest = TRUE)

cat("\nEducation groups:\n")
print(table(df$educ_group, useNA = "always"))

# Stratified models by education
educ_groups <- na.omit(unique(df$educ_group))
educ_results <- data.frame()

for (educ in educ_groups) {
  cat("\n\nEDUCATION:", as.character(educ), "\n")
  cat("───────────────────────────────────────────\n")
  
  df_educ <- df[df$educ_group == educ, ]
  cat("  N =", nrow(df_educ), "\n")
  
  tryCatch({
    m_educ <- glmer(tea_binary ~ agency_index + nes_inq_std + agency_index:nes_inq_std +
                    age + gender + hhsize + gemhhinc + gemoccu + gemwork + (1|country),
                    data = df_educ, family = binomial, nAGQ = 1)
    
    coef_educ <- coef(summary(m_educ))
    agency_effect <- coef_educ["agency_index", ]
    
    cat("  Agency Effect: β =", round(agency_effect[1], 4), 
        ", SE =", round(agency_effect[2], 4),
        ", p =", format.pval(agency_effect[4], digits = 3), "\n")
    
    educ_results <- rbind(educ_results, data.frame(
      Education = as.character(educ),
      N = nrow(df_educ),
      Agency_Beta = agency_effect[1],
      Agency_SE = agency_effect[2],
      Agency_Pval = agency_effect[4],
      Significant = ifelse(agency_effect[4] < 0.05, "***", "")
    ))
    
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
  })
}

# Test interaction
cat("\n\nTesting Agency × Education interaction...\n")
cat("════════════════════════════════════════════\n")

m_educ_int <- glmer(tea_binary ~ agency_index * gemeduc + nes_inq_std +
                    age + gender + hhsize + gemhhinc + gemoccu + gemwork + (1|country),
                    data = df, family = binomial, nAGQ = 1)

coef_educ_int <- coef(summary(m_educ_int))
if ("agency_index:gemeduc" %in% rownames(coef_educ_int)) {
  int_effect <- coef_educ_int["agency_index:gemeduc", ]
  cat("  Interaction β =", round(int_effect[1], 4), 
      ", p =", format.pval(int_effect[4], digits = 3), "\n")
  if (int_effect[4] < 0.05) {
    cat("  ✓ SIGNIFICANT: Agency effect varies by education\n")
  } else {
    cat("  ✗ NOT significant: Agency effect similar across education levels\n")
  }
}

write.csv(educ_results, "output/tables/table9_education_heterogeneity.csv", row.names = FALSE)

# ============================================================================
# PART B: INCOME STRATIFICATION
# ============================================================================

cat("\n\n[2] Income Stratification Analysis\n")
cat("================================================================================\n")

cat("\nIncome distribution:\n")
print(table(df$gemhhinc, useNA = "always"))

# Create income tertiles
df$income_group <- cut(df$gemhhinc,
                       breaks = c(0, 1, 2, 3),
                       labels = c("Low (Bottom 33%)", "Middle (Middle 33%)", "High (Top 33%)"),
                       include.lowest = TRUE)

cat("\nIncome groups:\n")
print(table(df$income_group, useNA = "always"))

# Stratified models by income
income_groups <- na.omit(unique(df$income_group))
income_results <- data.frame()

for (inc in income_groups) {
  cat("\n\nINCOME:", as.character(inc), "\n")
  cat("───────────────────────────────────────────\n")
  
  df_inc <- df[df$income_group == inc, ]
  cat("  N =", nrow(df_inc), "\n")
  
  tryCatch({
    m_inc <- glmer(tea_binary ~ agency_index + nes_inq_std + agency_index:nes_inq_std +
                   age + gender + hhsize + gemeduc + gemoccu + gemwork + (1|country),
                   data = df_inc, family = binomial, nAGQ = 1)
    
    coef_inc <- coef(summary(m_inc))
    agency_effect <- coef_inc["agency_index", ]
    
    cat("  Agency Effect: β =", round(agency_effect[1], 4), 
        ", SE =", round(agency_effect[2], 4),
        ", p =", format.pval(agency_effect[4], digits = 3), "\n")
    
    income_results <- rbind(income_results, data.frame(
      Income = as.character(inc),
      N = nrow(df_inc),
      Agency_Beta = agency_effect[1],
      Agency_SE = agency_effect[2],
      Agency_Pval = agency_effect[4],
      Significant = ifelse(agency_effect[4] < 0.05, "***", "")
    ))
    
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
  })
}

# Test interaction
cat("\n\nTesting Agency × Income interaction...\n")
cat("═══════════════════════════════════════\n")

m_inc_int <- glmer(tea_binary ~ agency_index * gemhhinc + nes_inq_std +
                   age + gender + hhsize + gemeduc + gemoccu + gemwork + (1|country),
                   data = df, family = binomial, nAGQ = 1)

coef_inc_int <- coef(summary(m_inc_int))
if ("agency_index:gemhhinc" %in% rownames(coef_inc_int)) {
  int_effect <- coef_inc_int["agency_index:gemhhinc", ]
  cat("  Interaction β =", round(int_effect[1], 4), 
      ", p =", format.pval(int_effect[4], digits = 3), "\n")
  if (int_effect[4] < 0.05) {
    cat("  ✓ SIGNIFICANT: Agency effect varies by income\n")
  } else {
    cat("  ✗ NOT significant: Agency effect similar across income levels\n")
  }
}

write.csv(income_results, "output/tables/table9_income_heterogeneity.csv", row.names = FALSE)

# ============================================================================
# SUMMARY & INTERPRETATION
# ============================================================================

cat("\n\n[3] COMPREHENSIVE INTERPRETATION\n")
cat("================================================================================\n")

cat("\nKEY FINDINGS:\n")
cat("─────────────\n")
cat("1. EDUCATION: Agency effects by education level\n")
if (nrow(educ_results) > 0) {
  educ_results_sorted <- educ_results[order(educ_results$Agency_Beta, decreasing = TRUE), ]
  for (i in 1:nrow(educ_results_sorted)) {
    cat(sprintf("   %s: β = %.4f %s\n", 
                educ_results_sorted$Education[i],
                educ_results_sorted$Agency_Beta[i],
                educ_results_sorted$Significant[i]))
  }
}

cat("\n2. INCOME: Agency effects by income level\n")
if (nrow(income_results) > 0) {
  income_results_sorted <- income_results[order(income_results$Agency_Beta, decreasing = TRUE), ]
  for (i in 1:nrow(income_results_sorted)) {
    cat(sprintf("   %s: β = %.4f %s\n", 
                income_results_sorted$Income[i],
                income_results_sorted$Agency_Beta[i],
                income_results_sorted$Significant[i]))
  }
}

cat("\nIMPLICATIONS:\n")
cat("─────────────\n")
cat("→ If agency matters MORE for low-SES: COMPENSATORY mechanism (levels playing field)\n")
cat("→ If agency matters MORE for high-SES: AMPLIFICATION mechanism (rich get richer)\n")
cat("→ If agency matters EQUALLY: UNIVERSAL mechanism (one size fits all)\n")
cat("\n→ Policy: Target agency interventions where effect is STRONGEST\n")
cat("→ If compensatory: Invest in disadvantaged populations (highest ROI)\n")
cat("→ If amplification: Need structural reforms, not just individual interventions\n")

cat("\n================================================================================\n")
cat("✓ SOCIOECONOMIC HETEROGENEITY ANALYSIS COMPLETE\n")
cat("Output files:\n")
cat("  - output/tables/table9_education_heterogeneity.csv\n")
cat("  - output/tables/table9_income_heterogeneity.csv\n")
cat("================================================================================\n\n")

rm(list = ls())
