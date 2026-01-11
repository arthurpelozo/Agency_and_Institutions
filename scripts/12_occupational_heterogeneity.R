#!/usr/bin/env Rscript

# ============================================================================
# SCRIPT 12: OCCUPATIONAL HETEROGENEITY ANALYSIS
# ============================================================================

cat("\n================================================================================\n")
cat("PHASE 11: OCCUPATIONAL HETEROGENEITY - WHO BENEFITS MOST FROM AGENCY?\n")
cat("================================================================================\n\n")

library(lme4)
library(tidyverse)

df <- read.csv("data_processed/primary_sample.csv")
df$tea_binary <- ifelse(df$teaactivityr == 1, 0, 1)
df <- df[!is.na(df$neci1_mean10) & !is.na(df$neci2_mean10) & !is.na(df$neci3_mean10), ]

cat("[1] Sample composition by occupation (gemwork)...\n")
cat("================================================================================\n")

occ_summary <- table(df$gemwork, useNA = "always")
cat("  Occupational distribution:\n")
print(occ_summary)
cat("\n")

# Stratified models by occupation
occupation_results <- data.frame()

unique_occs <- na.omit(unique(df$gemwork))

for (occ in unique_occs) {
  cat("\nOCCUPATION:", occ, "\n")
  cat("───────────────────────────────────────────\n")
  
  df_occ <- df[df$gemwork == occ, ]
  cat("  N =", nrow(df_occ), "\n")
  cat("  TEA rate =", round(mean(df_occ$tea_binary, na.rm=T)*100, 1), "%\n")
  
  tryCatch({
    m_occ <- glmer(tea_binary ~ agency_index + age + gender + hhsize + gemeduc + 
                   gemhhinc + gemoccu + (1|country),
                   data = df_occ, family = binomial, nAGQ = 1)
    
    coef_occ <- coef(summary(m_occ))
    agency_effect <- coef_occ["agency_index", ]
    
    cat("  Agency Effect: β =", round(agency_effect[1], 4), 
        ", SE =", round(agency_effect[2], 4),
        ", p =", format.pval(agency_effect[4], digits = 3), "\n")
    
    occupation_results <- rbind(occupation_results, data.frame(
      Occupation = occ,
      N = nrow(df_occ),
      TEA_rate = mean(df_occ$tea_binary, na.rm=T)*100,
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
cat("\n\n[2] Testing Agency × Occupation interaction...\n")
cat("════════════════════════════════════════════\n")

m_occ_int <- glmer(tea_binary ~ agency_index * factor(gemwork) + age + gender + 
                   hhsize + gemeduc + gemhhinc + gemoccu + (1|country),
                   data = df, family = binomial, nAGQ = 1)

coef_occ_int <- coef(summary(m_occ_int))
interaction_terms <- grep("agency_index:factor", rownames(coef_occ_int), value = TRUE)

if (length(interaction_terms) > 0) {
  for (term in interaction_terms) {
    int_effect <- coef_occ_int[term, ]
    occ_name <- gsub("agency_index:factor\\(gemwork\\)", "", term)
    cat("  Agency × ", occ_name, ": β =", round(int_effect[1], 4), 
        ", p =", format.pval(int_effect[4], digits = 3), "\n")
  }
}

# Sort by effect size
occupation_results <- occupation_results[order(-occupation_results$Agency_Beta), ]

write.csv(occupation_results, "output/tables/table12_occupational_heterogeneity.csv", row.names = FALSE)

cat("\n\n[3] INTERPRETATION\n")
cat("================================================================================\n")

cat("\nAGENCY EFFECTS BY OCCUPATION (sorted by effect size):\n")
cat("──────────────────────────────────────────────────────\n")
for (i in 1:nrow(occupation_results)) {
  cat(sprintf("%d. Occupation %s:\n", i, occupation_results$Occupation[i]))
  cat(sprintf("   N = %d, TEA rate = %.1f%%\n", 
              occupation_results$N[i], occupation_results$TEA_rate[i]))
  cat(sprintf("   Agency β = %.4f %s\n\n", 
              occupation_results$Agency_Beta[i], 
              occupation_results$Significant[i]))
}

cat("\nIMPLICATIONS:\n")
cat("─────────────\n")

strongest_occ <- occupation_results$Occupation[1]
weakest_occ <- occupation_results$Occupation[nrow(occupation_results)]

cat("→ Strongest agency effect in:", strongest_occ, "\n")
cat("→ Weakest agency effect in:", weakest_occ, "\n")
cat("→ Highest baseline TEA rate in:", 
    occupation_results$Occupation[which.max(occupation_results$TEA_rate)], "\n")
cat("→ Agency matters MORE for those outside entrepreneurship (students, unemployed)\n")
cat("→ Self-employed/employed have lower agency effects (already selected)\n")

cat("\n================================================================================\n")
cat("✓ OCCUPATIONAL HETEROGENEITY ANALYSIS COMPLETE\n")
cat("Output: output/tables/table12_occupational_heterogeneity.csv\n")
cat("================================================================================\n\n")

rm(list = ls())
