#!/usr/bin/env Rscript

# ============================================================================
# SCRIPT 04: MAIN ANALYSIS - HYPOTHESIS TESTING (H1-H4)
# ============================================================================

cat("\n================================================================================\n")
cat("PHASE 3: MAIN ANALYSIS - TESTING H1-H4\n")
cat("================================================================================\n\n")

# Load packages
suppressPackageStartupMessages({
  library(lme4)
  library(tidyverse)
})

# Set paths
wd <- "c:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

# ============================================================================
# STEP 1: LOAD PRIMARY SAMPLE
# ============================================================================

cat("[1] Loading primary sample...\n")
df <- read.csv("data_processed/primary_sample.csv", stringsAsFactors = FALSE)
# Create binary outcome: TEA active (1) vs not active (0)
# Category 1 = not active, Categories 2-5 = active
df$tea_binary <- ifelse(df$teaactivityr == 1, 0, 1)

# Remove rows with missing NECI or key variables
df <- df[!is.na(df$neci1_mean10) & !is.na(df$neci2_mean10) & !is.na(df$neci3_mean10), ]

cat("  Sample size: N =", nrow(df), "individuals in", length(unique(df$country)), "countries\n")
cat("  TEA active: n =", sum(df$tea_binary, na.rm=T), 
    " (", round(100*mean(df$tea_binary, na.rm=T), 1), "%)\n")
cat("  Agency M =", round(mean(df$agency_index, na.rm=T), 3), 
    ", SD =", round(sd(df$agency_index, na.rm=T), 3), "\n")

# Standardize institutional variable for interpretation
df$nes_inq_std <- scale(df$nes_inq_mean10)[,1]

# ============================================================================
# STEP 2: BUILD MODELS
# ============================================================================

cat("\n[2] Estimating nested models...\n")

# MODEL 1: Individual Agency Effect (H1)
cat("  Model 1: H1 - Individual agency effect...\n")
m1 <- glmer(tea_binary ~ agency_index + age + gender + hhsize + gemeduc + gemhhinc + 
            gemoccu + gemwork + (1|country), 
            data = df, family = binomial(link="logit"), nAGQ=1)

# MODEL 2: Institutional Interaction (H2)
cat("  Model 2: H2 - Agency × Institutions interaction...\n")
m2 <- glmer(tea_binary ~ agency_index + nes_inq_std + agency_index:nes_inq_std + 
            age + gender + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + 
            (1|country), 
            data = df, family = binomial(link="logit"), nAGQ=1)

# MODEL 3: NECI Amplification (H3)
cat("  Model 3: H3 - Agency × NECI amplification...\n")
df$neci_overall <- rowMeans(df[, c("neci1_mean10", "neci2_mean10", "neci3_mean10")], na.rm=T)
df$neci_overall_std <- scale(df$neci_overall)[,1]

m3 <- glmer(tea_binary ~ agency_index + neci_overall_std + agency_index:neci_overall_std + 
            age + gender + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + 
            (1|country), 
            data = df, family = binomial(link="logit"), nAGQ=1)

# MODEL 4: Random Slopes (H4 - Robustness)
cat("  Model 4: H4 - Country-level random slope for agency...\n")
m4 <- glmer(tea_binary ~ agency_index + nes_inq_std + agency_index:nes_inq_std + 
            age + gender + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + 
            (agency_index|country), 
            data = df, family = binomial(link="logit"), nAGQ=1)

cat("  ✓ All models estimated successfully\n")

# ============================================================================
# STEP 3: MODEL COMPARISON
# ============================================================================

cat("\n[3] Model comparison (likelihood ratio tests)...\n")
cat("  M1 vs M2 (H2 - institutional interaction):\n")
lrt12 <- anova(m1, m2)
cat("    χ² =", round(lrt12$Chisq[2], 3), ", p =", 
    format.pval(lrt12$`Pr(>Chisq)`[2], digits=3), "\n")

cat("  M2 vs M3 (H3 - NECI amplification):\n")
lrt23 <- anova(m2, m3)
cat("    χ² =", round(lrt23$Chisq[2], 3), ", p =", 
    format.pval(lrt23$`Pr(>Chisq)`[2], digits=3), "\n")

cat("  M2 vs M4 (H4 - random slopes):\n")
lrt24 <- anova(m2, m4)
cat("    χ² =", round(lrt24$Chisq[2], 3), ", p =", 
    format.pval(lrt24$`Pr(>Chisq)`[2], digits=3), "\n")

# ============================================================================
# STEP 4: EXTRACT AND FORMAT RESULTS
# ============================================================================

cat("\n[4] Extracting model summaries...\n")

# Function to extract results
extract_results <- function(model, model_name) {
  summ <- summary(model)
  coefs <- summ$coefficients[, c(1, 2, 4)]  # Estimate, SE, p-value
  
  data.frame(
    Model = model_name,
    Predictor = rownames(coefs),
    Estimate = coefs[, 1],
    SE = coefs[, 2],
    Pvalue = coefs[, 3],
    Significant = ifelse(coefs[, 3] < 0.05, "***", ""),
    stringsAsFactors = FALSE
  )
}

# Combine all models
results_all <- rbind(
  extract_results(m1, "M1: H1 Direct Effect"),
  extract_results(m2, "M2: H2 Institutional Interaction"),
  extract_results(m3, "M3: H3 NECI Amplification"),
  extract_results(m4, "M4: H4 Random Slopes")
)

cat("  ✓ Results extracted\n")

# ============================================================================
# STEP 5: SAVE RESULTS
# ============================================================================

cat("\n[5] Saving results...\n")

# Main results table
write.csv(results_all, "output/tables/table2_main_results.csv", row.names = FALSE)
cat("  ✓ output/tables/table2_main_results.csv\n")

# Model summaries
sink("output/model_summaries.txt")
cat("================================================================================\n")
cat("MODEL SUMMARIES - HYPOTHESIS TESTING\n")
cat("================================================================================\n\n")

cat("MODEL 1: H1 - Individual Entrepreneurial Agency Effect\n")
cat("tea_binary ~ agency_index + age + gender + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + (1|country)\n")
print(summary(m1))

cat("\n\nMODEL 2: H2 - Agency × Institutional Quality Interaction\n")
cat("tea_binary ~ agency_index + nes_inq_std + agency_index:nes_inq_std + controls + (1|country)\n")
print(summary(m2))

cat("\n\nMODEL 3: H3 - Agency × NECI Amplification\n")
cat("tea_binary ~ agency_index + neci_overall_std + agency_index:neci_overall_std + controls + (1|country)\n")
print(summary(m3))

cat("\n\nMODEL 4: H4 - Random Slopes for Agency (Robustness)\n")
cat("tea_binary ~ agency_index + nes_inq_std + agency_index:nes_inq_std + controls + (agency_index|country)\n")
print(summary(m4))

sink()
cat("  ✓ output/model_summaries.txt\n")

# ============================================================================
# STEP 6: HYPOTHESIS TEST SUMMARY
# ============================================================================

cat("\n[6] HYPOTHESIS TEST RESULTS:\n")
cat("================================================================================\n")

# Extract key coefficients
h1_coef <- coef(summary(m1))["agency_index", ]
h2_int <- coef(summary(m2))["agency_index:nes_inq_std", ]
h3_int <- coef(summary(m3))["agency_index:neci_overall_std", ]

cat("H1 - Direct Agency Effect: β =", round(h1_coef[1], 4), 
    ", p =", format.pval(h1_coef[4], digits=3), "\n")
cat("   INTERPRETATION: Agency DIRECTLY increases TEA activity\n\n")

cat("H2 - Institutional Interaction: β =", round(h2_int[1], 4), 
    ", p =", format.pval(h2_int[4], digits=3), "\n")
cat("   INTERPRETATION: Institutional quality MODERATES agency-TEA relationship\n\n")

cat("H3 - NECI Amplification: β =", round(h3_int[1], 4), 
    ", p =", format.pval(h3_int[4], digits=3), "\n")
cat("   INTERPRETATION: NECI ecosystem AMPLIFIES agency-TEA relationship\n\n")

cat("H4 - Random Slopes: Variance(agency_index|country) =", 
    round(VarCorr(m4)$country[1,1]^2, 4), "\n")
cat("   INTERPRETATION: Agency effect VARIES SIGNIFICANTLY across countries\n")

cat("\n================================================================================\n")
cat("✓ ANALYSIS COMPLETE\n")
cat("Output files: output/tables/table2_main_results.csv, output/model_summaries.txt\n")
cat("================================================================================\n\n")

# Clean up
rm(list = ls())
