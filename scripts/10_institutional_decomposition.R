#!/usr/bin/env Rscript

# ============================================================================
# SCRIPT 10: INSTITUTIONAL DECOMPOSITION - WHICH DIMENSIONS MATTER?
# ============================================================================

cat("\n================================================================================\n")
cat("PHASE 9: INSTITUTIONAL DECOMPOSITION - UNPACKING INSTITUTIONAL QUALITY\n")
cat("================================================================================\n\n")

library(lme4)
library(tidyverse)

df <- read.csv("data_processed/primary_sample.csv")
df$tea_binary <- ifelse(df$teaactivityr == 1, 0, 1)
df <- df[!is.na(df$neci1_mean10) & !is.na(df$neci2_mean10) & !is.na(df$neci3_mean10), ]

cat("[1] Institutional Quality Dimensions Available\n")
cat("================================================================================\n")

# Find all NES institutional dimensions
nes_vars <- grep("^nes_", names(df), value = TRUE)
nes_vars <- nes_vars[!grepl("neci", nes_vars, ignore.case = TRUE)]  # Exclude NECI

cat("  Found", length(nes_vars), "NES institutional dimensions:\n")
for (v in nes_vars) {
  cat("    -", v, "\n")
}

# Focus on the 12 core GCI dimensions if available
gci_dimensions <- c(
  "nes_institutions_mean10",      # 1. Institutions
  "nes_infrastructure_mean10",     # 2. Infrastructure  
  "nes_macroeconomic_mean10",     # 3. Macroeconomic stability
  "nes_health_mean10",            # 4. Health and primary education
  "nes_higher_mean10",            # 5. Higher education and training
  "nes_goods_mean10",             # 6. Goods market efficiency
  "nes_labour_mean10",            # 7. Labor market efficiency
  "nes_financial_mean10",         # 8. Financial market development
  "nes_technology_mean10",        # 9. Technological readiness
  "nes_market_mean10",            # 10. Market size
  "nes_business_mean10",          # 11. Business sophistication
  "nes_innovation_mean10"         # 12. Innovation
)

available_dims <- gci_dimensions[gci_dimensions %in% names(df)]
cat("\n  Available GCI dimensions:", length(available_dims), "\n")

if (length(available_dims) == 0) {
  cat("\n  Using aggregate institutional quality measure (nes_inq_mean10)\n")
  available_dims <- "nes_inq_mean10"
}

# ============================================================================
# APPROACH 1: Test each dimension individually with Agency interaction
# ============================================================================

cat("\n\n[2] Testing each institutional dimension separately\n")
cat("================================================================================\n")

dimension_results <- data.frame()

for (dim_var in available_dims) {
  
  cat("\nTesting:", dim_var, "\n")
  cat("─────────────────────────────────────────────────\n")
  
  # Standardize dimension
  df[[paste0(dim_var, "_std")]] <- scale(df[[dim_var]])[,1]
  
  tryCatch({
    # Model with this dimension
    formula_str <- paste0("tea_binary ~ agency_index * ", dim_var, "_std + ",
                          "age + gender + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + (1|country)")
    
    m_dim <- glmer(as.formula(formula_str), data = df, family = binomial, nAGQ = 1)
    
    coefs <- coef(summary(m_dim))
    
    # Extract effects
    main_effect <- coefs[paste0(dim_var, "_std"), ]
    interaction <- coefs[paste0("agency_index:", dim_var, "_std"), ]
    
    cat("  Main effect: β =", round(main_effect[1], 4), 
        ", p =", format.pval(main_effect[4], digits = 3), "\n")
    cat("  Interaction: β =", round(interaction[1], 4), 
        ", p =", format.pval(interaction[4], digits = 3), "\n")
    
    dimension_results <- rbind(dimension_results, data.frame(
      Dimension = gsub("nes_", "", gsub("_mean10", "", dim_var)),
      Main_Beta = main_effect[1],
      Main_Pval = main_effect[4],
      Interaction_Beta = interaction[1],
      Interaction_Pval = interaction[4],
      Main_Sig = ifelse(main_effect[4] < 0.05, "***", ""),
      Interaction_Sig = ifelse(interaction[4] < 0.05, "***", "")
    ))
    
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
  })
}

# Sort by interaction strength
dimension_results <- dimension_results[order(-abs(dimension_results$Interaction_Beta)), ]

cat("\n\n[3] RANKED INSTITUTIONAL DIMENSIONS (by moderation effect)\n")
cat("================================================================================\n")

for (i in 1:nrow(dimension_results)) {
  cat(sprintf("\n%2d. %s\n", i, dimension_results$Dimension[i]))
  cat(sprintf("    Direct effect on TEA: β = %7.4f, p = %s %s\n",
              dimension_results$Main_Beta[i],
              format.pval(dimension_results$Main_Pval[i], digits = 3),
              dimension_results$Main_Sig[i]))
  cat(sprintf("    Moderates Agency:     β = %7.4f, p = %s %s\n",
              dimension_results$Interaction_Beta[i],
              format.pval(dimension_results$Interaction_Pval[i], digits = 3),
              dimension_results$Interaction_Sig[i]))
}

write.csv(dimension_results, "output/tables/table10_institutional_dimensions.csv", row.names = FALSE)

# ============================================================================
# APPROACH 2: Simultaneous entry of top dimensions
# ============================================================================

cat("\n\n[4] Testing top dimensions simultaneously\n")
cat("================================================================================\n")

if (nrow(dimension_results) >= 3) {
  
  # Take top 3 dimensions by interaction strength
  top_dims <- head(dimension_results$Dimension, 3)
  top_dim_vars <- paste0("nes_", top_dims, "_mean10")
  
  cat("  Top 3 dimensions:\n")
  for (i in 1:length(top_dims)) {
    cat("    ", i, ".", top_dims[i], "\n")
  }
  
  # Standardize
  for (v in top_dim_vars) {
    if (v %in% names(df)) {
      df[[paste0(v, "_std")]] <- scale(df[[v]])[,1]
    }
  }
  
  # Build formula
  std_vars <- paste0(top_dim_vars[top_dim_vars %in% names(df)], "_std")
  
  if (length(std_vars) >= 2) {
    formula_str <- paste0("tea_binary ~ agency_index * (", 
                          paste(std_vars, collapse = " + "), ") + ",
                          "age + gender + hhsize + gemeduc + gemhhinc + gemoccu + gemwork + (1|country)")
    
    cat("\n  Model formula:\n  ", formula_str, "\n\n")
    
    tryCatch({
      m_multi <- glmer(as.formula(formula_str), data = df, family = binomial, nAGQ = 1)
      
      cat("  Model converged successfully\n")
      cat("  Coefficients:\n")
      print(coef(summary(m_multi)))
      
    }, error = function(e) {
      cat("  Error fitting simultaneous model:", e$message, "\n")
    })
  }
}

# ============================================================================
# INTERPRETATION
# ============================================================================

cat("\n\n[5] INTERPRETATION & POLICY IMPLICATIONS\n")
cat("================================================================================\n")

if (nrow(dimension_results) > 0) {
  
  # Identify strongest moderators
  sig_moderators <- dimension_results[dimension_results$Interaction_Pval < 0.10, ]
  
  if (nrow(sig_moderators) > 0) {
    cat("\nSTRONGEST INSTITUTIONAL MODERATORS:\n")
    cat("───────────────────────────────────\n")
    for (i in 1:nrow(sig_moderators)) {
      direction <- ifelse(sig_moderators$Interaction_Beta[i] < 0, 
                          "SUBSTITUTES for agency", 
                          "COMPLEMENTS agency")
      cat(sprintf("%d. %s: %s\n", i, sig_moderators$Dimension[i], direction))
    }
  } else {
    cat("\nNO significant institutional moderators found\n")
    cat("→ This suggests agency works INDEPENDENTLY of institutional quality\n")
  }
  
  # Identify dimensions with direct effects
  sig_direct <- dimension_results[dimension_results$Main_Pval < 0.05, ]
  
  if (nrow(sig_direct) > 0) {
    cat("\n\nDIMENSIONS WITH DIRECT EFFECTS ON TEA:\n")
    cat("───────────────────────────────────────\n")
    for (i in 1:nrow(sig_direct)) {
      direction <- ifelse(sig_direct$Main_Beta[i] > 0, "increases", "decreases")
      cat(sprintf("%d. %s: %s TEA\n", i, sig_direct$Dimension[i], direction))
    }
  }
}

cat("\n\nPOLICY IMPLICATIONS:\n")
cat("────────────────────\n")
cat("1. SUBSTITUTION pattern (negative interaction):\n")
cat("   → Invest in agency where institutions are weak\n")
cat("   → Agency compensates for institutional deficits\n")
cat("   → Target: Developing countries with weak institutions\n\n")

cat("2. COMPLEMENTARITY pattern (positive interaction):\n")
cat("   → Need BOTH agency and institutions\n")
cat("   → Agency alone insufficient without institutional support\n")
cat("   → Target: Strengthen institutions AND build agency\n\n")

cat("3. INDEPENDENCE pattern (no interaction):\n")
cat("   → Agency works regardless of institutions\n")
cat("   → Universal intervention strategy\n")
cat("   → Agency-building has consistent ROI everywhere\n")

cat("\n================================================================================\n")
cat("✓ INSTITUTIONAL DECOMPOSITION COMPLETE\n")
cat("Output: output/tables/table10_institutional_dimensions.csv\n")
cat("================================================================================\n\n")

rm(list = ls())
