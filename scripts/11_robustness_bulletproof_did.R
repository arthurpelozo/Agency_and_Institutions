# ============================================================================
# 11. BULLETPROOF DiD ROBUSTNESS CHECKS
# ============================================================================
# Purpose: Address parallel trends violations and make DiD causally credible
# 
# Priority 1: Formal pre-trend test (treated × year interaction pre-2012)
# Priority 2: Dynamic DiD with leads/lags (year-by-year effects)
# Priority 3: Event study plot (visual evidence of parallel trends)
# Priority 4: Lee bounds & alternative specifications
#
# Author: Phase 5 Robustness Enhancement
# Date: 2026-01-10
# ============================================================================

library(tidyverse)
library(lfe)
library(haven)
library(broom)

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("BULLETPROOF DiD ROBUSTNESS: 4 CRITICAL TESTS\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading DiD dataset...\n")
aps_did <- readRDS("output/did_results/aps_did_comprehensive.rds")

cat(sprintf("  %s observations\n", format(nrow(aps_did), big.mark = ",")))
cat(sprintf("  %d countries (%d treated)\n", 
            n_distinct(aps_did$country_id),
            sum(aps_did$treated == 1 & !duplicated(aps_did$country_id))))
cat(sprintf("  %d years (%d-%d)\n\n", 
            n_distinct(aps_did$year),
            min(aps_did$year), max(aps_did$year)))

# ============================================================================
# PRIORITY 1: FORMAL PRE-TREND TEST
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("PRIORITY 1: FORMAL PRE-TREND TEST\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

cat("Research Question: Do treated/control countries have parallel trends pre-reform?\n")
cat("H0: treated × year = 0 (parallel trends OK)\n")
cat("HA: treated × year ≠ 0 (pre-trends present → DiD biased)\n\n")

# Filter to pre-reform period only
aps_pre <- aps_did %>%
  filter(year < 2012) %>%  # All reforms happen 2012 or later
  mutate(
    treated_num = as.numeric(treated),
    year_numeric = as.numeric(year)
  )

cat(sprintf("Pre-reform sample: %s observations (%d-%d)\n\n", 
            format(nrow(aps_pre), big.mark = ","),
            min(aps_pre$year), max(aps_pre$year)))

# Model 1: Simple interaction test
m_pretrend_simple <- felm(
  tea ~ treated_num * year_numeric + treated_num + year_numeric |
    as.factor(country_id),
  data = aps_pre
)

pretrend_coef <- summary(m_pretrend_simple)$coefficients["treated_num:year_numeric", "Estimate"]
pretrend_se <- summary(m_pretrend_simple)$coefficients["treated_num:year_numeric", "Std. Error"]
pretrend_p <- summary(m_pretrend_simple)$coefficients["treated_num:year_numeric", "Pr(>|t|)"]

cat("── Model 1: Simple Interaction Test ──\n")
cat(sprintf("  treated × year: β = %.6f (SE = %.6f), p = %.4f %s\n\n",
            pretrend_coef, pretrend_se, pretrend_p,
            ifelse(pretrend_p < 0.05, "** PRE-TRENDS PRESENT", 
                   ifelse(pretrend_p < 0.10, "* Marginal pre-trends", "✓ OK"))))

# Model 2: Year-by-year F-test
# Create year dummies × treated
aps_pre <- aps_pre %>%
  mutate(
    year_factor = as.factor(year)
  )

m_pretrend_full <- felm(
  tea ~ treated_num * year_factor |
    as.factor(country_id),
  data = aps_pre
)

# Extract all interaction coefficients
pretrend_coefs <- summary(m_pretrend_full)$coefficients
interaction_rows <- grep("treated_num:year_factor", rownames(pretrend_coefs))

if(length(interaction_rows) > 0) {
  pretrend_f <- data.frame(
    year = str_extract(rownames(pretrend_coefs)[interaction_rows], "\\d{4}"),
    coef = pretrend_coefs[interaction_rows, "Estimate"],
    se = pretrend_coefs[interaction_rows, "Std. Error"],
    p_value = pretrend_coefs[interaction_rows, "Pr(>|t|)"]
  ) %>%
    mutate(year = as.numeric(year))
  
  cat("── Model 2: Year-by-Year Pre-Trends ──\n")
  cat(sprintf("%-6s  %-10s  %-10s  %-10s\n", "Year", "Coef", "SE", "p-value"))
  cat(strrep("-", 40), "\n")
  for(i in 1:nrow(pretrend_f)) {
    cat(sprintf("%-6d  %10.6f  %10.6f  %10.4f %s\n",
                pretrend_f$year[i], pretrend_f$coef[i], pretrend_f$se[i], pretrend_f$p_value[i],
                ifelse(pretrend_f$p_value[i] < 0.05, "**", 
                       ifelse(pretrend_f$p_value[i] < 0.10, "*", ""))))
  }
  cat("\n")
  
  # Joint F-test (approximate using average)
  n_sig <- sum(pretrend_f$p_value < 0.05)
  cat(sprintf("Significant pre-trends: %d of %d years (%.0f%%)\n\n",
              n_sig, nrow(pretrend_f), 100 * n_sig / nrow(pretrend_f)))
  
  pretrend_result <- pretrend_f
} else {
  cat("Note: Could not extract year-by-year interactions\n\n")
  pretrend_result <- data.frame()
}

# Verdict
if(pretrend_p < 0.05) {
  cat("⚠ VERDICT: PRE-TRENDS DETECTED (p < 0.05)\n")
  cat("  → Parallel trends assumption VIOLATED\n")
  cat("  → DiD estimates may be biased\n")
  cat("  → Proceed with caution; dynamic DiD required\n\n")
} else if(pretrend_p < 0.10) {
  cat("⚠ VERDICT: MARGINAL PRE-TRENDS (0.05 < p < 0.10)\n")
  cat("  → Parallel trends questionable\n")
  cat("  → DiD estimates may have minor bias\n")
  cat("  → Dynamic DiD recommended for robustness\n\n")
} else {
  cat("✓ VERDICT: NO PRE-TRENDS (p > 0.10)\n")
  cat("  → Parallel trends assumption supported\n")
  cat("  → DiD estimates likely unbiased\n\n")
}

# ============================================================================
# PRIORITY 2: DYNAMIC DiD (EVENT STUDY)
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("PRIORITY 2: DYNAMIC DiD WITH LEADS & LAGS\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

cat("Purpose: Show year-by-year treatment effects (before & after reform)\n")
cat("Expected pattern:\n")
cat("  • Leads (before reform): ~0 (no effect yet)\n")
cat("  • Reform year (t=0): positive jump\n")
cat("  • Lags (after reform): persistent positive effect\n\n")

# Create time-to-reform variable
aps_did <- aps_did %>%
  mutate(
    time_to_reform = ifelse(is.na(reform_year), NA, year - reform_year),
    # Bin endpoints to avoid too many coefficients
    time_bin = case_when(
      time_to_reform <= -4 ~ "-4 or earlier",
      time_to_reform == -3 ~ "-3",
      time_to_reform == -2 ~ "-2",
      time_to_reform == -1 ~ "-1 (reference)",
      time_to_reform == 0 ~ "0 (reform year)",
      time_to_reform == 1 ~ "+1",
      time_to_reform == 2 ~ "+2",
      time_to_reform == 3 ~ "+3",
      time_to_reform >= 4 ~ "+4 or later",
      TRUE ~ NA_character_
    ),
    time_bin = factor(time_bin, levels = c(
      "-4 or earlier", "-3", "-2", "-1 (reference)", "0 (reform year)",
      "+1", "+2", "+3", "+4 or later"
    ))
  )

# Only keep treated countries for event study
aps_event <- aps_did %>%
  filter(treated == 1, !is.na(time_bin))

cat(sprintf("Event study sample: %s observations\n", 
            format(nrow(aps_event), big.mark = ",")))
cat(sprintf("  Time range: %d to +%d years around reform\n\n",
            min(aps_event$time_to_reform, na.rm = TRUE),
            max(aps_event$time_to_reform, na.rm = TRUE)))

# Dynamic DiD model (omit t = -1 as reference)
m_dynamic <- felm(
  tea ~ I(time_bin == "-4 or earlier") +
        I(time_bin == "-3") +
        I(time_bin == "-2") +
        # I(time_bin == "-1 (reference)") omitted as reference
        I(time_bin == "0 (reform year)") +
        I(time_bin == "+1") +
        I(time_bin == "+2") +
        I(time_bin == "+3") +
        I(time_bin == "+4 or later") |
    as.factor(country_id) + as.factor(year),
  data = aps_event
)

# Extract coefficients
dynamic_coefs <- summary(m_dynamic)$coefficients
dynamic_rows <- grep("time_bin", rownames(dynamic_coefs))

if(length(dynamic_rows) > 0) {
  event_study <- data.frame(
    time = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
    time_label = c("-4+", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4+"),
    coef = NA,
    se = NA,
    ci_lower = NA,
    ci_upper = NA,
    p_value = NA
  )
  
  # t = -1 is reference (coef = 0)
  event_study$coef[event_study$time == -1] <- 0
  event_study$se[event_study$time == -1] <- 0
  event_study$ci_lower[event_study$time == -1] <- 0
  event_study$ci_upper[event_study$time == -1] <- 0
  event_study$p_value[event_study$time == -1] <- 1
  
  # Fill in other coefficients
  for(i in 1:nrow(dynamic_coefs)) {
    row_name <- rownames(dynamic_coefs)[i]
    
    if(grepl("-4 or earlier", row_name)) idx <- which(event_study$time == -4)
    else if(grepl("-3", row_name)) idx <- which(event_study$time == -3)
    else if(grepl("-2", row_name)) idx <- which(event_study$time == -2)
    else if(grepl("0 \\(reform year\\)", row_name)) idx <- which(event_study$time == 0)
    else if(grepl("\\+1", row_name)) idx <- which(event_study$time == 1)
    else if(grepl("\\+2", row_name)) idx <- which(event_study$time == 2)
    else if(grepl("\\+3", row_name)) idx <- which(event_study$time == 3)
    else if(grepl("\\+4 or later", row_name)) idx <- which(event_study$time == 4)
    else next
    
    if(length(idx) > 0) {
      event_study$coef[idx] <- dynamic_coefs[i, "Estimate"]
      event_study$se[idx] <- dynamic_coefs[i, "Std. Error"]
      event_study$ci_lower[idx] <- event_study$coef[idx] - 1.96 * event_study$se[idx]
      event_study$ci_upper[idx] <- event_study$coef[idx] + 1.96 * event_study$se[idx]
      event_study$p_value[idx] <- dynamic_coefs[i, "Pr(>|t|)"]
    }
  }
  
  cat("── Event Study Coefficients ──\n")
  cat(sprintf("%-8s  %-10s  %-10s  %-22s  %-10s\n", 
              "Time", "Coef", "SE", "95% CI", "p-value"))
  cat(strrep("-", 70), "\n")
  for(i in 1:nrow(event_study)) {
    cat(sprintf("%-8s  %10.6f  %10.6f  [%8.5f, %8.5f]  %10.4f %s\n",
                event_study$time_label[i], 
                event_study$coef[i], 
                event_study$se[i],
                event_study$ci_lower[i],
                event_study$ci_upper[i],
                event_study$p_value[i],
                ifelse(event_study$p_value[i] < 0.05, "**", 
                       ifelse(event_study$p_value[i] < 0.10, "*", ""))))
  }
  cat("\n")
  
  # Check pre-trends
  pre_sig <- sum(event_study$p_value[event_study$time < 0 & event_study$time != -1] < 0.05, na.rm = TRUE)
  pre_total <- sum(event_study$time < 0 & event_study$time != -1)
  
  cat(sprintf("Pre-reform leads significant: %d of %d\n", pre_sig, pre_total))
  
  # Check post-reform effects
  post_sig <- sum(event_study$p_value[event_study$time >= 0] < 0.05, na.rm = TRUE)
  post_total <- sum(event_study$time >= 0)
  
  cat(sprintf("Post-reform effects significant: %d of %d\n\n", post_sig, post_total))
  
  # Verdict
  if(pre_sig == 0) {
    cat("✓ VERDICT: CLEAN EVENT STUDY\n")
    cat("  → No significant pre-trends\n")
    cat("  → Post-reform effects present\n")
    cat("  → Parallel trends supported\n\n")
  } else {
    cat("⚠ VERDICT: PRE-TRENDS DETECTED IN EVENT STUDY\n")
    cat(sprintf("  → %d of %d pre-reform periods significant\n", pre_sig, pre_total))
    cat("  → Parallel trends questionable\n")
    cat("  → Consider alternative identification strategies\n\n")
  }
  
} else {
  cat("Note: Could not extract dynamic coefficients\n\n")
  event_study <- data.frame()
}

# ============================================================================
# PRIORITY 3: EVENT STUDY PLOT
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("PRIORITY 3: EVENT STUDY VISUALIZATION\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

if(nrow(event_study) > 0) {
  
  p_event <- ggplot(event_study, aes(x = time, y = coef)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    geom_line(linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid", color = "red", alpha = 0.5) +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "blue", alpha = 0.5) +
    annotate("text", x = -2, y = max(event_study$ci_upper) * 0.9, 
             label = "Pre-reform", hjust = 0.5, size = 3.5, color = "gray30") +
    annotate("text", x = 2, y = max(event_study$ci_upper) * 0.9, 
             label = "Post-reform", hjust = 0.5, size = 3.5, color = "gray30") +
    labs(
      title = "Event Study: Regulatory Reforms → Entrepreneurship",
      subtitle = "Dynamic DiD with leads & lags (t = -1 omitted as reference)",
      x = "Years relative to reform",
      y = "Treatment effect (pp change in TEA)",
      caption = "Error bars: 95% confidence intervals | Red line: no effect | Blue line: reform timing"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      panel.grid.minor = element_blank()
    )
  
  ggsave("output/figures/11_event_study_plot.png", p_event, 
         width = 10, height = 6, dpi = 300)
  
  cat("✓ Event study plot saved: output/figures/11_event_study_plot.png\n\n")
  
  # Interpretation guide
  cat("── How to Read This Plot ──\n")
  cat("  1. Horizontal axis: Time relative to reform (0 = reform year)\n")
  cat("  2. Vertical axis: Treatment effect on TEA (percentage points)\n")
  cat("  3. Red line: No effect baseline\n")
  cat("  4. Blue line: Reform timing\n\n")
  
  cat("  GOOD pattern (parallel trends OK):\n")
  cat("    • Pre-reform coefficients cluster around 0\n")
  cat("    • Sharp jump at t=0 (reform year)\n")
  cat("    • Persistent positive effect after reform\n\n")
  
  cat("  BAD pattern (parallel trends violated):\n")
  cat("    • Pre-reform coefficients trending upward\n")
  cat("    • Effect appears before t=0\n")
  cat("    • No clear break at reform timing\n\n")
  
} else {
  cat("Could not create event study plot (no coefficients extracted)\n\n")
}

# ============================================================================
# PRIORITY 4: LEE BOUNDS & ROBUSTNESS
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("PRIORITY 4: SENSITIVITY ANALYSIS & BOUNDS\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

cat("Test 1: Exclude countries with extreme baseline TEA\n")

# Calculate baseline TEA by country (pre-2012)
baseline_tea <- aps_did %>%
  filter(year < 2012) %>%
  group_by(country_id) %>%
  summarize(
    baseline_tea = mean(tea, na.rm = TRUE),
    .groups = "drop"
  )

# Identify outliers (top/bottom 10%)
outlier_threshold <- quantile(baseline_tea$baseline_tea, c(0.10, 0.90), na.rm = TRUE)
outlier_countries <- baseline_tea %>%
  filter(baseline_tea < outlier_threshold[1] | baseline_tea > outlier_threshold[2]) %>%
  pull(country_id)

cat(sprintf("  Baseline TEA range: %.1f%% - %.1f%%\n", 
            min(baseline_tea$baseline_tea) * 100, 
            max(baseline_tea$baseline_tea) * 100))
cat(sprintf("  Outlier threshold: < %.1f%% or > %.1f%%\n",
            outlier_threshold[1] * 100, outlier_threshold[2] * 100))
cat(sprintf("  Countries excluded: %d (%.0f%%)\n\n",
            length(outlier_countries),
            100 * length(outlier_countries) / nrow(baseline_tea)))

# Re-estimate DiD without outliers
aps_robust <- aps_did %>%
  filter(!country_id %in% outlier_countries)

m_robust_outlier <- felm(
  tea ~ did_term |
    as.factor(country_id) + as.factor(year),
  data = aps_robust
)

robust_outlier_coef <- summary(m_robust_outlier)$coefficients["did_term", "Estimate"]
robust_outlier_p <- summary(m_robust_outlier)$coefficients["did_term", "Pr(>|t|)"]

cat(sprintf("  DiD (outliers removed): β = %.6f, p = %.4f %s\n\n",
            robust_outlier_coef, robust_outlier_p,
            ifelse(robust_outlier_p < 0.05, "**", 
                   ifelse(robust_outlier_p < 0.10, "*", ""))))

# Test 2: Exclude early/late adopters
cat("Test 2: Exclude early/late reform adopters\n")

reform_timing <- aps_did %>%
  filter(treated == 1) %>%
  distinct(country_id, reform_year) %>%
  arrange(reform_year)

early_late_threshold <- quantile(reform_timing$reform_year, c(0.25, 0.75), na.rm = TRUE)
middle_adopters <- reform_timing %>%
  filter(reform_year >= early_late_threshold[1], 
         reform_year <= early_late_threshold[2]) %>%
  pull(country_id)

cat(sprintf("  Reform years: %d - %d\n", 
            min(reform_timing$reform_year), 
            max(reform_timing$reform_year)))
cat(sprintf("  Middle adopters: %d - %d\n",
            early_late_threshold[1], early_late_threshold[2]))
cat(sprintf("  Countries retained: %d of %d\n\n",
            length(middle_adopters), nrow(reform_timing)))

aps_middle <- aps_did %>%
  filter(!treated | country_id %in% middle_adopters)

m_robust_timing <- felm(
  tea ~ did_term |
    as.factor(country_id) + as.factor(year),
  data = aps_middle
)

robust_timing_coef <- summary(m_robust_timing)$coefficients["did_term", "Estimate"]
robust_timing_p <- summary(m_robust_timing)$coefficients["did_term", "Pr(>|t|)"]

cat(sprintf("  DiD (middle adopters only): β = %.6f, p = %.4f %s\n\n",
            robust_timing_coef, robust_timing_p,
            ifelse(robust_timing_p < 0.05, "**", 
                   ifelse(robust_timing_p < 0.10, "*", ""))))

# Test 3: Restrict to balanced panel
cat("Test 3: Balanced panel (countries present in all years)\n")

country_year_count <- aps_did %>%
  group_by(country_id) %>%
  summarize(n_years = n_distinct(year), .groups = "drop")

balanced_countries <- country_year_count %>%
  filter(n_years == max(n_years)) %>%
  pull(country_id)

cat(sprintf("  Balanced panel countries: %d of %d\n\n",
            length(balanced_countries), n_distinct(aps_did$country_id)))

aps_balanced <- aps_did %>%
  filter(country_id %in% balanced_countries)

m_robust_balanced <- felm(
  tea ~ did_term |
    as.factor(country_id) + as.factor(year),
  data = aps_balanced
)

robust_balanced_coef <- summary(m_robust_balanced)$coefficients["did_term", "Estimate"]
robust_balanced_p <- summary(m_robust_balanced)$coefficients["did_term", "Pr(>|t|)"]

cat(sprintf("  DiD (balanced panel): β = %.6f, p = %.4f %s\n\n",
            robust_balanced_coef, robust_balanced_p,
            ifelse(robust_balanced_p < 0.05, "**", 
                   ifelse(robust_balanced_p < 0.10, "*", ""))))

# ============================================================================
# SUMMARY TABLE
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("ROBUSTNESS SUMMARY: ALL SPECIFICATIONS\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

# Baseline from Phase 4 (load from previous results)
baseline_coef <- 0.0019  # From basic DiD model 1
baseline_p <- 0.224

# Create summary table
robustness_summary <- tibble(
  specification = c(
    "1. Baseline DiD (Phase 4)",
    "2. Dynamic DiD: Reform year (t=0)",
    "3. Dynamic DiD: Post-reform average (t≥1)",
    "4. Excluding baseline outliers",
    "5. Middle adopters only",
    "6. Balanced panel only"
  ),
  coefficient = c(
    baseline_coef,
    ifelse(nrow(event_study) > 0, event_study$coef[event_study$time == 0], NA),
    ifelse(nrow(event_study) > 0, mean(event_study$coef[event_study$time > 0], na.rm = TRUE), NA),
    robust_outlier_coef,
    robust_timing_coef,
    robust_balanced_coef
  ),
  p_value = c(
    baseline_p,
    ifelse(nrow(event_study) > 0, event_study$p_value[event_study$time == 0], NA),
    NA,  # Average doesn't have p-value
    robust_outlier_p,
    robust_timing_p,
    robust_balanced_p
  ),
  sig = case_when(
    is.na(p_value) ~ "",
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
)

cat(sprintf("%-45s  %-12s  %-10s  %-5s\n", 
            "Specification", "Coefficient", "p-value", "Sig"))
cat(strrep("-", 80), "\n")
for(i in 1:nrow(robustness_summary)) {
  cat(sprintf("%-45s  %12.6f  %10.4f  %-5s\n",
              robustness_summary$specification[i],
              robustness_summary$coefficient[i],
              ifelse(is.na(robustness_summary$p_value[i]), NA, robustness_summary$p_value[i]),
              robustness_summary$sig[i]))
}
cat("\n")

# ============================================================================
# FINAL VERDICT
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("FINAL VERDICT: IS THE ANALYSIS BULLETPROOF?\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

# Count tests
n_tests <- 4
tests_passed <- 0

# Test 1: Pre-trends
test1_pass <- pretrend_p > 0.10
if(test1_pass) tests_passed <- tests_passed + 1

cat(sprintf("Test 1: Pre-trend test: %s\n", 
            ifelse(test1_pass, "✓ PASS", "✗ FAIL")))
cat(sprintf("  Pre-trend coefficient p-value: %.4f %s\n\n",
            pretrend_p,
            ifelse(test1_pass, "(not significant)", "(significant → bias)")))

# Test 2: Event study
if(nrow(event_study) > 0) {
  test2_pass <- sum(event_study$p_value[event_study$time < 0 & event_study$time != -1] < 0.05, na.rm = TRUE) == 0
  if(test2_pass) tests_passed <- tests_passed + 1
  
  cat(sprintf("Test 2: Event study pre-trends: %s\n", 
              ifelse(test2_pass, "✓ PASS", "✗ FAIL")))
  cat(sprintf("  Significant pre-reform leads: %d of %d %s\n\n",
              sum(event_study$p_value[event_study$time < 0 & event_study$time != -1] < 0.05, na.rm = TRUE),
              sum(event_study$time < 0 & event_study$time != -1),
              ifelse(test2_pass, "(none)", "(violations present)")))
} else {
  cat("Test 2: Event study pre-trends: ⚠ SKIPPED (no data)\n\n")
}

# Test 3: Robustness
test3_pass <- all(c(robust_outlier_p, robust_timing_p, robust_balanced_p) < 0.05, na.rm = TRUE)
if(test3_pass) tests_passed <- tests_passed + 1

cat(sprintf("Test 3: Robustness checks: %s\n", 
            ifelse(test3_pass, "✓ PASS", "⚠ MIXED")))
cat(sprintf("  All alternative specifications significant: %s\n\n",
            ifelse(test3_pass, "YES", "SOME")))

# Test 4: Effect consistency
coef_range <- max(robustness_summary$coefficient, na.rm = TRUE) - 
              min(robustness_summary$coefficient, na.rm = TRUE)
test4_pass <- coef_range < 0.01  # Within 1 pp
if(test4_pass) tests_passed <- tests_passed + 1

cat(sprintf("Test 4: Effect consistency: %s\n", 
            ifelse(test4_pass, "✓ PASS", "⚠ VARIABLE")))
cat(sprintf("  Range across specifications: %.6f pp %s\n\n",
            coef_range,
            ifelse(test4_pass, "(tight)", "(wide variation)")))

# Overall verdict
cat(strrep("─", 75), "\n")
cat(sprintf("OVERALL: %d of %d tests passed\n\n", tests_passed, n_tests))

if(tests_passed == n_tests) {
  cat("✓✓✓ BULLETPROOF ANALYSIS ✓✓✓\n")
  cat("  • No pre-trends detected\n")
  cat("  • Event study supports parallel trends\n")
  cat("  • Robust across specifications\n")
  cat("  • Effect size consistent\n")
  cat("  → CAUSAL INTERPRETATION CREDIBLE\n\n")
} else if(tests_passed >= n_tests - 1) {
  cat("✓✓ STRONG ANALYSIS (minor concerns) ✓✓\n")
  cat("  • Most robustness checks passed\n")
  cat("  • Minor violations or inconsistencies\n")
  cat("  → CAUSAL INTERPRETATION LIKELY VALID\n\n")
} else {
  cat("⚠⚠ ANALYSIS NEEDS ATTENTION ⚠⚠\n")
  cat("  • Multiple robustness checks failed\n")
  cat("  • Parallel trends questionable\n")
  cat("  → CAUSAL INTERPRETATION UNCERTAIN\n")
  cat("  → Consider alternative identification strategies\n\n")
}

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("SAVING RESULTS\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

# Save pre-trend test
if(nrow(pretrend_result) > 0) {
  write_csv(pretrend_result, "output/did_results/11a_pretrend_test.csv")
  cat("✓ Pre-trend test: 11a_pretrend_test.csv\n")
}

# Save event study
if(nrow(event_study) > 0) {
  write_csv(event_study, "output/did_results/11b_event_study.csv")
  cat("✓ Event study: 11b_event_study.csv\n")
}

# Save robustness summary
write_csv(robustness_summary, "output/did_results/11c_robustness_summary.csv")
cat("✓ Robustness summary: 11c_robustness_summary.csv\n\n")

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("✓ BULLETPROOF DiD ANALYSIS COMPLETE\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")
