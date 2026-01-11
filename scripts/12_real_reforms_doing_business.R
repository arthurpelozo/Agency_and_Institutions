# ============================================================================
# 12. REAL REFORM TIMING FROM WORLD BANK DOING BUSINESS
# ============================================================================
# Purpose: Replace random treatment assignment with ACTUAL reform data
#          Re-run bulletproof DiD with real Doing Business reform timing
#
# Data sources:
#   - Historical-Data--DB04-DB20-.xlsx (WB Doing Business historical scores)
#   - Explanations-for-Historical-Data-Changes.xlsx (reform documentation)
#   - Starting a Business.xlsx (entrepreneurship-specific reforms)
#   - GEM APS individual-level data (1.34M observations)
#
# Author: Phase 5 - Real Data Integration
# Date: 2026-01-10
# ============================================================================

library(tidyverse)
library(lfe)
library(haven)
library(readxl)
library(broom)

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("REAL REFORM TIMING: WORLD BANK DOING BUSINESS DATA\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

# ============================================================================
# STEP 1: LOAD WORLD BANK DOING BUSINESS DATA
# ============================================================================

cat("Step 1: Loading World Bank Doing Business historical data...\n\n")

# Load historical data (DB04-DB20)
db_file <- "data_raw/data-DiD/Historical-Data--DB04-DB20-.xlsx"

if(!file.exists(db_file)) {
  stop("ERROR: Cannot find World Bank data file: ", db_file,
       "\nCurrent directory: ", getwd())
}

# Load DB21 Data sheet with full historical data (2004-2020)
# Row 1-3: Legends and colors
# Row 4: Column headers  
# Row 5+: Actual data
db_raw <- read_excel(db_file, sheet = "DB21 Data", skip = 4)

cat(sprintf("Loaded %d rows, %d columns\n", nrow(db_raw), ncol(db_raw)))
cat("Key columns:\n")
print(names(db_raw)[1:6])
cat("\n")

# Show sample
cat("Sample data (first 3 countries, DB2020):\n")
print(head(db_raw %>% filter(`DB year` == 2020) %>% select(1:6), 3))
cat("\n")

# ============================================================================
# STEP 2: CLEAN AND RESHAPE DOING BUSINESS DATA
# ============================================================================

cat("Step 2: Cleaning and reshaping Doing Business data...\n\n")

# The data has multiple years per country (panel format already)
# Columns: Country code, Economy, Region, Income group, DB year, Ease of doing business score
# Keep only key columns
db_clean <- db_raw %>%
  select(
    country_code = `Country code`,
    economy = Economy,
    region = Region,
    income = `Income group`,
    db_year = `DB year`,
    db_score = `Ease of doing business score (DB17-21 methodology)`,  # Changed for DB21 data
    starts_with("Score-")  # Topic scores
  ) %>%
  filter(!is.na(economy), !is.na(db_year), !is.na(db_score)) %>%
  mutate(
    db_year = as.numeric(db_year),
    db_score = as.numeric(db_score),
    # DB year corresponds to data as of June previous year
    # DB2020 = reforms up to mid-2019, so use db_year - 1 for calendar year
    year_calendar = db_year - 1
  ) %>%
  arrange(economy, db_year)

cat(sprintf("Cleaned data: %s observations\n", format(nrow(db_clean), big.mark = ",")))
cat(sprintf("Countries: %d\n", n_distinct(db_clean$economy)))
cat(sprintf("DB years: %d-%d\n", min(db_clean$db_year), max(db_clean$db_year)))
cat(sprintf("Calendar years: %d-%d\n\n", min(db_clean$year_calendar), max(db_clean$year_calendar)))

# Show sample
cat("Sample (USA 2015-2020):\n")
print(db_clean %>% 
        filter(economy == "United States", db_year >= 2015) %>% 
        select(economy, db_year, year_calendar, db_score))
cat("\n")

# ============================================================================
# STEP 3: IDENTIFY REFORMS (SCORE IMPROVEMENTS)
# ============================================================================

cat("Step 3: Identifying regulatory reforms from score changes...\n\n")

# Calculate year-over-year changes
db_reforms <- db_clean %>%
  group_by(economy) %>%
  arrange(db_year) %>%
  mutate(
    db_score_lag = lag(db_score),
    db_score_change = db_score - db_score_lag,
    db_score_pct_change = 100 * db_score_change / db_score_lag
  ) %>%
  ungroup()

# Summary of changes
cat("Distribution of year-over-year score changes:\n")
change_summary <- summary(db_reforms$db_score_change)
print(change_summary)
cat("\n")

# Define reform as improvement above threshold
# Using percentile-based thresholds
reforms_quantiles <- quantile(db_reforms$db_score_change, 
                               c(0.50, 0.75, 0.90), na.rm = TRUE)

cat("Reform thresholds (score improvement):\n")
cat(sprintf("  50th percentile (median): %.2f points\n", reforms_quantiles[1]))
cat(sprintf("  75th percentile: %.2f points\n", reforms_quantiles[2]))
cat(sprintf("  90th percentile: %.2f points\n\n", reforms_quantiles[3]))

# Use 75th percentile as moderate reform
reform_threshold <- reforms_quantiles[2]

db_reforms <- db_reforms %>%
  mutate(
    reform_major = db_score_change > reforms_quantiles[3],  # Top 10%
    reform_moderate = db_score_change > reform_threshold,    # Top 25%
    reform_any = db_score_change > reforms_quantiles[1]     # Top 50%
  )

cat(sprintf("Reforms identified:\n"))
cat(sprintf("  Major reforms (>%.1f pts, top 10%%): %d country-years (%.1f%%)\n",
            reforms_quantiles[3],
            sum(db_reforms$reform_major, na.rm = TRUE),
            100 * mean(db_reforms$reform_major, na.rm = TRUE)))
cat(sprintf("  Moderate reforms (>%.1f pts, top 25%%): %d country-years (%.1f%%)\n",
            reform_threshold,
            sum(db_reforms$reform_moderate, na.rm = TRUE),
            100 * mean(db_reforms$reform_moderate, na.rm = TRUE)))
cat(sprintf("  Any reforms (>%.1f pts, top 50%%): %d country-years (%.1f%%)\n\n",
            reforms_quantiles[1],
            sum(db_reforms$reform_any, na.rm = TRUE),
            100 * mean(db_reforms$reform_any, na.rm = TRUE)))

# For DiD, identify FIRST moderate reform in 2009-2017 window
# (GEM data available 2009-2020, need pre-periods and post-periods)
reforms_treated <- db_reforms %>%
  filter(
    reform_moderate == TRUE,
    year_calendar >= 2009,
    year_calendar <= 2015  # Need post-periods through 2018
  ) %>%
  group_by(economy) %>%
  summarize(
    reform_year = min(year_calendar),
    n_reforms = n(),
    avg_reform_size = mean(db_score_change, na.rm = TRUE),
    max_reform_size = max(db_score_change, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("Countries with moderate+ reforms (2009-2015): %d\n\n", nrow(reforms_treated)))

cat("Top 15 reformers by size:\n")
print(reforms_treated %>% 
        arrange(desc(avg_reform_size)) %>% 
        head(15) %>%
        mutate(across(where(is.numeric), ~round(., 2))))
cat("\n")

# ============================================================================
# STEP 4: LOAD GEM DATA
# ============================================================================

cat("Step 4: Loading GEM individual-level data...\n\n")

# Load the merged GEM data from Phase 4
aps_ind_file <- "output/did_results/aps_individual_merged.rds"

if(file.exists(aps_ind_file)) {
  aps_ind <- readRDS(aps_ind_file)
  cat(sprintf("Loaded %s GEM observations\n", format(nrow(aps_ind), big.mark = ",")))
} else {
  cat("WARNING: Merged GEM file not found. Attempting to load raw data...\n")
  
  # Load from raw SPSS files (fallback)
  aps_2019 <- read_sav("data_raw/GEM 2019 APS Global Individual Level Data_30Jan2021.sav")
  aps_2019$year <- 2019
  
  aps_ind <- aps_2019 %>%
    select(
      country = economy,
      year,
      tea = tea,
      agency_1 = Pknows,
      agency_2 = Pssskill,
      agency_3 = Popport,
      agency_4 = Nbgoodc,
      age = Age,
      gender = Gender,
      educ = gemeduc,
      income = gemincome
    ) %>%
    mutate(
      # Reconstruct agency index
      agency = (agency_1 + agency_2 + agency_3 + (1 - agency_4)) / 4,
      country = as.character(country)
    )
  
  cat(sprintf("Loaded %s observations from raw data\n", format(nrow(aps_ind), big.mark = ",")))
}

cat(sprintf("Countries: %d\n", n_distinct(aps_ind$country)))
cat(sprintf("Years: %d-%d\n\n", min(aps_ind$year), max(aps_ind$year)))

# ============================================================================
# STEP 5: MATCH COUNTRY NAMES (GEM <-> DOING BUSINESS)
# ============================================================================

cat("Step 5: Matching country names between GEM and Doing Business...\n\n")

gem_countries <- aps_ind %>% distinct(country) %>% pull(country) %>% sort()
db_countries <- db_reforms %>% distinct(economy) %>% pull(economy) %>% sort()

cat(sprintf("GEM countries: %d\n", length(gem_countries)))
cat(sprintf("DB countries: %d\n", length(db_countries)))

# Create matching table (common issues: US vs United States, etc.)
country_match <- tibble(
  gem_name = gem_countries
) %>%
  mutate(
    db_name = case_when(
      gem_name == "United States" ~ "United States",
      gem_name == "United Kingdom" ~ "United Kingdom",
      gem_name == "South Korea" ~ "Korea, Rep.",
      gem_name == "Russia" ~ "Russian Federation",
      gem_name == "Egypt" ~ "Egypt, Arab Rep.",
      gem_name == "Iran" ~ "Iran, Islamic Rep.",
      gem_name == "Slovak Republic" ~ "Slovak Republic",
      gem_name == "Venezuela" ~ "Venezuela, RB",
      TRUE ~ gem_name  # Default: assume same
    )
  )

# Check matches
matched <- country_match %>%
  mutate(
    in_db = db_name %in% db_countries
  )

cat(sprintf("Matched countries: %d of %d (%.0f%%)\n\n",
            sum(matched$in_db), nrow(matched),
            100 * mean(matched$in_db)))

# Show unmatched
unmatched <- matched %>% filter(!in_db)
if(nrow(unmatched) > 0) {
  cat("Unmatched countries (will be excluded):\n")
  print(unmatched$gem_name)
  cat("\n")
}

# ============================================================================
# STEP 6: MERGE TREATMENT ASSIGNMENT WITH GEM
# ============================================================================

cat("Step 6: Merging real reform timing with GEM data...\n\n")

# Add DB country names to GEM
aps_ind <- aps_ind %>%
  left_join(country_match, by = c("country" = "gem_name"))

# Merge with reform timing
aps_did_real <- aps_ind %>%
  left_join(
    reforms_treated %>% select(economy, reform_year, n_reforms, avg_reform_size),
    by = c("db_name" = "economy")
  ) %>%
  mutate(
    # Treatment indicators
    treated = !is.na(reform_year),
    post_reform = ifelse(treated, year >= reform_year, FALSE),
    did_term = treated * post_reform,
    
    # Time relative to reform
    time_to_reform = ifelse(treated, year - reform_year, NA)
  ) %>%
  filter(
    !is.na(db_name),  # Only countries matched to DB
    year >= 2008,     # Need pre-period
    year <= 2018      # Need post-period
  )

cat(sprintf("Final DiD sample: %s observations\n", 
            format(nrow(aps_did_real), big.mark = ",")))
cat(sprintf("  Treated countries: %d\n", 
            sum(aps_did_real$treated & !duplicated(aps_did_real$country))))
cat(sprintf("  Control countries: %d\n",
            sum(!aps_did_real$treated & !duplicated(aps_did_real$country))))
cat(sprintf("  Pre-reform obs: %s\n",
            format(sum(!aps_did_real$post_reform), big.mark = ",")))
cat(sprintf("  Post-reform obs: %s\n\n",
            format(sum(aps_did_real$post_reform, na.rm = TRUE), big.mark = ",")))

# Show reform years distribution
cat("Reform year distribution:\n")
reform_dist <- aps_did_real %>%
  filter(treated) %>%
  distinct(country, reform_year) %>%
  count(reform_year)
print(reform_dist)
cat("\n")

# ============================================================================
# STEP 7: RE-RUN BASIC DiD WITH REAL REFORMS
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("BASIC DiD WITH REAL REFORM TIMING\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

# Model 1: Simple DiD
m_did_real <- felm(
  tea ~ did_term |
    as.factor(country) + as.factor(year),
  data = aps_did_real %>% filter(!is.na(tea))
)

did_coef <- summary(m_did_real)$coefficients["did_term", "Estimate"]
did_se <- summary(m_did_real)$coefficients["did_term", "Std. Error"]
did_p <- summary(m_did_real)$coefficients["did_term", "Pr(>|t|)"]

cat("── Model 1: Basic DiD (Real Reform Timing) ──\n")
cat(sprintf("  DiD coefficient: β = %.6f (SE = %.6f)\n", did_coef, did_se))
cat(sprintf("  p-value: %.4f %s\n\n",
            did_p,
            ifelse(did_p < 0.01, "***", 
                   ifelse(did_p < 0.05, "**", 
                          ifelse(did_p < 0.10, "*", "")))))

# Compare to random assignment (from Phase 5)
cat("COMPARISON TO RANDOM ASSIGNMENT:\n")
cat("  Random assignment: β = 0.0019 (p = 0.224) — Not significant\n")
cat(sprintf("  Real reform timing: β = %.4f (p = %.4f) %s\n\n",
            did_coef, did_p,
            ifelse(did_p < 0.05, "— SIGNIFICANT!", "— Still not significant")))

# ============================================================================
# STEP 8: PRE-TREND TEST WITH REAL REFORMS
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("PRE-TREND TEST (REAL REFORMS)\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

# Filter to pre-reform period
aps_pre_real <- aps_did_real %>%
  filter(year < 2009 | (treated & year < reform_year))  # All 2008 + treated pre-reform

if(nrow(aps_pre_real) > 1000) {
  
  aps_pre_real <- aps_pre_real %>%
    mutate(
      treated_num = as.numeric(treated),
      year_numeric = as.numeric(year)
    )
  
  m_pretrend_real <- felm(
    tea ~ treated_num * year_numeric |
      as.factor(country),
    data = aps_pre_real %>% filter(!is.na(tea))
  )
  
  pretrend_coef_real <- summary(m_pretrend_real)$coefficients["treated_num:year_numeric", "Estimate"]
  pretrend_p_real <- summary(m_pretrend_real)$coefficients["treated_num:year_numeric", "Pr(>|t|)"]
  
  cat(sprintf("Pre-trend coefficient: β = %.6f, p = %.4f %s\n\n",
              pretrend_coef_real, pretrend_p_real,
              ifelse(pretrend_p_real < 0.05, "** PRE-TRENDS PRESENT",
                     ifelse(pretrend_p_real < 0.10, "* Marginal",
                            "✓ NO PRE-TRENDS"))))
  
  cat("COMPARISON TO RANDOM ASSIGNMENT:\n")
  cat("  Random assignment: β = 0.0135 (p < 0.001) — FAIL\n")
  cat(sprintf("  Real reform timing: β = %.4f (p = %.4f) %s\n\n",
              pretrend_coef_real, pretrend_p_real,
              ifelse(pretrend_p_real > 0.10, "— PASS!", "— Still problematic")))
  
} else {
  cat("Insufficient pre-reform data for pre-trend test\n\n")
  pretrend_p_real <- NA
}

# ============================================================================
# STEP 9: DYNAMIC DiD (EVENT STUDY) WITH REAL REFORMS
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("DYNAMIC DiD EVENT STUDY (REAL REFORMS)\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

# Create time bins
aps_event_real <- aps_did_real %>%
  filter(treated, !is.na(time_to_reform)) %>%
  mutate(
    time_bin = case_when(
      time_to_reform <= -4 ~ "-4+",
      time_to_reform == -3 ~ "-3",
      time_to_reform == -2 ~ "-2",
      time_to_reform == -1 ~ "-1 (ref)",
      time_to_reform == 0 ~ "0 (reform)",
      time_to_reform == 1 ~ "+1",
      time_to_reform == 2 ~ "+2",
      time_to_reform == 3 ~ "+3",
      time_to_reform >= 4 ~ "+4+",
      TRUE ~ NA_character_
    ),
    time_bin = factor(time_bin, levels = c(
      "-4+", "-3", "-2", "-1 (ref)", "0 (reform)",
      "+1", "+2", "+3", "+4+"
    ))
  )

cat(sprintf("Event study sample: %s observations\n\n",
            format(nrow(aps_event_real), big.mark = ",")))

# Dynamic model
m_dynamic_real <- felm(
  tea ~ I(time_bin == "-4+") +
        I(time_bin == "-3") +
        I(time_bin == "-2") +
        I(time_bin == "0 (reform)") +
        I(time_bin == "+1") +
        I(time_bin == "+2") +
        I(time_bin == "+3") +
        I(time_bin == "+4+") |
    as.factor(country) + as.factor(year),
  data = aps_event_real %>% filter(!is.na(tea))
)

# Extract coefficients
dynamic_coefs_real <- summary(m_dynamic_real)$coefficients
event_study_real <- tibble(
  time = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
  time_label = c("-4+", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4+"),
  coef = NA_real_,
  se = NA_real_,
  p_value = NA_real_
)

# t=-1 is reference
event_study_real$coef[event_study_real$time == -1] <- 0
event_study_real$se[event_study_real$time == -1] <- 0
event_study_real$p_value[event_study_real$time == -1] <- 1

# Fill others
for(i in 1:nrow(dynamic_coefs_real)) {
  row_name <- rownames(dynamic_coefs_real)[i]
  
  if(grepl("-4\\+", row_name)) idx <- which(event_study_real$time == -4)
  else if(grepl("-3", row_name) & !grepl("-", row_name %>% str_sub(-1))) idx <- which(event_study_real$time == -3)
  else if(grepl("-2", row_name) & !grepl("-", row_name %>% str_sub(-1))) idx <- which(event_study_real$time == -2)
  else if(grepl("0 \\(reform\\)", row_name)) idx <- which(event_study_real$time == 0)
  else if(grepl("\\+1", row_name)) idx <- which(event_study_real$time == 1)
  else if(grepl("\\+2", row_name)) idx <- which(event_study_real$time == 2)
  else if(grepl("\\+3", row_name)) idx <- which(event_study_real$time == 3)
  else if(grepl("\\+4\\+", row_name)) idx <- which(event_study_real$time == 4)
  else next
  
  if(length(idx) > 0) {
    event_study_real$coef[idx] <- dynamic_coefs_real[i, "Estimate"]
    event_study_real$se[idx] <- dynamic_coefs_real[i, "Std. Error"]
    event_study_real$p_value[idx] <- dynamic_coefs_real[i, "Pr(>|t|)"]
  }
}

event_study_real <- event_study_real %>%
  mutate(
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se
  )

cat("── Event Study Coefficients (Real Reforms) ──\n")
cat(sprintf("%-8s  %-10s  %-10s  %-10s\n", "Time", "Coef", "SE", "p-value"))
cat(strrep("-", 45), "\n")
for(i in 1:nrow(event_study_real)) {
  cat(sprintf("%-8s  %10.6f  %10.6f  %10.4f %s\n",
              event_study_real$time_label[i],
              event_study_real$coef[i],
              event_study_real$se[i],
              event_study_real$p_value[i],
              ifelse(event_study_real$p_value[i] < 0.05, "**",
                     ifelse(event_study_real$p_value[i] < 0.10, "*", ""))))
}
cat("\n")

# Check pre-trends
pre_sig_real <- sum(event_study_real$p_value[event_study_real$time < 0 & event_study_real$time != -1] < 0.05, na.rm = TRUE)
pre_total_real <- sum(event_study_real$time < 0 & event_study_real$time != -1)

cat(sprintf("Pre-reform leads significant: %d of %d\n", pre_sig_real, pre_total_real))

if(pre_sig_real == 0) {
  cat("✓ PASS: No significant pre-trends with real reform timing!\n\n")
} else {
  cat("⚠ WARNING: Some pre-trends remain with real timing\n\n")
}

# ============================================================================
# STEP 10: CREATE EVENT STUDY PLOT (REAL REFORMS)
# ============================================================================

p_event_real <- ggplot(event_study_real, aes(x = time, y = coef)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "steelblue") +
  geom_line(linetype = "dashed", alpha = 0.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", alpha = 0.5) +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "darkgreen", alpha = 0.7, linewidth = 1) +
  annotate("text", x = -2, y = max(event_study_real$ci_upper, na.rm = TRUE) * 0.9,
           label = "Pre-reform", hjust = 0.5, size = 4, color = "gray30") +
  annotate("text", x = 2, y = max(event_study_real$ci_upper, na.rm = TRUE) * 0.9,
           label = "Post-reform", hjust = 0.5, size = 4, color = "gray30") +
  labs(
    title = "Event Study: REAL Regulatory Reforms → Entrepreneurship",
    subtitle = "Using actual World Bank Doing Business reform timing (not random assignment)",
    x = "Years relative to reform",
    y = "Treatment effect (pp change in TEA)",
    caption = "Source: World Bank Doing Business (2004-2020) + GEM APS (2008-2018)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.minor = element_blank()
  )

ggsave("output/figures/12_event_study_real_reforms.png", p_event_real,
       width = 10, height = 6, dpi = 300)

cat("✓ Event study plot saved: output/figures/12_event_study_real_reforms.png\n\n")

# ============================================================================
# STEP 11: FINAL VERDICT
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("FINAL VERDICT: REAL REFORMS vs RANDOM ASSIGNMENT\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

verdict_table <- tibble(
  Test = c("Basic DiD", "Pre-trend test", "Event study pre-trends", "Overall"),
  Random = c(
    "β=0.0019, p=0.224 (FAIL)",
    "β=0.0135, p<0.001 (FAIL)",
    "3 of 3 significant (FAIL)",
    "0 of 4 tests passed"
  ),
  Real = c(
    sprintf("β=%.4f, p=%.4f %s", did_coef, did_p,
            ifelse(did_p < 0.05, "(PASS)", "(FAIL)")),
    ifelse(!is.na(pretrend_p_real),
           sprintf("β=%.4f, p=%.4f %s", pretrend_coef_real, pretrend_p_real,
                   ifelse(pretrend_p_real > 0.10, "(PASS)", "(FAIL)")),
           "Insufficient data"),
    sprintf("%d of %d significant %s", pre_sig_real, pre_total_real,
            ifelse(pre_sig_real == 0, "(PASS)", "(FAIL)")),
    sprintf("%d of 3 tests passed",
            sum(c(did_p < 0.05,
                  ifelse(!is.na(pretrend_p_real), pretrend_p_real > 0.10, FALSE),
                  pre_sig_real == 0)))
  )
)

print(verdict_table, width = Inf)
cat("\n")

# ============================================================================
# STEP 12: SAVE RESULTS
# ============================================================================

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("SAVING RESULTS\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")

# Save DiD dataset with real reforms
saveRDS(aps_did_real, "output/did_results/aps_did_real_reforms.rds")
cat("✓ DiD dataset: aps_did_real_reforms.rds\n")

# Save event study
write_csv(event_study_real, "output/did_results/12_event_study_real_reforms.csv")
cat("✓ Event study: 12_event_study_real_reforms.csv\n")

# Save reform timing
write_csv(reforms_treated, "output/did_results/12_reform_timing_doingbusiness.csv")
cat("✓ Reform timing: 12_reform_timing_doingbusiness.csv\n\n")

cat("═" %>% rep(75) %>% paste(collapse = ""), "\n")
cat("✓ REAL REFORM ANALYSIS COMPLETE\n")
cat("═" %>% rep(75) %>% paste(collapse = ""), "\n\n")
