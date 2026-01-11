# ============================================================================
# Causal Impacts of Reforms Beyond Entrepreneurship (DiD Battery)
# ============================================================================
# Outcomes: Business registrations, Youth employment, Financial inclusion
# Data sources (required):
# - World Bank Entrepreneurship Database (country-year new business registrations)
# - ILO Youth employment (country-year youth employment/unemployment/NEET)
# - Global Findex (country-year account ownership, saving, borrowing)
# - Doing Business historical (already loaded) to identify reform timing
# ============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(fixest)

wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

cat("\n============================================================\n")
cat("REFORM IMPACTS BEYOND ENTREPRENEURSHIP: DiD BATTERY\n")
cat("============================================================\n\n")

# ---------------------------------------------------------------------------
# Step 0: Load or construct reforms (from DB historical file)
# ---------------------------------------------------------------------------

source_reforms <- function() {
  db_file <- "data_raw/data-DiD/Historical-Data--DB04-DB20-.xlsx"
  db_raw <- read_excel(db_file, sheet = "DB21 Data", skip = 4)
  
  db_combined <- db_raw %>%
    select(
      country_code = `Country code`,
      economy = Economy,
      db_year = `DB year`,
      score_db17_21 = `Ease of doing business score (DB17-21 methodology)`,
      score_db15 = `Ease of doing business score (DB15 methodology)`,
      score_db10_14 = `Ease of doing business score (DB10-14 methodology)`
    ) %>%
    mutate(year_calendar = db_year - 1) %>%
    mutate(
      db_score = case_when(
        !is.na(score_db17_21) ~ score_db17_21,
        !is.na(score_db15) ~ score_db15,
        !is.na(score_db10_14) ~ score_db10_14,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(economy), !is.na(year_calendar), !is.na(db_score)) %>%
    arrange(economy, year_calendar)
  
  db_changes <- db_combined %>%
    group_by(economy) %>%
    arrange(year_calendar) %>%
    mutate(score_change = db_score - lag(db_score), years_gap = year_calendar - lag(year_calendar)) %>%
    ungroup() %>%
    filter(years_gap == 1)
  
  q <- quantile(db_changes$score_change, c(0.75), na.rm=TRUE)
  threshold <- as.numeric(q[1])
  
  reforms_treated <- db_changes %>%
    filter(score_change > threshold, year_calendar >= 2010, year_calendar <= 2015) %>%
    group_by(economy) %>%
    arrange(year_calendar) %>%
    slice(1) %>%
    ungroup() %>%
    select(economy, reform_year = year_calendar)
  
  cat("✓ Reforms computed:", nrow(reforms_treated), "countries\n")
  reforms_treated
}

reforms <- source_reforms()

# Helper to build panel with treated/post indicators
add_did_flags <- function(df, country_col, year_col) {
  df %>%
    left_join(reforms, by = setNames("economy", country_col)) %>%
    mutate(
      treated = ifelse(!is.na(reform_year), 1, 0),
      post = ifelse(.data[[year_col]] >= reform_year, 1, 0),
      did_term = treated * post
    )
}

# ---------------------------------------------------------------------------
# Step 1: Business Registrations (Entrepreneurship Database)
# ---------------------------------------------------------------------------
# File: API_IC.BUS.NDNS.ZS_DS2_en_excel_v2_426.xls
# World Bank indicator: New business density (new registrations per 1,000 people ages 15-64)

run_business_registrations <- function() {
  file <- "data_raw/data_causality/API_IC.BUS.NDNS.ZS_DS2_en_excel_v2_426.xls"
  if(!file.exists(file)) {
    cat("⚠ Missing:", file, "\n")
    return(NULL)
  }
  
  cat("\n=== Business Registrations Analysis ===\n")
  
  # Read World Bank format (skip metadata rows)
  df_raw <- read_excel(file, sheet = "Data", skip = 3)
  
  # Reshape from wide to long
  df_long <- df_raw %>%
    select(`Country Name`, matches("^[0-9]{4}$")) %>%
    rename(economy = `Country Name`) %>%
    pivot_longer(cols = -economy, names_to = "year", values_to = "outcome") %>%
    mutate(year = as.numeric(year)) %>%
    filter(!is.na(economy), !is.na(year), !is.na(outcome), year >= 2005, year <= 2020)
  
  cat("✓ Data loaded:", nrow(df_long), "country-year observations\n")
  cat("  - Countries:", n_distinct(df_long$economy), "\n")
  cat("  - Year range:", min(df_long$year), "to", max(df_long$year), "\n")
  
  # Merge with reforms
  panel <- df_long %>%
    left_join(reforms, by = "economy") %>%
    mutate(
      treated = ifelse(!is.na(reform_year), 1, 0),
      post = ifelse(year >= reform_year & !is.na(reform_year), 1, 0),
      did_term = treated * post
    ) %>%
    filter(!is.na(did_term))
  
  cat("  - Treated countries:", sum(panel$treated > 0 & !duplicated(panel$economy)), "\n")
  cat("  - Control countries:", sum(panel$treated == 0 & !duplicated(panel$economy)), "\n\n")
  
  # Run DiD
  cat("Running DiD: Business registrations per 1,000 working-age adults...\n")
  m <- feols(outcome ~ did_term | economy + year, data = panel, cluster = ~economy)
  s <- summary(m)
  
  cat("\n")
  print(s)
  
  # Extract results
  coef_val <- coef(m)[1]
  se_val <- s$se[1]
  pval <- s$coeftable[1,4]
  
  control_mean <- mean(panel$outcome[panel$treated == 0 & panel$post == 0], na.rm = TRUE)
  effect_pct <- 100 * coef_val / control_mean
  
  cat("\n✓ Effect interpretation:\n")
  cat(sprintf("  - Control mean (pre-reform): %.2f registrations per 1,000\n", control_mean))
  cat(sprintf("  - DiD coefficient: %.2f\n", coef_val))
  cat(sprintf("  - P-value: %.4f\n", pval))
  cat(sprintf("  - Percentage change: %.1f%%\n\n", effect_pct))
  
  out <- data.frame(
    outcome = "business_registrations",
    coef = coef_val,
    se = se_val,
    pvalue = pval,
    control_mean = control_mean,
    effect_pct = effect_pct,
    n_countries = length(unique(panel$economy)),
    n_obs = nrow(panel)
  )
  write.csv(out, "output/13_business_registrations_did.csv", row.names=FALSE)
  
  return(out)
}

# ---------------------------------------------------------------------------
# Step 2: Youth Employment (ILO)
# ---------------------------------------------------------------------------
# File: EIP_2EET_SEX_RT_A-20260111T0112.csv
# ILO NEET rate (youth not in employment, education or training)

run_youth_employment <- function() {
  file <- "data_raw/data_causality/EIP_2EET_SEX_RT_A-20260111T0112.csv"
  if(!file.exists(file)) {
    cat("⚠ Missing:", file, "\n")
    return(NULL)
  }
  
  cat("\n=== Youth NEET Rate Analysis ===\n")
  
  df_raw <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  
  # This file has time as rows and regions/countries as columns
  # Need to reshape from wide to long
  cat("File structure: time in rows, regions/countries in columns\n")
  cat("Columns found:", ncol(df_raw), "\n")
  
  # Reshape to long format
  df_long <- df_raw %>%
    pivot_longer(cols = -time, names_to = "economy", values_to = "outcome") %>%
    mutate(
      year = as.numeric(time),
      outcome = as.numeric(outcome)
    ) %>%
    # Filter out regional aggregates (World, Africa, etc.)
    filter(!grepl("^World|^Africa$|^Americas$|^Arab|^Asia|^Europe", economy)) %>%
    filter(!is.na(economy), !is.na(year), !is.na(outcome), year >= 2005, year <= 2020)
  
  if(nrow(df_long) == 0) {
    cat("⚠ No individual country data found (only regional aggregates)\n")
    cat("  This dataset may not have country-level detail.\n")
    return(NULL)
  }
  
  cat("✓ Data loaded:", nrow(df_long), "country-year observations\n")
  cat("  - Countries:", n_distinct(df_long$economy), "\n")
  cat("  - Year range:", min(df_long$year), "to", max(df_long$year), "\n")
  cat("  - Sample countries:", paste(head(unique(df_long$economy), 5), collapse=", "), "\n")
  
  # Merge with reforms
  panel <- df_long %>%
    left_join(reforms, by = "economy") %>%
    mutate(
      treated = ifelse(!is.na(reform_year), 1, 0),
      post = ifelse(year >= reform_year & !is.na(reform_year), 1, 0),
      did_term = treated * post
    ) %>%
    filter(!is.na(did_term))
  
  cat("  - Treated countries:", sum(panel$treated > 0 & !duplicated(panel$economy)), "\n")
  cat("  - Control countries:", sum(panel$treated == 0 & !duplicated(panel$economy)), "\n\n")
  
  if(sum(panel$treated) == 0) {
    cat("⚠ No treated countries matched. Skipping analysis.\n")
    return(NULL)
  }
  
  # Run DiD
  cat("Running DiD: Youth NEET rate (%)...\n")
  m <- feols(outcome ~ did_term | economy + year, data = panel, cluster = ~economy)
  s <- summary(m)
  
  cat("\n")
  print(s)
  
  # Extract results
  coef_val <- coef(m)[1]
  se_val <- s$se[1]
  pval <- s$coeftable[1,4]
  
  control_mean <- mean(panel$outcome[panel$treated == 0 & panel$post == 0], na.rm = TRUE)
  effect_pct <- 100 * coef_val / control_mean
  
  cat("\n✓ Effect interpretation:\n")
  cat(sprintf("  - Control mean (pre-reform): %.2f%% NEET rate\n", control_mean))
  cat(sprintf("  - DiD coefficient: %.2f percentage points\n", coef_val))
  cat(sprintf("  - P-value: %.4f\n", pval))
  cat(sprintf("  - Percentage change: %.1f%%\n\n", effect_pct))
  
  out <- data.frame(
    outcome = "youth_neet",
    coef = coef_val,
    se = se_val,
    pvalue = pval,
    control_mean = control_mean,
    effect_pct = effect_pct,
    n_countries = length(unique(panel$economy)),
    n_obs = nrow(panel)
  )
  write.csv(out, "output/13_youth_employment_did.csv", row.names=FALSE)
  
  return(out)
}

# ---------------------------------------------------------------------------
# Step 3: Financial Inclusion (Global Findex)
# ---------------------------------------------------------------------------
# File: GlobalFindexDatabase2025.xlsx
# Account ownership, digital payments, saving, borrowing

run_financial_inclusion <- function() {
  file <- "data_raw/data_causality/GlobalFindexDatabase2025.xlsx"
  if(!file.exists(file)) {
    cat("⚠ Missing:", file, "\n")
    return(NULL)
  }
  
  cat("\n=== Financial Inclusion Analysis ===\n")
  
  # Try common sheet names
  sheets <- excel_sheets(file)
  cat("Available sheets:", paste(sheets, collapse=", "), "\n")
  
  # Usually "Data" or first sheet
  sheet_name <- if("Data" %in% sheets) "Data" else sheets[1]
  
  df_raw <- read_excel(file, sheet = sheet_name)
  
  # Findex format varies by edition - try to identify key columns
  colnames(df_raw) <- make.names(colnames(df_raw))
  
  # Look for account ownership indicator (most stable across years)
  account_col <- names(df_raw)[grepl("account|has.account|ownership", names(df_raw), ignore.case = TRUE)][1]
  country_col <- names(df_raw)[grepl("economy|country", names(df_raw), ignore.case = TRUE)][1]
  year_col <- names(df_raw)[grepl("year", names(df_raw), ignore.case = TRUE)][1]
  
  if(is.na(account_col) | is.na(country_col)) {
    cat("⚠ Could not identify columns. Found:\n")
    print(head(names(df_raw), 20))
    return(NULL)
  }
  
  df_long <- df_raw %>%
    select(economy = !!country_col, year = !!year_col, outcome = !!account_col) %>%
    mutate(
      year = as.numeric(year),
      outcome = as.numeric(outcome)
    ) %>%
    filter(!is.na(economy), !is.na(year), !is.na(outcome), year >= 2005)
  
  cat("✓ Data loaded:", nrow(df_long), "country-year observations\n")
  cat("  - Countries:", n_distinct(df_long$economy), "\n")
  cat("  - Years:", paste(sort(unique(df_long$year)), collapse=", "), "\n")
  
  # Merge with reforms
  panel <- df_long %>%
    left_join(reforms, by = "economy") %>%
    mutate(
      treated = ifelse(!is.na(reform_year), 1, 0),
      post = ifelse(year >= reform_year & !is.na(reform_year), 1, 0),
      did_term = treated * post
    ) %>%
    filter(!is.na(did_term))
  
  cat("  - Treated countries:", sum(panel$treated > 0 & !duplicated(panel$economy)), "\n")
  cat("  - Control countries:", sum(panel$treated == 0 & !duplicated(panel$economy)), "\n\n")
  
  # Run DiD
  cat("Running DiD: Account ownership (%)...\n")
  m <- feols(outcome ~ did_term | economy + year, data = panel, cluster = ~economy)
  s <- summary(m)
  
  cat("\n")
  print(s)
  
  # Extract results
  coef_val <- coef(m)[1]
  se_val <- s$se[1]
  pval <- s$coeftable[1,4]
  
  control_mean <- mean(panel$outcome[panel$treated == 0 & panel$post == 0], na.rm = TRUE)
  effect_pct <- 100 * coef_val / control_mean
  
  cat("\n✓ Effect interpretation:\n")
  cat(sprintf("  - Control mean (pre-reform): %.2f%% have accounts\n", control_mean))
  cat(sprintf("  - DiD coefficient: %.2f percentage points\n", coef_val))
  cat(sprintf("  - P-value: %.4f\n", pval))
  cat(sprintf("  - Percentage change: %.1f%%\n\n", effect_pct))
  
  out <- data.frame(
    outcome = "financial_inclusion",
    coef = coef_val,
    se = se_val,
    pvalue = pval,
    control_mean = control_mean,
    effect_pct = effect_pct,
    n_countries = length(unique(panel$economy)),
    n_obs = nrow(panel)
  )
  write.csv(out, "output/13_financial_inclusion_did.csv", row.names=FALSE)
  
  return(out)
}

# ---------------------------------------------------------------------------
# Execute all available analyses
# ---------------------------------------------------------------------------

cat("\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("RUNNING ALL ANALYSES\n")
cat(paste(rep("=", 70), collapse=""), "\n")

results <- list()

results$registrations <- run_business_registrations()
results$youth <- run_youth_employment()
results$finance <- run_financial_inclusion()

# Consolidate results
cat("\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("SUMMARY OF REFORM IMPACTS\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

all_results <- bind_rows(results)
if(nrow(all_results) > 0) {
  all_results <- all_results %>%
    mutate(
      significant = ifelse(pvalue < 0.05, "✓ YES", "✗ NO"),
      direction = ifelse(coef > 0, "↑ Increase", "↓ Decrease")
    )
  
  write.csv(all_results, "output/13_all_reform_impacts_summary.csv", row.names=FALSE)
  
  cat("Results:\n\n")
  print(all_results %>% select(outcome, coef, pvalue, significant, direction, control_mean, n_countries))
  
  cat("\n\nInterpretation:\n")
  for(i in 1:nrow(all_results)) {
    row <- all_results[i,]
    cat(sprintf("\n%d. %s:\n", i, toupper(row$outcome)))
    cat(sprintf("   - Effect: %.3f (p=%.4f) %s\n", row$coef, row$pvalue, row$significant))
    cat(sprintf("   - Countries: %d | Observations: %d\n", row$n_countries, row$n_obs))
    
    if(row$pvalue < 0.05) {
      cat(sprintf("   - FINDING: Reforms causally %s this outcome by %.1f%%\n", 
                  ifelse(row$coef > 0, "INCREASE", "DECREASE"), abs(row$effect_pct)))
    } else {
      cat("   - FINDING: No significant reform effect detected\n")
    }
  }
  
  cat("\n\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat("POLICY IMPLICATIONS\n")
  cat(paste(rep("=", 70), collapse=""), "\n\n")
  
  sig_count <- sum(all_results$pvalue < 0.05, na.rm=TRUE)
  
  if(sig_count == 0) {
    cat("⚠ NO SIGNIFICANT EFFECTS DETECTED\n\n")
    cat("Reforms alone do not significantly affect:\n")
    cat("- Business registrations (firm entry)\n")
    cat("- Youth employment/NEET rates\n")
    cat("- Financial inclusion\n\n")
    cat("Implication: Regulatory simplification is insufficient for development.\n")
    cat("Complementary interventions needed (training, finance, networks).\n\n")
  } else {
    cat(sprintf("✓ %d OF %d OUTCOMES SHOW SIGNIFICANT EFFECTS\n\n", sig_count, nrow(all_results)))
    
    sig_results <- all_results %>% filter(pvalue < 0.05)
    for(i in 1:nrow(sig_results)) {
      row <- sig_results[i,]
      cat(sprintf("- %s: %s%.2f (%.1f%%)\n", 
                  toupper(row$outcome),
                  ifelse(row$coef > 0, "+", ""),
                  row$coef,
                  abs(row$effect_pct)))
    }
    
    cat("\nImplication: Reforms work for specific outcomes but not entrepreneurship.\n")
    cat("Policy should target these pathways.\n\n")
  }
}

cat("\nDone. Check output/ for detailed results.\n")
