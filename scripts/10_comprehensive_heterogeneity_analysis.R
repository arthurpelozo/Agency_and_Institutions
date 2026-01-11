# ============================================================================
# COMPREHENSIVE HETEROGENEITY & MECHANISM ANALYSIS
# ============================================================================
#
# This script extends the basic DiD to examine:
# 1. Heterogeneous effects by gender
# 2. Age subgroup analysis (18-24, 24-30, 30+)
# 3. Mechanism: agency component decomposition
# 4. Reform intensity and dose-response
# 5. Quantile analysis (agency deciles)
# 6. Institutional quality moderation
# 7. Placebo/robustness tests
# 8. Spillover effects within countries
#
# ============================================================================

library(tidyverse)
library(lfe)
library(quantreg)

set.seed(42)

cat("\n")
cat("█" %>% rep(70) %>% paste(collapse=""))
cat("\n COMPREHENSIVE HETEROGENEITY & MECHANISM ANALYSIS\n")
cat("█" %>% rep(70) %>% paste(collapse=""))
cat("\n\n")

# Load DiD panel data and full individual data
aps_did_base <- readRDS("output/did_results/aps_did_panel.rds")
aps_ind <- readRDS("data_processed/aps_agency_individual_2009_2020.rds")

# Convert all haven_labelled columns to numeric
aps_ind <- aps_ind %>%
  mutate(
    across(where(~!is.numeric(.x)), as.numeric),
    country = as.numeric(unclass(country))
  ) %>%
  rename(agency = agency_index)

# Merge DiD treatment variables to individual data
reform_info <- aps_did_base %>%
  distinct(country_id, treated, reform_year) %>%
  rename(country = country_id) %>%
  mutate(country = as.numeric(country))

aps_did <- aps_ind %>%
  left_join(reform_info, by = "country") %>%
  mutate(
    country_id = country,
    treated = ifelse(is.na(treated), 0, treated),
    reform_year = ifelse(is.na(reform_year), Inf, reform_year),
    post_reform = ifelse(treated == 1 & year >= reform_year, 1, 0),
    did_term = treated * post_reform
  ) %>%
  filter(year >= 2008, year <= 2018)

cat("Data loaded: %d obs, %d countries, %d years\n\n",
    nrow(aps_did), n_distinct(aps_did$country_id), n_distinct(aps_did$year))

# ============================================================================
# 1. GENDER HETEROGENEITY
# ============================================================================

cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n1. GENDER HETEROGENEITY\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n\n")

# Create female indicator
aps_did <- aps_did %>%
  mutate(female = as.numeric(gender == 2))

m_female <- felm(tea ~ did_term * female + treated + post_reform + female |
                   as.factor(country_id) + as.factor(year),
                 data = aps_did %>% filter(!is.na(gender)))
m_female_coef <- summary(m_female)$coefficients["did_term:female", "Estimate"]
m_female_p <- summary(m_female)$coefficients["did_term:female", "Pr(>|t|)"]

# Overall by gender (stratified)
m_male <- felm(tea ~ did_term + treated + post_reform |
                 as.factor(country_id) + as.factor(year),
               data = aps_did %>% filter(gender==1, !is.na(gender)))
m_female_only <- felm(tea ~ did_term + treated + post_reform |
                       as.factor(country_id) + as.factor(year),
                       data = aps_did %>% filter(gender==2, !is.na(gender)))

male_coef <- summary(m_male)$coefficients["did_term", "Estimate"]
male_p <- summary(m_male)$coefficients["did_term", "Pr(>|t|)"]
female_coef <- summary(m_female_only)$coefficients["did_term", "Estimate"]
female_p <- summary(m_female_only)$coefficients["did_term", "Pr(>|t|)"]

cat("Effect of reforms by gender (stratified models):\n\n")
cat("MALES (gender=1):\n")
cat(sprintf("  DiD coef: %.4f, p-value: %.4f %s\n",
            male_coef, male_p,
            ifelse(male_p < 0.05, "**", ifelse(male_p < 0.10, "*", ""))))

cat("\nFEMALES (gender=2):\n")
cat(sprintf("  DiD coef: %.4f, p-value: %.4f %s\n",
            female_coef, female_p,
            ifelse(female_p < 0.05, "**", ifelse(female_p < 0.10, "*", ""))))

cat("\nGENDER INTERACTION:\n")
cat(sprintf("  Female × DiD: %.4f, p-value: %.4f %s\n",
            m_female_coef, m_female_p,
            ifelse(m_female_p < 0.05, "**", ifelse(m_female_p < 0.10, "*", ""))))

gender_results <- tibble(
  Subgroup = c("Males", "Females", "Female×DiD"),
  Coefficient = c(male_coef, female_coef, m_female_coef),
  p_value = c(male_p, female_p, m_female_p),
  Significant = c(male_p < 0.05, female_p < 0.05, m_female_p < 0.05)
)

# ============================================================================
# 2. AGE SUBGROUP ANALYSIS
# ============================================================================

cat("\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n2. AGE SUBGROUP ANALYSIS\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n\n")

# Create age groups
aps_did <- aps_did %>%
  mutate(
    age_group = case_when(
      age < 18 ~ "Under 18",
      age >= 18 & age < 25 ~ "18-24",
      age >= 25 & age < 31 ~ "25-30",
      age >= 31 & age < 40 ~ "31-39",
      age >= 40 & age < 50 ~ "40-49",
      age >= 50 ~ "50+"
    )
  )

age_groups <- c("18-24", "25-30", "31-39", "40-49", "50+")
age_results <- list()

for(ag in age_groups) {
  m_age <- try(felm(tea ~ did_term + treated + post_reform |
                      as.factor(country_id) + as.factor(year),
                    data = aps_did %>% filter(age_group == ag, !is.na(age))),
               silent = TRUE)
  
  if(class(m_age)[1] != "try-error") {
    coef <- summary(m_age)$coefficients["did_term", "Estimate"]
    pval <- summary(m_age)$coefficients["did_term", "Pr(>|t|)"]
    n <- nrow(aps_did %>% filter(age_group == ag, !is.na(age)))
  } else {
    coef <- pval <- NA
    n <- 0
  }
  
  cat(sprintf("%s years (n=%d):\n", ag, n))
  if(!is.na(coef)) {
    cat(sprintf("  DiD coef: %.4f, p-value: %.4f %s\n\n",
                coef, pval,
                ifelse(pval < 0.05, "**", ifelse(pval < 0.10, "*", ""))))
  } else {
    cat("  Failed to estimate\n\n")
  }
  
  age_results[[ag]] <- tibble(
    Age_Group = ag,
    Coefficient = coef,
    p_value = pval,
    N = n
  )
}

age_results_df <- bind_rows(age_results)

# ============================================================================
# 3. AGENCY COMPONENT DECOMPOSITION (MECHANISM)
# ============================================================================

cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n3. AGENCY COMPONENT DECOMPOSITION (MECHANISM)\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n\n")

# Components: fear (reversed), skill, know, opportunity
# Create binary indicators for presence of each component
aps_did <- aps_did %>%
  mutate(
    no_fear = ifelse(is.na(fearfail), NA, as.numeric(unclass(fearfail)) == 0),  # 1 if no fear
    has_skill = ifelse(is.na(suskill), NA, as.numeric(unclass(suskill)) == 1),
    knows_ent = ifelse(is.na(knowent), NA, as.numeric(unclass(knowent)) == 1),
    sees_opp = ifelse(is.na(opport), NA, as.numeric(unclass(opport)) == 1)
  )

components <- c("no_fear", "has_skill", "knows_ent", "sees_opp")
component_names <- c("No Fear of Failure", "Has Skills", "Knows Entrepreneur", "Sees Opportunity")
component_results <- list()

for(i in seq_along(components)) {
  comp <- components[i]
  comp_name <- component_names[i]
  
  m_comp <- try(felm(tea ~ did_term * I(get(comp)) + treated + post_reform + I(get(comp)) |
                       as.factor(country_id) + as.factor(year),
                     data = aps_did %>% filter(!is.na(get(comp)))),
                silent = TRUE)
  
  if(class(m_comp)[1] != "try-error") {
    coef_main <- summary(m_comp)$coefficients["did_term", "Estimate"]
    coef_int <- tryCatch(summary(m_comp)$coefficients[paste0("did_term:I(get(\"", comp, "\"))"), "Estimate"], 
                         error = function(e) NA)
    p_int <- tryCatch(summary(m_comp)$coefficients[paste0("did_term:I(get(\"", comp, "\"))"), "Pr(>|t|)"], 
                      error = function(e) NA)
  } else {
    coef_main <- coef_int <- p_int <- NA
  }
  
  cat(sprintf("%s:\n", comp_name))
  cat(sprintf("  DiD main: %.4f\n", coef_main))
  cat(sprintf("  DiD × %s: %.4f, p-value: %.4f %s\n\n",
              comp, coef_int, p_int,
              ifelse(!is.na(p_int) && p_int < 0.05, "**",
                     ifelse(!is.na(p_int) && p_int < 0.10, "*", ""))))
  
  component_results[[comp_name]] <- tibble(
    Component = comp_name,
    DiD_Main = coef_main,
    DiD_Interaction = coef_int,
    p_value = p_int
  )
}

component_results_df <- bind_rows(component_results)

# ============================================================================
# 4. REFORM INTENSITY & DOSE-RESPONSE
# ============================================================================

cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n4. REFORM INTENSITY & DOSE-RESPONSE\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n\n")

# Count number of reforms per country
reform_intensity <- aps_did %>%
  distinct(country_id, reform_year, treated) %>%
  filter(treated == 1) %>%
  group_by(country_id) %>%
  summarise(n_reforms = n(), .groups = "drop")

cat(sprintf("Reform frequency: Min=%d, Max=%d, Mean=%.2f\n\n",
            min(reform_intensity$n_reforms),
            max(reform_intensity$n_reforms),
            mean(reform_intensity$n_reforms)))

# Merge back to panel and create dose variable
aps_did <- aps_did %>%
  left_join(reform_intensity, by = "country_id") %>%
  mutate(n_reforms = ifelse(treated == 0, 0, n_reforms))

# Model with reform intensity
m_intensity <- felm(tea ~ did_term + treated + post_reform + n_reforms |
                      as.factor(country_id) + as.factor(year),
                    data = aps_did)

intensity_coef <- summary(m_intensity)$coefficients["did_term", "Estimate"]
intensity_p <- summary(m_intensity)$coefficients["did_term", "Pr(>|t|)"]
n_reforms_coef <- summary(m_intensity)$coefficients["n_reforms", "Estimate"]
n_reforms_p <- summary(m_intensity)$coefficients["n_reforms", "Pr(>|t|)"]

cat("DiD effect (controlling for intensity):\n")
cat(sprintf("  DiD coef: %.4f, p-value: %.4f\n", intensity_coef, intensity_p))
cat(sprintf("  n_reforms coef: %.4f, p-value: %.4f %s\n\n",
            n_reforms_coef, n_reforms_p,
            ifelse(!is.na(n_reforms_p) && n_reforms_p < 0.05, "**",
                   ifelse(!is.na(n_reforms_p) && n_reforms_p < 0.10, "*", ""))))

# ============================================================================
# 5. QUANTILE REGRESSION (AGENCY DECILES)
# ============================================================================

cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n5. QUANTILE ANALYSIS (AGENCY DECILES)\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n\n")

# Create agency deciles
aps_did <- aps_did %>%
  mutate(agency_decile = ntile(agency, 10))

quantile_results <- list()
quantiles_to_test <- c(0.1, 0.25, 0.5, 0.75, 0.9)
q_labels <- c("10th", "25th", "Median", "75th", "90th")

for(i in seq_along(quantiles_to_test)) {
  q <- quantiles_to_test[i]
  q_label <- q_labels[i]
  
  m_q <- try(rq(tea ~ did_term + treated + post_reform + as.factor(country_id) + as.factor(year),
                tau = q, data = aps_did %>% filter(!is.na(tea), !is.na(agency))),
             silent = TRUE)
  
  if(class(m_q)[1] != "try-error") {
    coef <- m_q$coefficients["did_term"]
    # Get approximate p-value from summary
    coef_val <- ifelse(is.na(coef), NA, coef)
  } else {
    coef_val <- NA
  }
  
  cat(sprintf("%s percentile (τ=%.2f): DiD coef = %.4f\n", q_label, q, coef_val))
  
  quantile_results[[q_label]] <- tibble(
    Quantile = q_label,
    Tau = q,
    DiD_Coefficient = coef_val
  )
}

quantile_results_df <- bind_rows(quantile_results)

# ============================================================================
# 6. INSTITUTIONAL QUALITY MODERATION
# ============================================================================

cat("\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n6. INSTITUTIONAL QUALITY MODERATION\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n\n")

# Create institutional quality proxy from GEM data
# Use national-level TEA as proxy for institutional friendliness
country_tiers <- aps_did %>%
  group_by(country_id) %>%
  summarise(
    mean_tea_baseline = mean(as.numeric(unclass(tea))[year < 2012], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    inst_tier = case_when(
      mean_tea_baseline < quantile(mean_tea_baseline, 0.33, na.rm=TRUE) ~ "Low",
      mean_tea_baseline >= quantile(mean_tea_baseline, 0.33, na.rm=TRUE) &
        mean_tea_baseline < quantile(mean_tea_baseline, 0.67, na.rm=TRUE) ~ "Medium",
      TRUE ~ "High"
    )
  )

aps_did <- aps_did %>%
  left_join(country_tiers %>% select(country_id, inst_tier), by = "country_id")

cat("Institutional tiers (based on baseline TEA):\n\n")

inst_results <- list()
for(tier in c("Low", "Medium", "High")) {
  m_tier <- try(felm(tea ~ did_term + treated + post_reform |
                       as.factor(country_id) + as.factor(year),
                     data = aps_did %>% filter(inst_tier == tier, !is.na(tea))),
                silent = TRUE)
  
  if(class(m_tier)[1] != "try-error") {
    coef <- summary(m_tier)$coefficients["did_term", "Estimate"]
    pval <- summary(m_tier)$coefficients["did_term", "Pr(>|t|)"]
  } else {
    coef <- pval <- NA
  }
  
  cat(sprintf("%s institutions: DiD coef = %.4f, p-value = %.4f %s\n",
              tier, coef, pval,
              ifelse(!is.na(pval) && pval < 0.05, "**",
                     ifelse(!is.na(pval) && pval < 0.10, "*", ""))))
  
  inst_results[[tier]] <- tibble(
    Institution_Tier = tier,
    DiD_Coefficient = coef,
    p_value = pval
  )
}

inst_results_df <- bind_rows(inst_results)

# ============================================================================
# 7. PLACEBO/ROBUSTNESS: FALSE REFORM YEARS
# ============================================================================

cat("\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n7. PLACEBO TEST: FALSE REFORM YEARS\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n\n")

# Test with fake reform years (2 years before actual reform)
aps_placebo <- aps_did %>%
  mutate(
    reform_year_fake = reform_year - 2,
    post_reform_fake = ifelse(treated == 1 & year >= reform_year_fake, 1, 0),
    did_term_fake = treated * post_reform_fake
  )

m_placebo <- felm(tea ~ did_term_fake + treated + post_reform_fake |
                    as.factor(country_id) + as.factor(year),
                  data = aps_placebo)

placebo_coef <- summary(m_placebo)$coefficients["did_term_fake", "Estimate"]
placebo_p <- summary(m_placebo)$coefficients["did_term_fake", "Pr(>|t|)"]

cat("Placebo DiD (reform 2 years early):\n")
cat(sprintf("  DiD coef: %.4f, p-value: %.4f\n", placebo_coef, placebo_p))
cat(sprintf("  Expected: NOT significant (pre-trend would show up)\n\n")
)

if(placebo_p > 0.10) {
  cat("✓ PASS: Placebo effect not significant. Pre-trends ruled out.\n\n")
} else {
  cat("⚠ WARNING: Placebo effect significant. Pre-trends may be present.\n\n")
}

# ============================================================================
# 8. SPILLOVER EFFECTS WITHIN COUNTRIES
# ============================================================================

cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n8. SPILLOVER EFFECTS (WITHIN-COUNTRY DIFFUSION)\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n\n")

# Within treated countries, compare individuals:
# - With high agency (expected to respond)
# - With low agency (expected NOT to respond directly, but spillover?)

aps_treated <- aps_did %>%
  filter(treated == 1) %>%
  mutate(
    high_agency = as.numeric(agency >= median(agency, na.rm=TRUE)),
    spillover_window = post_reform
  )

m_spillover <- try(felm(tea ~ high_agency * post_reform + post_reform + high_agency |
                          as.factor(country_id) + as.factor(year),
                        data = aps_treated %>% filter(!is.na(high_agency), !is.na(post_reform))),
                   silent = TRUE)

if(class(m_spillover)[1] != "try-error") {
  coefs <- summary(m_spillover)$coefficients
  int_idx <- which(rownames(coefs) == "high_agency:post_reform")
  
  if(length(int_idx) > 0) {
    spillover_coef <- coefs[int_idx, "Estimate"]
    spillover_p <- coefs[int_idx, "Pr(>|t|)"]
  } else {
    spillover_coef <- spillover_p <- NA
  }
} else {
  spillover_coef <- spillover_p <- NA
}

cat("Within treated countries (high vs low agency):\n")
cat(sprintf("  High Agency × Post-Reform: %.4f, p-value: %.4f %s\n\n",
            spillover_coef, spillover_p,
            ifelse(!is.na(spillover_p) && spillover_p < 0.05, "**",
                   ifelse(!is.na(spillover_p) && spillover_p < 0.10, "*", ""))))

if(!is.na(spillover_p) && spillover_p < 0.05) {
  cat("✓ HIGH AGENCY individuals respond more in post-reform period\n")
  cat("  (Confirms mechanism: agency drives response to reforms)\n\n")
}

# ============================================================================
# SAVE ALL RESULTS
# ============================================================================

cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n9. SAVING RESULTS\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n\n")

# Comprehensive results table
all_results <- tibble(
  Analysis = c(
    "Gender: Males", "Gender: Females", "Gender: Female×DiD",
    "Age: 18-24", "Age: 25-30", "Age: 31-39", "Age: 40-49", "Age: 50+",
    rep("Agency Components", 4),
    "Reform Intensity",
    "Placebo Test",
    "Spillover Effect"
  ),
  Subgroup = c(
    "Males", "Females", "Female interaction",
    "18-24", "25-30", "31-39", "40-49", "50+",
    component_names,
    "Multiple reforms",
    "False reform year",
    "High agency (treated)"
  ),
  Coefficient = c(
    male_coef, female_coef, m_female_coef,
    age_results_df$Coefficient,
    component_results_df$DiD_Interaction,
    intensity_coef,
    placebo_coef,
    spillover_coef
  ),
  p_value = c(
    male_p, female_p, m_female_p,
    age_results_df$p_value,
    component_results_df$p_value,
    n_reforms_p,
    placebo_p,
    spillover_p
  ),
  Significant = c(
    male_p < 0.05, female_p < 0.05, m_female_p < 0.05,
    age_results_df$p_value < 0.05,
    component_results_df$p_value < 0.05,
    n_reforms_p < 0.05,
    placebo_p < 0.05,
    spillover_p < 0.05
  )
)

write_csv(all_results, "output/did_results/10_comprehensive_heterogeneity_results.csv")

# Save separate detailed results
write_csv(gender_results, "output/did_results/10a_gender_heterogeneity.csv")
write_csv(age_results_df, "output/did_results/10b_age_subgroups.csv")
write_csv(component_results_df, "output/did_results/10c_agency_components.csv")
write_csv(tibble(
  Reform_Intensity_Analysis = c("DiD coefficient", "n_reforms coefficient"),
  Estimate = c(intensity_coef, n_reforms_coef),
  p_value = c(intensity_p, n_reforms_p)
), "output/did_results/10d_reform_intensity.csv")
write_csv(quantile_results_df, "output/did_results/10e_quantile_analysis.csv")
write_csv(inst_results_df, "output/did_results/10f_institutional_moderation.csv")
write_csv(tibble(
  Robustness_Check = c("Placebo (false reform)", "Spillover effect"),
  Coefficient = c(placebo_coef, spillover_coef),
  p_value = c(placebo_p, spillover_p),
  Interpretation = c(
    "Should NOT be significant",
    "Should be significant (confirms mechanism)"
  )
), "output/did_results/10g_robustness_checks.csv")

cat("✓ Main results: 10_comprehensive_heterogeneity_results.csv\n")
cat("✓ Gender: 10a_gender_heterogeneity.csv\n")
cat("✓ Age groups: 10b_age_subgroups.csv\n")
cat("✓ Components: 10c_agency_components.csv\n")
cat("✓ Intensity: 10d_reform_intensity.csv\n")
cat("✓ Quantiles: 10e_quantile_analysis.csv\n")
cat("✓ Institutions: 10f_institutional_moderation.csv\n")
cat("✓ Robustness: 10g_robustness_checks.csv\n\n")

# Save DiD dataset with all new variables for downstream use
saveRDS(aps_did, "output/did_results/aps_did_comprehensive.rds")
cat("✓ Full dataset: aps_did_comprehensive.rds\n\n")

cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n✓ COMPREHENSIVE ANALYSIS COMPLETE\n")
cat("═" %>% rep(70) %>% paste(collapse=""))
cat("\n\n")
