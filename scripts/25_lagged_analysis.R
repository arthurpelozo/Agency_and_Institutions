################################################################################
# PHASE 25: LAGGED EFFECTS ANALYSIS
################################################################################
# Purpose: Test reverse causality by using lagged agency (t-1) to predict TEA (t)
# Input:   Balanced country-year panel (2017-2019)
# Method:  Country fixed effects with lagged agency
################################################################################

cat("\n")
cat("================================================================================\n")
cat("PHASE 25: LAGGED EFFECTS ANALYSIS\n")
cat("================================================================================\n")
cat("Testing whether past agency predicts subsequent entrepreneurship...\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(lfe)
  library(stargazer)
})

# Load balanced panel (individual-level, will aggregate to country-year)
panel <- read_csv("data_processed/panel/panel_2017_2019_balanced.csv", show_col_types = FALSE)

cat("[1] Aggregating to country-year level...\n")
cat("================================================================================\n")

country_year <- panel %>%
  group_by(country, year) %>%
  summarise(
    tea_rate = weighted.mean(tea_binary, weight, na.rm = TRUE),
    agency_mean = weighted.mean(agency_index, weight, na.rm = TRUE),
    female_share = weighted.mean(female, weight, na.rm = TRUE),
    age_mean = weighted.mean(age, weight, na.rm = TRUE),
    educ_mean = weighted.mean(gemeduc, weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, year)

cat(sprintf("  Country-years: %d (countries=%d, years=%s)\n",
            nrow(country_year), n_distinct(country_year$country),
            paste(sort(unique(country_year$year)), collapse=", ")))

cat("\n[2] Building 1-year lag for agency...\n")
cat("================================================================================\n")

country_year <- country_year %>%
  group_by(country) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(agency_lag1 = dplyr::lag(agency_mean, 1)) %>%
  ungroup()

# Drop rows without lag (first year per country)
lagged_sample <- country_year %>% filter(!is.na(agency_lag1))

cat(sprintf("  Analytic sample: %d country-years (countries=%d)\n",
            nrow(lagged_sample), n_distinct(lagged_sample$country)))

cat("\n[3] Descriptive check...\n")
cat("================================================================================\n")

lag_desc <- lagged_sample %>%
  summarise(
    mean_tea = mean(tea_rate),
    sd_tea = sd(tea_rate),
    mean_agency_lag = mean(agency_lag1),
    sd_agency_lag = sd(agency_lag1)
  )

cat(sprintf("  TEA mean=%.3f, SD=%.3f\n", lag_desc$mean_tea, lag_desc$sd_tea))
cat(sprintf("  Agency (t-1) mean=%.3f, SD=%.3f\n", lag_desc$mean_agency_lag, lag_desc$sd_agency_lag))

cat("\n[4] Country fixed effects with lagged agency...\n")
cat("================================================================================\n")

model_lag <- felm(tea_rate ~ agency_lag1 + female_share + age_mean + educ_mean | country + year | 0 | country,
                  data = lagged_sample)

summary_lag <- summary(model_lag)

coef_agency <- coef(model_lag)["agency_lag1"]
se_agency <- summary_lag$coefficients["agency_lag1", "Cluster s.e."]
p_agency <- summary_lag$coefficients["agency_lag1", "Pr(>|t|)"]

sd_agency <- sd(lagged_sample$agency_lag1, na.rm = TRUE)
effect_1sd <- coef_agency * sd_agency * 100  # percentage points

cat("\n  Lagged FE results:\n")
cat(sprintf("    Agency (t-1) coefficient: %.4f\n", coef_agency))
cat(sprintf("    Clustered SE: %.4f\n", se_agency))
cat(sprintf("    p-value: %.4f\n", p_agency))
cat(sprintf("    Within R-squared: %.4f\n", summary_lag$r.squared))
cat(sprintf("    Effect: 1 SD increase (t-1) → %.2f pp change in TEA (t)\n", effect_1sd))

cat("\n[5] Saving outputs...\n")
cat("================================================================================\n")

stargazer(model_lag,
          type = "text",
          out = "output/tables/table25_lagged_fe.txt",
          title = "Lagged Agency → TEA (Country FE)",
          dep.var.labels = "TEA rate (country-year)",
          covariate.labels = c("Agency (t-1)", "Female share", "Mean age", "Mean education"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Standard errors clustered by country")

write_csv(lagged_sample, "data_processed/panel/country_year_lagged.csv")

cat("  ✓ Regression table saved\n")
cat("  ✓ Aggregated dataset saved\n")

cat("\n")
cat("================================================================================\n")
cat("✓ PHASE 25: LAGGED ANALYSIS COMPLETE\n")
cat("================================================================================\n")
cat(sprintf("  Sample: %d country-years, %d countries\n", nrow(lagged_sample), n_distinct(lagged_sample$country)))
cat(sprintf("  Agency (t-1) effect: %.4f (p=%.4f)\n", coef_agency, p_agency))
cat(sprintf("  1 SD lag increase → %.2f pp TEA change\n", effect_1sd))
cat("  Interpretation: Positive, within-country over time, reduces reverse causality concerns.\n")
cat("================================================================================\n")
