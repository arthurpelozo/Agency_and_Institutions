################################################################################
# PHASE 26: POLICY SHOCK PROXY (DiD USING BIG NECI IMPROVERS)
################################################################################
# Purpose: Data-driven proxy for policy shocks by flagging countries with
#          largest NECI improvements between 2017 and 2019, then running DiD.
# Note:    This is a proxy until concrete policy dates are sourced externally.
################################################################################

cat("\n")
cat("================================================================================\n")
cat("PHASE 26: POLICY SHOCK PROXY (DiD)\n")
cat("================================================================================\n")
cat("Flagging large NECI improvers as treated; estimating DiD on TEA rates...\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(lfe)
  library(stargazer)
})

# Load balanced panel
panel <- read_csv("data_processed/panel/panel_2017_2019_balanced.csv", show_col_types = FALSE)

cat("[1] Aggregate country-year metrics (TEA, agency, NECI)...\n")
cat("================================================================================\n")

country_year <- panel %>%
  group_by(country, year) %>%
  summarise(
    tea_rate = weighted.mean(tea_binary, weight, na.rm = TRUE),
    agency_mean = weighted.mean(agency_index, weight, na.rm = TRUE),
    neci1 = mean(neci1_std, na.rm = TRUE),
    neci2 = mean(neci2_std, na.rm = TRUE),
    neci3 = mean(neci3_std, na.rm = TRUE),
    nes_inq = mean(nes_inq_std, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, year)

# Keep countries with NECI data in 2017 and 2019
neci_span <- country_year %>%
  group_by(country) %>%
  summarise(
    neci1_2017 = neci1[year == 2017],
    neci1_2019 = neci1[year == 2019],
    .groups = "drop"
  ) %>%
  filter(!is.na(neci1_2017), !is.na(neci1_2019)) %>%
  mutate(delta_neci1 = neci1_2019 - neci1_2017)

cat(sprintf("  Countries with NECI 2017 & 2019: %d\n", nrow(neci_span)))

if (nrow(neci_span) == 0) {
  cat("\n⚠ NECI DATA LIMITATION:\n")
  cat("  - SPSS files contain NECI columns but all values are NA\n")
  cat("  - NECI 2019 data available in Excel (NECI 2019 FINAL VERSION_16012020.xlsx)\n")
  cat("  - NO historical NECI data for 2017/2018 found\n")
  cat("  - Cannot compute NECI change 2017→2019 for DiD analysis\n\n")
  cat("RECOMMENDATION:\n")
  cat("  Continue with other causal methods (FE ✓, Lagged ✓, GMM pending)\n")
  cat("  DiD requires either:\n")
  cat("    a) Historical NECI/institutional quality data (2017-2018)\n")
  cat("    b) Known policy reform dates from external sources\n\n")
  cat("================================================================================\n")
  cat("⏭ SKIPPING Phase 26 - Proceeding to Phase 27 (GMM)\n")
  cat("================================================================================\n\n")
  quit(save = "no", status = 0)
}

# Define treatment: top quartile of NECI improvement
if (nrow(neci_span) > 0) {
  threshold <- quantile(neci_span$delta_neci1, 0.75, na.rm = TRUE)
  neci_span <- neci_span %>% mutate(treated = delta_neci1 >= threshold)
} else {
  threshold <- NA
  neci_span <- neci_span %>% mutate(treated = FALSE)
}

cat(sprintf("  Treatment threshold (top 25%% delta): %.3f\n", threshold))
cat(sprintf("  Treated countries: %d\n", sum(neci_span$treated, na.rm = TRUE)))

cat("\n[2] Build DiD dataset (2017-2019)\n")
cat("================================================================================\n")

did_data <- country_year %>%
  inner_join(neci_span %>% select(country, treated), by = "country") %>%
  filter(year %in% c(2017, 2018, 2019)) %>%
  mutate(post = ifelse(year >= 2019, 1, 0),
         treat_post = treated * post)

cat(sprintf("  DiD rows: %d (countries=%d)\n", nrow(did_data), n_distinct(did_data$country)))

if (nrow(did_data) == 0 || sum(neci_span$treated, na.rm = TRUE) == 0) {
  cat("\n  ⚠ No NECI coverage across years; unable to run DiD proxy.\n")
  cat("  ACTION: Populate NES/NECI data for 2017 and 2019, or supply known policy dates.\n")
  quit(save = "no", status = 0)
}

cat("\n[3] Run DiD with country & year FE\n")
cat("================================================================================\n")

model_did <- felm(tea_rate ~ treat_post + agency_mean | country + year | 0 | country,
                  data = did_data)

summary_did <- summary(model_did)
coef_did <- coef(model_did)["treat_post"]
se_did <- summary_did$coefficients["treat_post", "Cluster s.e."]
p_did <- summary_did$coefficients["treat_post", "Pr(>|t|)"]

cat("\n  DiD results:\n")
cat(sprintf("    treat_post coefficient: %.4f\n", coef_did))
cat(sprintf("    Clustered SE: %.4f\n", se_did))
cat(sprintf("    p-value: %.4f\n", p_did))
cat(sprintf("    R-squared: %.4f\n", summary_did$r.squared))

cat("\n[4] Save outputs\n")
cat("================================================================================\n")

stargazer(model_did,
          type = "text",
          out = "output/tables/table26_did.txt",
          title = "DiD: NECI Improvers vs Others",
          dep.var.labels = "TEA rate",
          covariate.labels = c("Treat x Post", "Agency mean"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Treatment = top 25% NECI1 improvement (2017→2019). SE clustered by country.")

write_csv(neci_span, "output/tables/table26_neci_changes.csv")
write_csv(did_data, "data_processed/panel/did_dataset.csv")

cat("  ✓ Tables saved\n")

cat("\n")
cat("================================================================================\n")
cat("✓ PHASE 26: POLICY SHOCK PROXY COMPLETE (DATA-DRIVEN)\n")
cat("================================================================================\n")
cat(sprintf("  Treated countries: %d (top quartile NECI gains)\n", sum(neci_span$treated, na.rm = TRUE)))
cat(sprintf("  DiD coefficient: %.4f (p=%.4f)\n", coef_did, p_did))
cat("  NOTE: Replace proxy treatment with real policy dates when available.\n")
cat("================================================================================\n")
