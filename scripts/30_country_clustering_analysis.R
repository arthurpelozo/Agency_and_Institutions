################################################################################
# Phase 30: Country Clustering Analysis
# Identifies which institutional contexts make agency effects strongest
################################################################################

library(tidyverse)
library(lfe)

cat("════════════════════════════════════════════════════════════════════════\n")
cat("PHASE 30: COUNTRY CLUSTERING ANALYSIS\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

# Create output directories
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1. Load panel data
################################################################################

cat("1. LOADING PANEL DATA (2017-2019)\n")
cat("────────────────────────────────────────────────────────────────────────\n")

panel <- read_csv("data_processed/panel/panel_2017_2019_balanced.csv",
                  show_col_types = FALSE)

# Complete cases
panel_complete <- panel %>%
  filter(!is.na(agency_index), !is.na(tea_binary), !is.na(age), 
         !is.na(female), !is.na(gemeduc)) %>%
  rename(agency = agency_index, educ = gemeduc)

cat(sprintf("✓ Loaded: %d observations, %d countries, %d years\n",
            nrow(panel_complete),
            n_distinct(panel_complete$country),
            n_distinct(panel_complete$year)))

################################################################################
# 2. Country-specific agency effects
################################################################################

cat("\n2. ESTIMATING COUNTRY-SPECIFIC AGENCY EFFECTS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Estimate effect by country (pooling across years)
country_effects <- panel_complete %>%
  group_by(country) %>%
  summarize(
    n_obs = n(),
    n_years = n_distinct(year),
    mean_tea = mean(tea_binary, na.rm = TRUE),
    mean_agency = mean(agency, na.rm = TRUE),
    sd_agency = sd(agency, na.rm = TRUE)
  ) %>%
  filter(n_obs >= 100)  # Require at least 100 observations

cat(sprintf("✓ %d countries with n≥100\n", nrow(country_effects)))

# Estimate country-specific regressions
country_models <- list()
country_coefs <- data.frame()

for (ctry in country_effects$country) {
  ctry_data <- panel_complete %>%
    filter(country == ctry)
  
  # Simple regression within country
  tryCatch({
    model <- lm(tea_binary ~ agency + age + I(age^2) + female + educ,
                data = ctry_data)
    
    coef_agency <- coef(summary(model))["agency", "Estimate"]
    se_agency <- coef(summary(model))["agency", "Std. Error"]
    p_agency <- coef(summary(model))["agency", "Pr(>|t|)"]
    
    country_coefs <- bind_rows(country_coefs, data.frame(
      country = ctry,
      beta_agency = coef_agency,
      se_agency = se_agency,
      p_value = p_agency,
      n = nrow(ctry_data)
    ))
    
    country_models[[ctry]] <- model
  }, error = function(e) {
    cat(sprintf("  ⚠ Failed for %s: %s\n", ctry, e$message))
  })
}

# Merge with country characteristics
country_effects <- country_effects %>%
  left_join(country_coefs, by = "country")

cat(sprintf("✓ Estimated %d country-specific models\n", nrow(country_coefs)))

################################################################################
# 3. Classify countries by effect strength
################################################################################

cat("\n3. CLASSIFYING COUNTRIES BY EFFECT STRENGTH\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Create categories based on effect size and significance
country_effects <- country_effects %>%
  mutate(
    effect_sig = ifelse(p_value < 0.05, "Significant", "Not Significant"),
    effect_size = case_when(
      is.na(beta_agency) ~ "No estimate",
      beta_agency < 0 ~ "Negative",
      beta_agency < 0.05 ~ "Weak (0-0.05)",
      beta_agency < 0.15 ~ "Moderate (0.05-0.15)",
      TRUE ~ "Strong (>0.15)"
    ),
    effect_category = case_when(
      is.na(beta_agency) ~ "No estimate",
      p_value >= 0.05 ~ "Not significant",
      beta_agency < 0.05 ~ "Weak positive",
      beta_agency < 0.15 ~ "Moderate positive",
      TRUE ~ "Strong positive"
    )
  )

# Summary by category
cat("\nCountries by effect category:\n")
effect_summary <- country_effects %>%
  count(effect_category) %>%
  arrange(desc(n))
print(effect_summary)

cat("\nCountries by effect size (significant only):\n")
sig_summary <- country_effects %>%
  filter(p_value < 0.05) %>%
  count(effect_size) %>%
  arrange(desc(n))
print(sig_summary)

################################################################################
# 4. Top and bottom countries
################################################################################

cat("\n4. TOP AND BOTTOM COUNTRIES BY AGENCY EFFECT\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Top 10 countries (strongest positive effect)
top_countries <- country_effects %>%
  filter(p_value < 0.05) %>%
  arrange(desc(beta_agency)) %>%
  head(10)

cat("\nTOP 10: Strongest agency → TEA effects:\n")
print(top_countries %>%
        select(country, beta_agency, se_agency, p_value, n_obs, mean_tea) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Bottom 10 countries (weakest or negative)
bottom_countries <- country_effects %>%
  arrange(beta_agency) %>%
  head(10)

cat("\nBOTTOM 10: Weakest agency → TEA effects:\n")
print(bottom_countries %>%
        select(country, beta_agency, se_agency, p_value, n_obs, mean_tea) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

################################################################################
# 5. Institutional context analysis (if NECI available)
################################################################################

cat("\n5. INSTITUTIONAL CONTEXT ANALYSIS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Check for NECI variables
neci_vars <- names(panel_complete)[grepl("^NECI", names(panel_complete))]
neci_complete <- neci_vars[!sapply(panel_complete[neci_vars], function(x) all(is.na(x)))]

if (length(neci_complete) > 0) {
  cat(sprintf("✓ Found %d NECI variables with data\n", length(neci_complete)))
  
  # Aggregate NECI by country (2019 data)
  neci_country <- panel_complete %>%
    filter(year == 2019) %>%
    group_by(country) %>%
    summarize(across(all_of(neci_complete), ~mean(.x, na.rm = TRUE)))
  
  # Merge with effects
  country_effects <- country_effects %>%
    left_join(neci_country, by = "country")
  
  # Correlations between NECI and agency effect
  cat("\nCorrelations between institutional variables and agency effect:\n")
  for (neci_var in neci_complete[1:min(5, length(neci_complete))]) {
    tryCatch({
      # Check if we have enough valid observations
      valid_obs <- sum(!is.na(country_effects$beta_agency) & 
                       !is.na(country_effects[[neci_var]]))
      
      if (valid_obs >= 3) {
        cor_test <- cor.test(country_effects$beta_agency, 
                             country_effects[[neci_var]], 
                             use = "complete.obs")
        cat(sprintf("  %s: r=%.3f (p=%.3f) [n=%d]\n", 
                    neci_var, cor_test$estimate, cor_test$p.value, valid_obs))
      } else {
        cat(sprintf("  %s: insufficient data (n=%d)\n", neci_var, valid_obs))
      }
    }, error = function(e) {
      cat(sprintf("  %s: error - %s\n", neci_var, e$message))
    })
  }
} else {
  cat("⚠ No NECI data available for institutional context analysis\n")
  cat("  Analysis limited to country-specific effects\n")
}

################################################################################
# 6. Visualizations
################################################################################

cat("\n6. CREATING VISUALIZATIONS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Plot 1: Distribution of country-specific effects
p1 <- ggplot(country_effects %>% filter(!is.na(beta_agency)), 
             aes(x = beta_agency)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = median(country_effects$beta_agency, na.rm = TRUE),
             linetype = "dashed", color = "darkgreen") +
  labs(
    title = "Distribution of Country-Specific Agency Effects",
    subtitle = "Effect of agency on TEA across countries",
    x = "Agency coefficient (β)",
    y = "Number of countries"
  ) +
  theme_minimal()

ggsave("output/figures/fig_country_effects_distribution.png", p1, 
       width = 10, height = 6, dpi = 300)
cat("✓ Saved: output/figures/fig_country_effects_distribution.png\n")

# Plot 2: Effect size vs sample size
p2 <- ggplot(country_effects %>% filter(!is.na(beta_agency)), 
             aes(x = n_obs, y = beta_agency, color = effect_sig)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Significant" = "darkgreen", 
                                 "Not Significant" = "gray50")) +
  labs(
    title = "Agency Effect by Sample Size",
    subtitle = "Country-specific estimates",
    x = "Sample size (n observations)",
    y = "Agency coefficient (β)",
    color = "Significance (p<0.05)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/fig_country_effects_vs_n.png", p2, 
       width = 10, height = 6, dpi = 300)
cat("✓ Saved: output/figures/fig_country_effects_vs_n.png\n")

# Plot 3: Effect vs baseline TEA rate
p3 <- ggplot(country_effects %>% filter(!is.na(beta_agency)), 
             aes(x = mean_tea * 100, y = beta_agency, 
                 color = effect_category, size = n_obs)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_size_continuous(range = c(2, 8)) +
  labs(
    title = "Agency Effect vs Baseline Entrepreneurship Rate",
    subtitle = "Do agency effects vary with entrepreneurship levels?",
    x = "Mean TEA rate (%)",
    y = "Agency coefficient (β)",
    color = "Effect category",
    size = "Sample size"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave("output/figures/fig_country_effects_vs_tea.png", p3, 
       width = 12, height = 6, dpi = 300)
cat("✓ Saved: output/figures/fig_country_effects_vs_tea.png\n")

################################################################################
# 7. Key insights
################################################################################

cat("\n7. KEY INSIGHTS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

cat("\nCROSS-COUNTRY HETEROGENEITY:\n")
cat(sprintf("  • Effect range: %.3f to %.3f\n",
            min(country_effects$beta_agency, na.rm = TRUE),
            max(country_effects$beta_agency, na.rm = TRUE)))
cat(sprintf("  • Median effect: %.3f\n",
            median(country_effects$beta_agency, na.rm = TRUE)))
cat(sprintf("  • IQR: %.3f to %.3f\n",
            quantile(country_effects$beta_agency, 0.25, na.rm = TRUE),
            quantile(country_effects$beta_agency, 0.75, na.rm = TRUE)))
cat(sprintf("  • Significant effects: %d/%d countries (%.1f%%)\n",
            sum(country_effects$p_value < 0.05, na.rm = TRUE),
            sum(!is.na(country_effects$p_value)),
            100 * sum(country_effects$p_value < 0.05, na.rm = TRUE) / 
              sum(!is.na(country_effects$p_value))))

# Correlation between effect and baseline entrepreneurship
cor_tea_effect <- cor.test(country_effects$mean_tea, 
                            country_effects$beta_agency,
                            use = "complete.obs")
cat(sprintf("\nEFFECT vs BASELINE TEA:\n"))
cat(sprintf("  • Correlation: r=%.3f (p=%.3f)\n",
            cor_tea_effect$estimate, cor_tea_effect$p.value))
if (cor_tea_effect$p.value < 0.05) {
  if (cor_tea_effect$estimate > 0) {
    cat("  → Agency matters MORE in high-entrepreneurship countries\n")
  } else {
    cat("  → Agency matters LESS in high-entrepreneurship countries\n")
  }
} else {
  cat("  → No significant relationship with baseline TEA\n")
}

################################################################################
# 8. Save results
################################################################################

cat("\n8. SAVING RESULTS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

write_csv(country_effects, "output/tables/table30_country_effects.csv")
cat("✓ Saved: output/tables/table30_country_effects.csv\n")

# Save top/bottom lists
top_bottom <- bind_rows(
  top_countries %>% mutate(group = "Top 10"),
  bottom_countries %>% mutate(group = "Bottom 10")
)
write_csv(top_bottom, "output/tables/table30_top_bottom_countries.csv")
cat("✓ Saved: output/tables/table30_top_bottom_countries.csv\n")

# Save models
saveRDS(country_models, "output/tables/table30_country_models.rds")
cat("✓ Saved: output/tables/table30_country_models.rds\n")

cat("\n════════════════════════════════════════════════════════════════════════\n")
cat("PHASE 30 COMPLETE: Country Clustering Analysis\n")
cat("════════════════════════════════════════════════════════════════════════\n")
