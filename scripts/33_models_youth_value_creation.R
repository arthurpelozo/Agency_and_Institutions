# ============================================================
# Youth Value Creation: Core Econometric Models
# Uses processed youth APS panel (18-30) stacked 2009-2020
# ============================================================

library(tidyverse)
library(fixest)
library(haven)
library(broom)

wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

ind_file <- "data_processed/aps_youth_panel_2005_2021.rds"
agg_file <- "data_processed/aps_youth_country_year_2005_2021.rds"

out_txt <- "output/model_youth_value_creation.txt"
out_csv <- "output/model_youth_value_creation_coefs.csv"

cat("\n=== Loading data ===\n")
df <- readRDS(ind_file)

# Keep years we successfully loaded
valid_years <- c(2009, 2010, 2011, 2013, 2014, 2016, 2017, 2019, 2020)
df <- df %>% filter(year %in% valid_years)

# Clean controls
# gemeduc may be missing; gemhhinc, gemoccu, gemwork often present in later years only

# Define helper for safe factor country/year
df_mod <- df %>%
  mutate(
    country = as.factor(country),
    year = as.factor(year),
    female = ifelse(gender == 2, 1, 0),
    gemwork_num = as.numeric(zap_labels(gemwork)),
    employed = ifelse(!is.na(gemwork_num) & gemwork_num == 1, 1, 0)
  )

# Baseline model: TEA ~ agency + controls + country FE + year FE
cat("Running baseline logit with country/year FE...\n")
m1 <- feglm(tea_binary ~ agency_index + female + age + gemeduc | country + year,
            data = df_mod, family = binomial(), cluster = ~country)

# Components model
cat("Running components model...\n")
m2 <- feglm(tea_binary ~ I(1 - fear_binary) + skill_binary + know_binary + opport_binary +
              female + age + gemeduc | country + year,
            data = df_mod, family = binomial(), cluster = ~country)

# Gender interaction
cat("Running gender interaction model...\n")
m3 <- feglm(tea_binary ~ agency_index * female + age + gemeduc | country + year,
            data = df_mod, family = binomial(), cluster = ~country)

# Employment interaction (where gemwork available)
df_emp <- df_mod %>% filter(!is.na(gemwork_num))
m4 <- feglm(tea_binary ~ agency_index * employed + female + age + gemeduc | country + year,
            data = df_emp, family = binomial(), cluster = ~country)

# Pre/post 2018 stability (proxy for methodology change)
df_band <- df_mod %>% mutate(post2018 = ifelse(year %in% c("2019", "2020"), 1, 0))
m5 <- feglm(tea_binary ~ agency_index * post2018 + female + age + gemeduc | country + year,
            data = df_band, family = binomial(), cluster = ~country)

# Year-band stability (three bands, drop year FE to avoid collinearity)
df_band3 <- df_mod %>% mutate(
  band = case_when(
    year %in% c("2009", "2010", "2011", "2013", "2014") ~ "2009_2014",
    year %in% c("2016", "2017") ~ "2016_2017",
    TRUE ~ "2019_2020"
  )
)
m6 <- feglm(tea_binary ~ agency_index * band + female + age + gemeduc | country,
            data = df_band3, family = binomial(), cluster = ~country)

# Collect summaries
sink(out_txt)
cat("Youth Value Creation Core Models\n")
cat("Generated: ", Sys.time(), "\n\n")

cat("Model 1: Agency baseline (country/year FE)\n")
print(summary(m1))
cat("\n---\n\n")

cat("Model 2: Components (inverse fear, skill, know, opport)\n")
print(summary(m2))
cat("\n---\n\n")

cat("Model 3: Gender interaction\n")
print(summary(m3))
cat("\n---\n\n")

cat("Model 4: Employment interaction (subset with gemwork)\n")
print(summary(m4))
cat("\n---\n\n")

cat("Model 5: Post-2018 stability interaction\n")
print(summary(m5))
cat("\n")

cat("---\n\n")

cat("Model 6: Year-band stability (country FE, no year FE)\n")
print(summary(m6))
cat("\n")
sink()

# Export coefficient table
models_named <- list(
  m1 = m1,
  m2 = m2,
  m3 = m3,
  m4 = m4,
  m5 = m5,
  m6 = m6
)

coef_tbl <- bind_rows(lapply(names(models_named), function(nm) {
  broom::tidy(models_named[[nm]]) %>% mutate(model = nm)
}))

write_csv(coef_tbl, out_csv)

cat("\nSaved:\n")
cat(sprintf("  %s\n", out_txt))
cat(sprintf("  %s\n", out_csv))
cat("Done.\n")
