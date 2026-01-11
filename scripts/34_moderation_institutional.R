# ============================================================
# Youth Value Creation: Institutional Moderation Models
# Simplified: focus on Doing Business + youth panel analysis
# ============================================================

library(tidyverse)
library(readxl)

wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

cat("\n=== Loading country-year youth panel ===\n")
cy_data <- readRDS("data_processed/aps_youth_country_year_2005_2021.rds")
cat(sprintf("Loaded: %d country-year obs\n", nrow(cy_data)))

# ============================================================
# LOAD DOING BUSINESS DATA
# ============================================================

cat("\n=== Loading Doing Business data ===\n")

db_file <- "data_raw/data-DiD/Historical-Data--DB04-DB20-.xlsx"
db_df <- NULL

if (file.exists(db_file)) {
  tryCatch({
    db_raw <- read_excel(db_file, sheet = 1) %>% as_tibble()
    
    # Get first column name (usually "Economy")
    first_col <- names(db_raw)[1]
    year_cols <- names(db_raw)[grepl("^\\d{4}", names(db_raw))]
    
    cat(sprintf("Found %d year columns\n", length(year_cols)))
    
    db_df <- db_raw %>%
      select(all_of(first_col), all_of(year_cols)) %>%
      pivot_longer(cols = all_of(year_cols),
                   names_to = "year", values_to = "db_score") %>%
      mutate(
        country = tolower(as.character(.data[[first_col]])),
        year = as.numeric(year),
        db_score = as.numeric(db_score)
      ) %>%
      select(country, year, db_score) %>%
      filter(!is.na(db_score))
    
    cat(sprintf("Loaded %d DB country-years\n", nrow(db_df)))
  }, error = function(e) {
    cat("Error loading DB: ", e$message, "\n")
  })
}

# ============================================================
# MERGE TO YOUTH PANEL
# ============================================================

cat("\n=== Merging data ===\n")

cy_merged <- cy_data

if (!is.null(db_df)) {
  cy_merged <- cy_merged %>%
    left_join(db_df, by = c("country", "year"))
  cat(sprintf("Merged: %d with DB scores\n", sum(!is.na(cy_merged$db_score))))
}

# ============================================================
# MODERATION MODELS
# ============================================================

cat("\n=== Fitting moderation models ===\n")

cy_mod <- cy_merged %>%
  mutate(
    agency_z = as.numeric(scale(agency_mean)[, 1]),
    db_score_z = as.numeric(scale(db_score)[, 1])
  )

out_txt <- "output/model_moderation_youth.txt"
sink(out_txt)

cat("Youth Value Creation: Institutional Moderation Models\n")
cat("Country-year level (N = ", nrow(cy_mod), ")\n", sep="")
cat("Generated: ", Sys.time(), "\n\n")

# M1: Baseline
cat("Model 1: Agency → TEA (baseline)\n")
m1 <- lm(tea_pct ~ agency_mean, data = cy_mod)
print(summary(m1))
cat("\n---\n\n")

# M2: With year FE
cat("Model 2: Agency → TEA (year FE)\n")
m2 <- lm(tea_pct ~ agency_mean + factor(year), data = cy_mod)
print(summary(m2))
cat("\n---\n\n")

# M3: Components
cat("Model 3: Components breakdown\n")
m3 <- lm(tea_pct ~ skill_pct + know_pct + opport_pct + scale(fear_pct) + factor(year),
         data = cy_mod)
print(summary(m3))
cat("\n---\n\n")

# M4: Agency × DB interaction (if DB data available)
if (!is.null(db_df)) {
  cy_db <- cy_mod %>% filter(!is.na(db_score_z))
  
  cat("Model 4: Agency × DB Score interaction\n")
  cat("N = ", nrow(cy_db), "\n\n", sep="")
  m4 <- lm(tea_pct ~ agency_z * db_score_z + factor(year), data = cy_db)
  print(summary(m4))
  cat("\n---\n\n")
  
  # M5: Components × DB
  cat("Model 5: Components × DB Score\n")
  cat("N = ", nrow(cy_db), "\n\n", sep="")
  m5 <- lm(tea_pct ~ scale(skill_pct) * db_score_z + 
                    scale(know_pct) * db_score_z + 
                    scale(opport_pct) * db_score_z + 
                    factor(year),
           data = cy_db)
  print(summary(m5))
  cat("\n")
}

sink()

cat("\nSaved: ", out_txt, "\n", sep="")

# Export merged data
saveRDS(cy_merged, "data_processed/aps_youth_country_year_with_covariates.rds")

# Summary stats
cat("\n=== Summary Statistics ===\n")
print(cy_merged %>%
  summarise(
    n_obs = n(),
    n_countries = n_distinct(country),
    n_years = n_distinct(year),
    mean_agency = mean(agency_mean, na.rm = TRUE),
    mean_tea = mean(tea_pct, na.rm = TRUE),
    mean_db_score = mean(db_score, na.rm = TRUE)
  ))

cat("\n=== Complete ===\n")

# Try to load 2019 NES (most recent complete data)
neci_file <- "data_raw/GEM 2019 NES ALL LEVELS_NECI UPDATED_13012020.sav"
neci_df <- NULL

if (file.exists(neci_file)) {
  cat("Loading NECI from 2019 NES...\n")
  neci_raw <- read_sav(neci_file) %>% as_tibble()
  names(neci_raw) <- tolower(names(neci_raw))
  
  # Try standard country column first
  if ("country" %in% names(neci_raw)) {
    neci_df <- neci_raw %>%
      select(country, matches("neci|^nes_")) %>%
      distinct(country, .keep_all = TRUE) %>%
      mutate(country = tolower(as.character(country)))
    cat(sprintf("Extracted %d countries\n", nrow(neci_df)))
  } else {
    # Try alternative country identifiers
    country_cols <- names(neci_raw)[grepl("economy|country_name|nation", names(neci_raw))]
    if (length(country_cols) > 0) {
      country_col <- country_cols[1]
      cat(sprintf("Using column '%s' as country\n", country_col))
      neci_df <- neci_raw %>%
        select(all_of(country_col), matches("neci")) %>%
        rename(country = all_of(country_col)) %>%
        distinct(country, .keep_all = TRUE) %>%
        mutate(country = tolower(as.character(country)))
      cat(sprintf("Extracted %d countries\n", nrow(neci_df)))
    } else {
      cat("No NECI data extractable; skipping.\n")
    }
  }
} else {
  cat("NECI file not found, will skip.\n")
}

# 2. Doing Business scores (DB_score_* by year)
db_file <- "data_raw/data-DiD/Historical-Data--DB04-DB20-.xlsx"
if (file.exists(db_file)) {
  cat("\nLoading Doing Business scores...\n")
  db_raw <- read_excel(db_file)
  
  db_df <- db_raw %>%
    rename_with(tolower) %>%
    select(economy, matches("^\\d{4}")) %>%
    pivot_longer(cols = matches("^\\d{4}"), 
                 names_to = "year", values_to = "db_score") %>%
    mutate(
      country = tolower(as.character(economy)),
      year = as.numeric(year)
    ) %>%
    select(country, year, db_score) %>%
    filter(!is.na(db_score))
  
  cat(sprintf("Loaded %d DB country-years\n", nrow(db_df)))
} else {
  cat("DB file not found, will skip.\n")
  db_df <- NULL
}

# 3. Global Findex (financial inclusion)
findex_file <- "data_raw/data_causality/GlobalFindexDatabase2025.xlsx"
if (file.exists(findex_file)) {
  cat("\nLoading Global Findex...\n")
  findex_raw <- read_excel(findex_file, sheet = 1)
  
  # Usually has country, year, and multiple indicators
  # Typically has account ownership, digital payments, etc.
  findex_df <- findex_raw %>%
    rename_with(tolower) %>%
    select(economy, year, matches("account|financial|digital")) %>%
    mutate(
      country = tolower(as.character(economy)),
      year = as.numeric(year)
    ) %>%
    select(country, year, everything(), -economy) %>%
    filter(!is.na(year))
  
  cat(sprintf("Loaded %d Findex country-years\n", nrow(findex_df)))
} else {
  cat("Findex file not found, will skip.\n")
  findex_df <- NULL
}

# 4. Business registration data (new firm density)
reg_file <- "data_raw/data_causality/API_IC.BUS.NDNS.ZS_DS2_en_excel_v2_426.xls"
if (file.exists(reg_file)) {
  cat("\nLoading business registration data...\n")
  reg_raw <- read_excel(reg_file, sheet = 1)
  
  reg_df <- reg_raw %>%
    rename_with(tolower) %>%
    select(`country name`, `country code`, matches("^\\d{4}")) %>%
    pivot_longer(cols = matches("^\\d{4}"), 
                 names_to = "year", values_to = "firm_density") %>%
    mutate(
      country = tolower(as.character(`country name`)),
      year = as.numeric(year)
    ) %>%
    select(country, year, firm_density) %>%
    filter(!is.na(firm_density))
  
  cat(sprintf("Loaded %d firm density country-years\n", nrow(reg_df)))
} else {
  cat("Firm density file not found, will skip.\n")
  reg_df <- NULL
}

# 5. ILO youth NEET data
ilo_file <- "data_raw/data_causality/EIP_2EET_SEX_RT_A-20260111T0112.csv"
if (file.exists(ilo_file)) {
  cat("\nLoading ILO youth NEET...\n")
  neet_raw <- read_csv(ilo_file)
  
  neet_df <- neet_raw %>%
    rename_with(tolower) %>%
    select(country, year, obs_value) %>%
    mutate(
      country = tolower(as.character(country)),
      year = as.numeric(year),
      youth_neet = as.numeric(obs_value)
    ) %>%
    select(country, year, youth_neet) %>%
    filter(!is.na(youth_neet))
  
  cat(sprintf("Loaded %d NEET country-years\n", nrow(neet_df)))
} else {
  cat("NEET file not found, will skip.\n")
  neet_df <- NULL
}

# ============================================================
# MERGE TO YOUTH PANEL
# ============================================================

cat("\n=== Merging covariates to youth panel ===\n")

# Start with youth panel
cy_merged <- cy_data

# Add DB scores
if (!is.null(db_df)) {
  cy_merged <- cy_merged %>%
    left_join(db_df, by = c("country", "year"))
}

# Add Findex
if (!is.null(findex_df)) {
  # Findex often has multiple indicators; keep first account measure
  findex_simple <- findex_df %>%
    select(country, year, account = contains("account")) %>%
    distinct(country, year, .keep_all = TRUE)
  cy_merged <- cy_merged %>%
    left_join(findex_simple, by = c("country", "year"))
}

# Add firm density
if (!is.null(reg_df)) {
  cy_merged <- cy_merged %>%
    left_join(reg_df, by = c("country", "year"))
}

# Add NEET (usually not year-specific but merged on country)
if (!is.null(neet_df)) {
  neet_recent <- neet_df %>%
    group_by(country) %>%
    slice_max(year, n = 1) %>%
    ungroup() %>%
    select(country, youth_neet)
  cy_merged <- cy_merged %>%
    left_join(neet_recent, by = "country")
}

# Add NECI (usually not year-specific)
if (!is.null(neci_df)) {
  # Extract numeric NECI if available
  neci_num <- neci_df %>%
    select(country, matches("neci")) %>%
    distinct(country, .keep_all = TRUE)
  cy_merged <- cy_merged %>%
    left_join(neci_num, by = "country")
}

cat(sprintf("\nMerged data: %d rows\n", nrow(cy_merged)))
cat(sprintf("Columns: %s\n", paste(names(cy_merged), collapse = ", "))

# ============================================================
# MODERATION MODELS
# ============================================================

cat("\n=== Fitting moderation models ===\n")

# Standardize key covariates for interpretation
cy_mod <- cy_merged %>%
  mutate(
    agency_z = scale(agency_mean)[, 1],
    db_score_z = scale(db_score, na.rm = TRUE)[, 1],
    firm_density_z = scale(firm_density, na.rm = TRUE)[, 1]
  )

out_txt <- "output/model_moderation_youth.txt"
sink(out_txt)

cat("Youth Value Creation: Institutional Moderation Models\n")
cat("Country-year level analysis (N = country-years)\n")
cat("Generated: ", Sys.time(), "\n\n")

# M1: Baseline country-year (agency → TEA)
cat("Model 1: Agency → TEA (country-year level)\n\n")
m1_cy <- lm(tea_pct ~ agency_mean + factor(year), data = cy_mod)
print(summary(m1_cy))
cat("\n---\n\n")

# M2: Agency × DB score interaction
cat("Model 2: Agency × Doing Business interaction\n\n")
m2_cy <- lm(tea_pct ~ agency_z * db_score_z + factor(year), 
            data = cy_mod %>% filter(!is.na(db_score_z)))
print(summary(m2_cy))
cat("\n---\n\n")

# M3: Agency × Firm density interaction
cat("Model 3: Agency × Firm Density interaction\n\n")
m3_cy <- lm(tea_pct ~ agency_z * firm_density_z + factor(year),
            data = cy_mod %>% filter(!is.na(firm_density_z)))
print(summary(m3_cy))
cat("\n---\n\n")

# M4: Components by year
cat("Model 4: Components breakdown by year (skill + know + opport effects)\n\n")
m4_cy <- lm(tea_pct ~ (skill_pct + know_pct + opport_pct) * factor(year),
            data = cy_mod)
print(summary(m4_cy))
cat("\n")

sink()

cat("\nSaved to:", out_txt, "\n")

# Export country-year merged data
saveRDS(cy_merged, "data_processed/aps_youth_country_year_with_covariates.rds")
cat("Saved merged data to: data_processed/aps_youth_country_year_with_covariates.rds\n")

cat("\n=== Moderation models complete ===\n")
