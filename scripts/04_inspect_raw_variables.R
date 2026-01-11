# ============================================================================
# Inspect Raw Variables in GEM APS SPSS Files
# ============================================================================
# Purpose: Find true agency items (fear, skills, know, opportunity) in raw data
# ============================================================================

library(haven)
library(tidyverse)

wd <- "C:/Users/arthu/Downloads/Development Project/GEM_Institutional_Causality"
setwd(wd)

data_raw_path <- "data_raw"

cat("\n=== Inspecting Raw Variables in GEM APS Files ===\n\n")

# Load a few key years to examine variable names
years_to_inspect <- c(2013, 2014, 2017, 2019)

for (year_val in years_to_inspect) {
  
  # Find file for this year
  aps_files <- list.files(
    data_raw_path,
    pattern = sprintf("GEM %d APS Global.*Individual.*Level.*\\.sav$", year_val),
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(aps_files) > 0) {
    filepath <- aps_files[1]
    filename <- basename(filepath)
    
    cat(sprintf("=== %s (%s) ===\n", filename, year_val))
    
    tryCatch({
      df <- read_sav(filepath)
      
      # Convert to lowercase for easier searching
      col_names <- tolower(names(df))
      
      # Search for agency-related variables
      agency_patterns <- c(
        "fear", "afraid", "confidenc", "skill", "know", "opport",
        "effic", "self_eff", "risk", "perceiv", "readines", "ready"
      )
      
      cat("Agency-related columns:\n")
      for (pattern in agency_patterns) {
        matches <- col_names[grepl(pattern, col_names)]
        if (length(matches) > 0) {
          cat(sprintf("  Pattern '%s': %s\n", pattern, paste(matches, collapse = ", ")))
        }
      }
      
      # Show first 20 column names
      cat("\nFirst 20 columns:\n")
      cat(sprintf("  %s\n", paste(names(df)[1:min(20, length(names(df)))], collapse = "\n  ")))
      
      cat("\nTotal columns: ", ncol(df), "\n\n")
      
    }, error = function(e) {
      cat(sprintf("ERROR: %s\n\n", e$message))
    })
  } else {
    cat(sprintf("File not found for year %d\n\n", year_val))
  }
}

cat("\n=== Variable Name Patterns to Search ===\n")
cat("Common GEM variable prefixes:\n")
cat("  A_* or Q* = Demographics\n")
cat("  B_* = Knowledge/Perception questions\n")
cat("  C_* = Knowledge-based screening\n")
cat("  D_* = Opportunity perception\n")
cat("  E_* = Skills/fears/motivations\n")
cat("  F_* = Established business owners\n")
cat("  G_* = Intention/attitudes\n\n")

cat("Specific agency items typically include:\n")
cat("  1. Fear of failure (often in E_*)\n")
cat("  2. Know someone entrepreneur (often B_006 or similar)\n")
cat("  3. Have required skills (often E_*)\n")
cat("  4. Perceive good opportunity (often D_*)\n\n")

cat("=== Inspection Complete ===\n")
