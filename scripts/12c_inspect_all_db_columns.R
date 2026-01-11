# Inspect ALL columns in Historical Data to find different year ranges
library(readxl)
library(dplyr)
library(tidyr)

# Load the file
db_file <- "data_raw/data-DiD/Historical-Data--DB04-DB20-.xlsx"

cat(paste(rep("=", 60), collapse=""), "\n")
cat("INSPECTING ALL COLUMNS IN DB21 DATA SHEET\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

# Read with skip=4 to get past headers
db_raw <- read_excel(db_file, sheet = "DB21 Data", skip = 4)

cat("Total columns found:", ncol(db_raw), "\n\n")

# Get all column names
all_cols <- names(db_raw)

cat("ALL COLUMN NAMES:\n")
cat(paste(rep("-", 60), collapse=""), "\n")
for(i in 1:length(all_cols)) {
  cat(sprintf("%2d. %s\n", i, all_cols[i]))
}

cat("\n\n")
cat(paste(rep("=", 60), collapse=""), "\n")
cat("COLUMNS WITH 'DB' IN NAME (methodology periods):\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

db_cols <- all_cols[grepl("DB", all_cols, ignore.case = TRUE)]
for(col in db_cols) {
  cat("- ", col, "\n")
}

cat("\n\n")
cat(paste(rep("=", 60), collapse=""), "\n")
cat("SAMPLE DATA FOR EACH DB SCORE COLUMN:\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

# Find columns that likely contain scores (looking for patterns like "score", "rank", etc.)
score_cols <- all_cols[grepl("score|rank", all_cols, ignore.case = TRUE)]

for(col in score_cols) {
  cat("Column:", col, "\n")
  sample_data <- db_raw %>%
    select(Economy, all_of(col)) %>%
    filter(!is.na(.data[[col]])) %>%
    head(5)
  
  print(sample_data)
  cat("\nNon-NA values:", sum(!is.na(db_raw[[col]])), "out of", nrow(db_raw), "\n")
  if(is.numeric(db_raw[[col]])) {
    cat("Range:", min(db_raw[[col]], na.rm=TRUE), "to", max(db_raw[[col]], na.rm=TRUE), "\n")
  }
  cat("\n")
}

cat("\n\n")
cat(paste(rep("=", 60), collapse=""), "\n")
cat("CHECKING DB20 DATA SHEET FOR COMPARISON:\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

db20_raw <- read_excel(db_file, sheet = "DB20 Data", skip = 4)
cat("DB20 sheet columns:", ncol(db20_raw), "\n\n")

db20_cols <- names(db20_raw)
db20_db_cols <- db20_cols[grepl("DB", db20_cols, ignore.case = TRUE)]

cat("DB-related columns in DB20 sheet:\n")
for(col in db20_db_cols) {
  cat("- ", col, "\n")
}

cat("\n\nDONE\n")
