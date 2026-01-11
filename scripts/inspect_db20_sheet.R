library(readxl)
library(tidyverse)

# Path to the historical data file
db_file <- "data_raw/data-DiD/Historical-Data--DB04-DB20-.xlsx"

# List all sheets
cat("Available sheets:\n")
sheet_names <- excel_sheets(db_file)
print(sheet_names)

# Read DB20 sheet
cat("\n\n=== READING DB20 SHEET ===\n")
db20 <- read_excel(db_file, sheet = "DB20 Data", skip = 0)

cat("\nFirst 5 rows of DB20 sheet:\n")
print(db20[1:5, ])

cat("\n\nColumn names (all of them):\n")
print(names(db20))

cat("\n\nData dimensions:\n")
print(dim(db20))

cat("\n\nFirst few rows with column info:\n")
glimpse(db20)

cat("\n\nSample of data (first 3 countries, all columns):\n")
print(db20[1:3, ])

# Check for columns that might represent different year periods
cat("\n\nLooking for columns with DB methodology periods...\n")
method_cols <- names(db20)[grepl("DB", names(db20))]
cat("Columns containing 'DB':\n")
print(method_cols)
