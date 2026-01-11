# Quick inspection of Doing Business data structure
library(readxl)

db_file <- "data_raw/data-DiD/Historical-Data--DB04-DB20-.xlsx"

# Check all sheets
sheets <- excel_sheets(db_file)
cat("Available sheets:\n")
print(sheets)
cat("\n")

# Inspect DB20 Data sheet with different skip values
for(skip_rows in c(0, 1, 2, 3, 4, 5)) {
  cat(sprintf("\n=== Reading DB20 Data with skip=%d ===\n", skip_rows))
  
  data <- read_excel(db_file, sheet = "DB20 Data", skip = skip_rows, n_max = 5)
  
  cat(sprintf("Rows: %d, Cols: %d\n", nrow(data), ncol(data)))
  cat("First 3 column names:\n")
  print(names(data)[1:min(3, length(names(data)))])
  
  cat("\nFirst 2 rows:\n")
  print(head(data, 2))
}

# Try "Starting a Business" file (might be cleaner)
cat("\n\n=== CHECKING STARTING A BUSINESS FILE ===\n")
start_file <- "data_raw/data-DiD/Starting a Business.xlsx"

if(file.exists(start_file)) {
  sheets_start <- excel_sheets(start_file)
  cat("Sheets in Starting a Business:\n")
  print(sheets_start)
  
  # Try first data sheet
  for(i in 1:min(2, length(sheets_start))) {
    cat(sprintf("\n--- Sheet: %s ---\n", sheets_start[i]))
    data_start <- read_excel(start_file, sheet = sheets_start[i], n_max = 5)
    cat(sprintf("Rows: %d, Cols: %d\n", nrow(data_start), ncol(data_start)))
    cat("Column names:\n")
    print(names(data_start))
    cat("\nFirst 2 rows:\n")
    print(head(data_start, 2))
  }
}
