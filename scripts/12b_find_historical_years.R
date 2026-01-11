# Check all sheets in Historical Data file for older data
library(readxl)

db_file <- "data_raw/data-DiD/Historical-Data--DB04-DB20-.xlsx"
sheets <- excel_sheets(db_file)

cat("All sheets:\n")
print(sheets)
cat("\n")

# Try each sheet
for(sheet in sheets[2:length(sheets)]) {  # Skip Readme
  cat(sprintf("\n=== SHEET: %s ===\n", sheet))
  
  # Try different skip values
  for(skip in c(0, 4)) {
    tryCatch({
      data <- read_excel(db_file, sheet = sheet, skip = skip, n_max = 5)
      
      if("DB year" %in% names(data) || "DB Year" %in% names(data)) {
        cat(sprintf("  skip=%d: %d rows, years column found\n", skip, nrow(data)))
        
        # Try to get full data
        data_full <- read_excel(db_file, sheet = sheet, skip = skip)
        
        if("DB year" %in% names(data_full)) {
          years <- sort(unique(data_full$`DB year`))
        } else if("DB Year" %in% names(data_full)) {
          years <- sort(unique(data_full$`DB Year`))
        } else {
          years <- NULL
        }
        
        if(!is.null(years)) {
          cat(sprintf("    Years available: %s\n", paste(range(years, na.rm=TRUE), collapse="-")))
          cat(sprintf("    Countries: %d\n", n_distinct(data_full$Economy, na.rm=TRUE)))
        }
      }
    }, error = function(e) {})
  }
}
