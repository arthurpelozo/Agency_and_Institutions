library(dplyr)

# Load aggregated data
agg <- readRDS("output/did_results/aps_agg_for_did.rds")

cat("Aggregated data structure:\n")
cat("Rows:", nrow(agg), "\n")
cat("Columns:", ncol(agg), "\n\n")

cat("Column names:\n")
print(names(agg))

cat("\n\nFirst few rows:\n")
print(head(agg, 20))

cat("\n\nYear range:\n")
print(range(agg$year))

cat("\n\nCountries:\n")
if("country" %in% names(agg)) {
  cat("Via 'country':", n_distinct(agg$country), "\n")
  print(sort(unique(agg$country)))
}
