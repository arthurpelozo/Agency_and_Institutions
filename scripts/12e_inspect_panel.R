library(dplyr)

# Load panel data
panel <- readRDS("output/did_results/aps_did_panel.rds")

cat("Panel data structure:\n")
cat("Rows:", nrow(panel), "\n")
cat("Columns:", ncol(panel), "\n\n")

cat("Column names:\n")
print(names(panel))

cat("\n\nFirst few rows:\n")
print(head(panel, 10))

cat("\n\nCountry variable summary:\n")
if("econame" %in% names(panel)) {
  cat("Countries via econame:", n_distinct(panel$econame), "\n")
  print(unique(panel$econame)[1:10])
}
if("country" %in% names(panel)) {
  cat("Countries via country:", n_distinct(panel$country), "\n")
  print(unique(panel$country)[1:10])
}
if("country_id" %in% names(panel)) {
  cat("Countries via country_id:", n_distinct(panel$country_id), "\n")
}
