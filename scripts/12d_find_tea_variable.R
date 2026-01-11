library(haven)

# Load data
aps <- read_sav("data_raw/GEM 2019 APS Global Individual Level Data_30Jan2021.sav")

# Look for TEA variables
tea_vars <- names(aps)[grepl("tea|entrepreneur", names(aps), ignore.case=TRUE)]
cat("TEA-related variables:\n")
print(tea_vars)

# Look for entrepreneurship variables
cat("\n\nAll variables (first 50):\n")
print(names(aps)[1:50])
