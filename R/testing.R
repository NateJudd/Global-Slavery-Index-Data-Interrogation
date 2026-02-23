# ============================================================
# testing.R
# Group-based significance testing
# ============================================================

source("R/source.R")
install_and_load_packages()

# ---- Load and clean ----
data_path <- here::here("data", "raw", "2023-Global-Slavery-Index-Data.xlsx")

df <- load_data(data_path)
df <- clean_data(df)

# ---- Vulnerability split ----
vul_groups <- median_split(df$`Total Vulnerability score (%)`)
vul_test <- t.test(vul_groups$lower, vul_groups$upper)

cat("Vulnerability group comparison:\n")
print(vul_test)

# ---- Government response split ----
gr_groups <- median_split(df$`Government response total (%)`)
gr_test <- t.test(gr_groups$lower, gr_groups$upper)

cat("\nGovernment response group comparison:\n")
print(gr_test)
