# ============================================================
# modelling.R
# GLM modelling for prevalence
# ============================================================

source("R/source.R")
install_and_load_packages()

# ---- Load and clean ----
data_path <- here::here("data", "raw", "2023-Global-Slavery-Index-Data.xlsx")

df <- load_data(data_path)
df <- clean_data(df)

response_var <- "`Estimated prevalence of modern slavery per 1,000 population`"

# ---- Model 1: All predictors ----
formula_1 <- as.formula(paste(response_var, "~ ."))

model_1 <- fit_glm_model(df, formula_1)
print(model_1$summary)

# ---- Model 2: Core predictors ----
formula_2 <- as.formula(paste(
  response_var,
  "~ Population + Region + `Total Vulnerability score (%)` + `Government response total (%)`"
))

model_2 <- fit_glm_model(df, formula_2)
print(model_2$summary)

# ---- Model 3: Vulnerability subcomponents ----
formula_3 <- as.formula(paste(
  response_var,
  "~ `Governance issues` + `Lack of basic needs` + Inequality + `Disenfranchised groups` + `Effects of conflict`"
))

model_3 <- fit_glm_model(df, formula_3)
print(model_3$summary)

# ---- Overdispersion diagnostics ----
dispersion_2 <- check_overdispersion(model_2$model)
cat("Overdispersion statistic (Model 2):", dispersion_2, "\n")

# ---- Save AIC comparison ----
aic_table <- data.frame(
  Model = c("All predictors", "Core predictors", "Vulnerability components"),
  AIC = c(model_1$aic, model_2$aic, model_3$aic)
)

write.csv(aic_table, file = file.path("outputs", "models", "model_aic_comparison.csv"), row.names = FALSE)
