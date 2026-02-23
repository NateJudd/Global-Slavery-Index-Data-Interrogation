# ============================================================
# source.R
# Shared utilities and core functions
# ============================================================

# ---- Load required packages ----
required_packages <- c(
  "readxl",
  "dplyr",
  "ggplot2",
  "sf",
  "countrycode",
  "rnaturalearth",
  "rnaturalearthdata",
  "stringr",
  "here"
)

install_and_load_packages <- function(pkgs = required_packages) {
  installed <- rownames(installed.packages())
  
  for (pkg in pkgs) {
    if (!pkg %in% installed) {
      message("Installing missing package: ", pkg)
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

load_packages <- function(pkgs = required_packages) {
  invisible(lapply(pkgs, require, character.only = TRUE))
}

# ---- Data loading ----
load_data <- function(path) {
  if (!file.exists(path)) {
    stop("Data file not found: ", path)
  }
  
  read_excel(path, sheet = 2, skip = 2)
}

# ---- Basic cleaning ----
clean_data <- function(df) {
  df %>%
    mutate(
      iso3c = countrycode(Country, "country.name", "iso3c"),
      Region = as.factor(Region)
    )
}

# ---- World map with merged data ----
create_world_dataset <- function(df) {
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  world %>%
    left_join(df, by = c("iso_a3" = "iso3c"))
}

# ---- Generic GLM wrapper ----
fit_glm_model <- function(data, formula, family = poisson()) {
  model <- glm(formula = formula, data = data, family = family)
  
  list(
    model = model,
    summary = summary(model),
    aic = AIC(model)
  )
}

# ---- Overdispersion check ----
check_overdispersion <- function(model) {
  rp <- residuals(model, type = "pearson")
  n <- length(rp)
  rdf <- df.residual(model)
  sum(rp^2) / rdf
}

# ---- Split variable into median groups ----
median_split <- function(x) {
  n <- length(x)
  x_sorted <- sort(x)
  
  list(
    lower = x_sorted[1:(n/2)],
    upper = x_sorted[(n/2 + 1):n]
  )
}
