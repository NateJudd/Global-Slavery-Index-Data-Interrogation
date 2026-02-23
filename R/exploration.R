# ============================================================
# exploration.R
# Data exploration and mapping
# ============================================================

source("R/source.R")
load_packages()

# ---- Load and clean ----
data_path <- here::here("data", "raw", "2023-Global-Slavery-Index-Data.xlsx")

df <- load_data(data_path)
df <- clean_data(df)

world_df <- create_world_dataset(df)

# ---- Plotting helper ----
plot_map <- function(world_df, variable, output_name) {
  p <- ggplot(world_df) +
    geom_sf(aes(fill = .data[[variable]])) +
    theme_bw() +
    labs(
      title = paste("Global Map of", variable),
      fill = variable
    )
  
  ggsave(
    filename = file.path("outputs", "figures", output_name),
    plot = p,
    width = 10,
    height = 6
  )
}

# ---- Generate maps ----
plot_map(world_df,
         "Estimated prevalence of modern slavery per 1,000 population",
         "prevalence_map.png")

plot_map(world_df,
         "Total Vulnerability score (%)",
         "vulnerability_map.png")

plot_map(world_df,
         "Government response total (%)",
         "government_response_map.png")
