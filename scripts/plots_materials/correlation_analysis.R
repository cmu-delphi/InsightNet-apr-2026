# Auto-contained Correlation Analysis Script
# Replicates plots from signal_evaluation.qmd:

# Load Libraries ---
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  epidatr, epiprocess, dplyr, ggplot2, purrr, tidyr,
  tibble, scales, patchwork, glue, MMWRweek
)

# Shared Setup ---
# Source the shared setup if it exists
source("scripts/setup.R")

# 3. Default Parameters ---
params <- list(
  guiding_source = "hhs",
  guiding_signal = "confirmed_admissions_covid_1d",
  guiding_name = "COVID Hospital Admissions (HHS)",
  candidate_source = "doctor-visits",
  candidate_signal = "smoothed_adj_cli",
  candidate_name = "Doctor Visits: Smoothed Adj CLI",
  geo_type = "state",
  time_type = "day",
  start_day = "2021-01-01",
  end_day = "2022-06-01",
  max_locations_plot = 6
)

# Data Loading and Preprocessing ---

df_guiding <- pub_covidcast(
  params$guiding_source, params$guiding_signal,
  params$geo_type, params$time_type,
  time_values = epirange(as.numeric(gsub("-", "", params$start_day)), as.numeric(gsub("-", "", params$end_day)))
) |> as_epi_df()

df_candidate <- pub_covidcast(
  params$candidate_source, params$candidate_signal,
  params$geo_type, params$time_type,
  time_values = epirange(as.numeric(gsub("-", "", params$start_day)), as.numeric(gsub("-", "", params$end_day)))
) |> as_epi_df()

# Combine data
df_inter <- df_guiding |>
  select(geo_value, time_value, guiding = value) |>
  inner_join(
    df_candidate |> select(geo_value, time_value, candidate = value),
    by = c("geo_value", "time_value")
  ) |>
  as_epi_df()

n_inter_locations <- n_distinct(df_inter$geo_value)
time_unit <- "days"
lag_unit_label <- "Lag (days)"
lag_values <- c(-28, -14, 0, 14, 28)
lags_sweep <- -21:21

# Plot Generation ---

# --- Plot 2.1.3: Trend Comparison (Normalized) ---
set.seed(123)
all_geos <- sort(unique(df_inter$geo_value))
n_plot <- min(length(all_geos), params$max_locations_plot)
sample_geos <- all_geos[round(seq(1, length(all_geos), length.out = 4))]

df_long_norm <- df_inter |>
  filter(geo_value %in% sample_geos) |>
  pivot_longer(
    cols = c(candidate, guiding),
    names_to = "indicator", values_to = "value"
  ) |>
  group_by(geo_value, indicator) |>
  mutate(value_norm = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(indicator = recode(indicator,
    candidate = params$candidate_name,
    guiding = params$guiding_name
  ))

(p_trend_norm <- df_long_norm |>
  filter(time_value >= as.Date("2021-10-01")) |>
  ggplot(aes(x = time_value, y = value_norm, color = stringr::str_wrap(indicator, 20))) +
  geom_line(linewidth = 0.5, alpha = 0.6) +
  geom_point(size = 0.3, alpha = 0.8) +
  facet_wrap(~ covidcast::abbr_to_name(stringr::str_to_upper(geo_value)), ncol = 2) +
  scale_y_continuous(n.breaks = 3) +
  scale_x_date(breaks = breaks_pretty(), label = label_date_short()) +
  labs(
    title = "Trend Comparison (Normalized)",
    subtitle = "Standardized indicators per location",
    x = "Date", y = "Standardized Value", color = "Indicator"
  ) +
  theme(legend.position = "bottom"))

ggsave(paste0("plots/trend.png"),
  bg = "transparent",
  width = 160, # Ancho de la gráfica
  height = 80,
  units = "mm",
  dpi = 300
)

ggsave(paste0("plots/trend_sq.png"),
  bg = "transparent",
  width = 120, # Ancho de la gráfica
  height = 90,
  units = "mm",
  dpi = 300
)

# Correlation over Time ---
cor_by_time <- epi_cor(df_inter, candidate, guiding,
  cor_by = time_value, method = "spearman"
)

mean_rho <- mean(cor_by_time$cor, na.rm = TRUE)

(p_cor_time <- ggplot(cor_by_time, aes(x = time_value, y = cor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(aes(color = "Correlation"), linewidth = 0.5) +
  geom_smooth(aes(color = "LOESS smoothed trend"),
    method = "loess", span = 0.2, se = TRUE,
    fill = dis_pal_list[2], alpha = 0.15
  ) +
  scale_x_date(label = label_date_short()) +
  scale_y_continuous(limits = c(0, 0.75)) +
  scale_color_manual(name = "Trend", values = c("Correlation" = dis_pal_list[1], "LOESS smoothed trend" = dis_pal_list[2])) +
  labs(
    title = "Correlation over time",
    subtitle = glue("Spearman rho across locations | Mean = {sprintf('%.3f', mean_rho)}"),
    x = "Date", y = "Spearman rho"
  ) +
  theme(legend.position = "bottom"))

ggsave(paste0("plots/corr_time.png"),
  bg = "transparent",
  width = 160, # Ancho de la gráfica
  height = 80,
  units = "mm",
  dpi = 300
)

ggsave(paste0("plots/corr_time_sq.png"),
  bg = "transparent",
  width = 120, # Ancho de la gráfica
  height = 90,
  units = "mm",
  dpi = 300
)

# --- Plot 3.3.2: Per-location temporal correlation distributions at different lags ---
cor_lagged_geo <- map(lag_values, ~ {
  epi_cor(df_inter, candidate, guiding,
    cor_by = geo_value, dt1 = -.x, method = "spearman"
  ) |>
    mutate(lag = .x)
}) |>
  list_rbind() |>
  mutate(lag = as.factor(lag))

(p_cor_dist_lagged <- ggplot(cor_lagged_geo, aes(x = cor)) +
  geom_density(aes(fill = lag, color = lag), alpha = 0.15) +
  xlim(-1, 1) +
  labs(
    title = "Temporal correlations at different lags",
    # subtitle = "Positive lag = candidate leads the guiding indicator",
    x = "Spearman rho", y = "Density",
    fill = lag_unit_label, color = lag_unit_label
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.35))) +
  theme(legend.position = "bottom"))

ggsave(paste0("plots/corr_dist.png"),
  bg = "transparent",
  width = 160, # Ancho de la gráfica
  height = 80,
  units = "mm",
  dpi = 300
)

ggsave(paste0("plots/corr_dist_sq.png"),
  bg = "transparent",
  width = 120, # Ancho de la gráfica
  height = 90,
  units = "mm",
  dpi = 300
)

# --- Plot 3.4: Systematic Lag Analysis (Sweep) ---
cor_sweep <- map(lags_sweep, ~ {
  epi_cor(df_inter, candidate, guiding,
    cor_by = geo_value, dt1 = -.x, method = "spearman"
  ) |>
    mutate(lag = .x)
}) |> list_rbind()

lag_summary <- cor_sweep |>
  group_by(lag) |>
  summarize(
    mean_cor = mean(cor, na.rm = TRUE),
    median_cor = median(cor, na.rm = TRUE),
    q25 = quantile(cor, 0.25, na.rm = TRUE),
    q75 = quantile(cor, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

optimal_lag <- lag_summary$lag[which.max(lag_summary$mean_cor)]

(p_lag_sweep <- ggplot(lag_summary, aes(x = lag)) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = "IQR"), alpha = 0.2) +
  geom_line(aes(y = mean_cor, color = "Mean"), linewidth = 1.2) +
  geom_point(aes(y = mean_cor, color = "Mean"), size = 1) +
  geom_line(aes(y = median_cor, color = "Median"), linetype = "dashed") +
  geom_vline(xintercept = optimal_lag, linetype = "dotted", color = "black") +
  annotate("text",
    x = optimal_lag, y = -1, label = sprintf("Optimal: %d", optimal_lag),
    hjust = -0.1, vjust = -0.5
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Summary metrics vs. Lag",
    subtitle = "Searching for the optimal lead time",
    x = lag_unit_label, y = "Spearman rho",
    color = "", fill = ""
  ) +
  theme(legend.position = "bottom"))

ggsave(paste0("plots/cor_sweep.png"),
  bg = "transparent",
  width = 160, # Ancho de la gráfica
  height = 80,
  units = "mm",
  dpi = 300
)

# 6. Display/Save Results ---

# Save to file
output_dir <- here::here("scripts", "plots_materials")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
ggsave(file.path(output_dir, "correlation_analysis_replicated.png"),
  combined_plot,
  width = 14, height = 12, dpi = 300
)

message("Analysis complete. Plot saved to scripts/plots_materials/correlation_analysis_replicated.png")
