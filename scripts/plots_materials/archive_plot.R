# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(epiprocess)
library(rlang)
library(httr2)
library(readr)
library(glue)
library(magrittr)

# Helper Functions ---
EPIDATA_V2_URL <- "https://delphi.cmu.edu/cast-api/epidata/v2"

`%nin%` <- function(x, y) !(x %in% y)

build_cast_api_query <- function(
    source = c("nssp", "nhsn"),
    signal = NULL,
    geo_type = c("state", "nation"),
    columns = NULL,
    fill_method = c("source", "fill_ave", "fill_zero"),
    limit = -1,
    offset = 0,
    version_query = NULL,
    geo_value = NULL,
    time_value = NULL
) {
  source <- rlang::arg_match(source)
  fill_method <- rlang::arg_match(fill_method)
  geo_type <- rlang::arg_match(geo_type)
  columns <- columns %||% c("geo_value", "time_value", "value", "report_ts_nominal_start")
  
  httr2::request(EPIDATA_V2_URL) %>%
    httr2::req_url_path_append("archive/") %>%
    httr2::req_url_query(
      source = source,
      signal = signal,
      geo_type = geo_type,
      version_query = version_query,
      columns = columns,
      limit = limit,
      offset = offset,
      fill_method = fill_method,
      geo_value = geo_value,
      time_value = time_value,
      .multi = "explode"
    )
}

get_cast_api_data <- function(...) {
  req <- build_cast_api_query(...)
  filename <- tempfile(fileext = ".csv")
  
  # Check for proxy environment variable if any (Delphi specific)
  # proxy_port <- Sys.getenv("CAST_API_PROXY_PORT", "")
  # if (proxy_port != "") {
  #   req <- req %>% httr2::req_proxy(url = "socks5h://localhost", port = as.integer(proxy_port))
  # }
  
  req <- req %>% httr2::req_perform(path = filename)
  readr::read_csv(filename, show_col_types = FALSE)
}

get_nhsn_data_archive <- function(disease = c("covid", "flu")) {
  disease <- rlang::arg_match(disease)
  message("Fetching state data...")
  nhsn_state <- get_cast_api_data(
    source = "nhsn",
    signal = glue::glue("confirmed_admissions_{disease}_ew"),
    geo_type = "state",
    columns = c("geo_value", "time_value", "value", "report_ts_nominal_start", "report_ts_nominal_end"),
    version_query = glue::glue("<={Sys.Date()}")
  )
  message("Fetching national data...")
  nhsn_nation <- get_cast_api_data(
    source = "nhsn",
    signal = glue::glue("confirmed_admissions_{disease}_ew"),
    geo_type = "nation",
    columns = c("geo_value", "time_value", "value", "report_ts_nominal_start", "report_ts_nominal_end"),
    version_query = glue::glue("<={Sys.Date()}")
  )
  
  message("Combining and converting to epi_archive...")
  nhsn_data <- nhsn_state %>%
    rbind(nhsn_nation) %>%
    select(geo_value, time_value, version = report_ts_nominal_start, value) %>%
    mutate(
      geo_value = tolower(geo_value),
      version = as.Date(version)
    ) %>%
    arrange(geo_value, time_value, version) %>%
    distinct(geo_value, time_value, version, .keep_all = TRUE) %>%
    epiprocess::as_epi_archive(compactify = TRUE)
  
  nhsn_data
}

# Archive Fetching and Logic ---
nhsn_archive_data <- get_nhsn_data_archive("flu")

nssp_data <- epidatr::pub_covidcast(
  source = "nssp",
  signals = "pct_ed_visits_covid",
  geo_type = "state",
  time_type = "week",
  geo_values = "*",
  issues = "*" # This gets all versions available as of today
)

# issues = # pub_covidcast returns a data frame with 'issue' which we use as 'version'
nhsn_archive_data <- nssp_data %>%
  mutate(geo_value_comp = covidcast::abbr_to_name(stringr::str_to_upper(geo_value))) |> 
  rename(version = issue) %>%
  # mutate(geo_value = tolower(geo_value)) %>%
  arrange(geo_value, time_value, version) %>%
  epiprocess::as_epi_archive(compactify = TRUE)

this_week <- round_date(Sys.Date() - 3, "week", 6)

recent_archive <- nhsn_archive_data %>%
  filter(this_week - time_value < 10*7)

recent_archive$time_type <- "day"

# Revision analysis identifies which geographies had the most significant updates
revision_sum <- recent_archive %>%
  epiprocess::revision_analysis(value, min_waiting_period = NULL)

av_re_spread <- revision_sum$revision_behavior %>%
  group_by(geo_value) %>%
  summarize(rel_spread = mean(rel_spread, na.rm = TRUE)) %>%
  arrange(desc(rel_spread)) %>%
  filter(geo_value %nin% c("vi", "as", "gu"))

worst_geos <- av_re_spread %>% pull(geo_value)
worst_geos <- worst_geos[1:9]

# Prepare data for plotting
filtered_archive <- recent_archive %>%
  filter(geo_value %in% worst_geos) %>%
  filter(time_value >= "2024-11-19")

# Update levels to ensure consistent plot order
filtered_archive$DT %<>%
  mutate(geo_value = factor(geo_value, levels = av_re_spread$geo_value[1:18]))

# Plotting ---
autoplot(filtered_archive, "value") +
  facet_wrap(~geo_value, ncol = 3, scales = "free") +
  theme(strip.text.x = element_text(size = 8)) +
  ylim(0, NA) +
  labs(title = "States with the largest mean revision",
       subtitle = paste("Generated on", Sys.Date()))

recent_archive %>%
  filter(geo_value %in% c("nc", "ga", "ny", "me")) |> 
  autoplot("value") +
  facet_wrap(~covidcast::abbr_to_name(stringr::str_to_upper(geo_value)), ncol = 2, scales = "free") +
  theme(strip.text.x = element_text(size = 8),
        legend.position = "right") +
  guides(colour = guide_colorbar(barwidth = 0.5, barheight = 10)) +
  scale_y_continuous(n.breaks = 3) +
  # ylim(0, NA) +
  scale_x_date(breaks = breaks_pretty(), label = label_date_short()) +
  labs(title = "Mean Revision Spread of NSSP COVID-19 ED Visits")


# Save plot
ggsave(paste0("plots/archive.png"),
       bg = "transparent",
       width = 160,                 # Ancho de la gráfica
       height = 80,
       units = "mm",
       dpi = 300)

last_plot() +
  labs(title = "Mean Revision Spread of \nNSSP COVID-19 ED Visits")

ggsave(paste0("plots/archive_sq.png"),
       bg = "transparent",
       width = 120*1.2,                 # Ancho de la gráfica
       height = 90*1.2,
       units = "mm",
       dpi = 300)
