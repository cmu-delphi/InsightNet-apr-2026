# Workshop setup script
# Set theme and colors
colours <- c(
  "#172431",
  "#ffffff",
  "#C41230",
  "#941120",
  "#007BC0",
  "#002676",
  "#FDB515",
  "#002145",
  "#00A7E1",
  "#8C338F",
  "#368f33"
)

# Palette selection
dis_pal_list <- colours[c(3, 5, 7, 10, 11, 9)]

if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_family = "Lato", base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray50"),
        plot.caption = ggplot2::element_text(hjust = 0, color = "gray50"),
        panel.grid = ggplot2::element_line(linetype = 2, linewidth = 0.3, color = "gray90"),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "top",
        legend.key.width = ggplot2::unit(3, "lines"),
        strip.background = ggplot2::element_rect(fill = "gray95", linetype = "blank"),
        panel.border = ggplot2::element_rect(color = "gray95", fill = NA)
      )
  )

  # Palette as global defaults for discrete and continuous scales
  options(
    ggplot2.discrete.colour   = dis_pal_list,
    ggplot2.discrete.fill     = dis_pal_list,
    ggplot2.continuous.colour = function() ggplot2::scale_colour_gradientn(colours = dis_pal_list),
    ggplot2.continuous.fill   = function() ggplot2::scale_fill_gradientn(colours = dis_pal_list)
  )

  # Update defaults for geoms/stats
  tibble::tribble(
    ~name,      ~aes,     ~alpha, ~is_stat,
    "line",     "colour", 1.0,    FALSE,
    "point",    "colour", 0.7,    FALSE,
    "function", "colour", 1.0,    FALSE,
    "bar",      "fill",   0.9,    FALSE,
    "col",      "fill",   0.9,    FALSE,
    "boxplot",  "fill",   0.8,    FALSE,
    "bin",      "fill",   0.8,    TRUE,
    "identity", "fill",   0.7,    TRUE
  ) %>%
    purrr::pwalk(function(name, aes, alpha, is_stat) {
      updater <- if (is_stat) ggplot2::update_stat_defaults else ggplot2::update_geom_defaults
      updater(name, setNames(list(dis_pal_list[1], alpha), c(aes, "alpha")))
    })
}

# Helper: build epirange with correct format for the signal's time_type
time_range <- function(start, end, time_type = "day") {
  if (time_type == "week") {
    to_yw <- function(d) {
      d <- as.Date(d)
      m <- MMWRweek::MMWRweek(d)
      m$MMWRyear * 100L + m$MMWRweek
    }
    epiprocess::epirange(to_yw(start), to_yw(end))
  } else {
    epiprocess::epirange(start, end)
  }
}
