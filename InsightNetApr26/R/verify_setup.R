required_pkgs <- tibble::tribble(
  ~package, ~op, ~req_version,
  "dplyr", NA, NA,
  "epidatasets", ">=", "0.0.1",
  "epidatr", ">=", "1.2.2",
  "epipredict", ">=", "0.1.5",
  "epiprocess", ">=", "0.12.0",
  "ggplot2", NA, NA,
  "parsnip", NA, NA
)


#' Verify system setup
#'
#' Call this function to ensure that all necessary packages are installed.
#'
#' @return Returns `TRUE` if assigned. Otherwise produces status messages.
#' @export
verify_setup <- function() {
  # We use pak (which is in Imports) to get statuses
  statuses <- pak::pkg_status(required_pkgs$package)

  # Check for missing packages
  not_installed <- required_pkgs$package[!(required_pkgs$package %in% statuses$package)]
  if (length(not_installed) > 0) {
    cli::cli_abort(
      "The following required packages are not installed: {.pkg {not_installed}}."
    )
  }

  # Check versions
  # Join statuses back to required_pkgs to get op and req_version
  # (Since we don't have dplyr, we do it in base R)
  idx <- match(required_pkgs$package, statuses$package)
  current_versions <- statuses$version[idx]

  needs_check <- !is.na(required_pkgs$req_version)
  if (any(needs_check)) {
    for (i in which(needs_check)) {
      pkg <- required_pkgs$package[i]
      op <- required_pkgs$op[i]
      req_v <- required_pkgs$req_version[i]
      curr_v <- current_versions[i]

      if (!do.call(op, list(curr_v, req_v))) {
        cli::cli_abort(c(
          "Package {.pkg {pkg}} does not have the correct version:",
          "i" = "Installed: {.val {curr_v}}.",
          "i" = "Required: {.val {paste(op, req_v)}}."
        ))
      }
    }
  }

  cli::cli_alert_success("You should be good to go!")
  invisible(TRUE)
}
