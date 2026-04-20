# Configuration of packages and their corresponding remotes
required_pkgs <- tibble::tribble(
  ~package,      ~op,  ~req_version, ~remote,
  "dplyr",       NA,   NA,           "dplyr",
  "epidatasets", ">=", "0.0.1",      "cmu-delphi/epidatasets",
  "epidatr",     ">=", "1.2.2",      "cmu-delphi/epidatr",
  "epipredict",  ">=", "0.1.5",      "cmu-delphi/epipredict@dev",
  "epiprocess",  ">=", "0.12.0",     "cmu-delphi/epiprocess@dev",
  "ggplot2",     NA,   NA,           "ggplot2",
  "parsnip",     NA,   NA,           "parsnip"
)

#' Verify and install system setup
#'
#' Call this function to ensure that all necessary packages are installed.
#' If packages are missing or outdated, it will attempt to install them using {pak}.
#'
#' @param install Logical. If TRUE (default), automatically installs missing/outdated packages.
#' @return Returns `TRUE` if everything is satisfactory. Otherwise attempts install or errors.
#' @export
verify_setup <- function(install = TRUE) {
  cli::cli_h1("Verifying Workshop Setup")
  
  # Get current status using pak
  statuses <- pak::pkg_status(required_pkgs$package)
  
  # Identify missing or outdated packages
  to_install <- c()
  
  for (i in seq_len(nrow(required_pkgs))) {
    pkg <- required_pkgs$package[i]
    op <- required_pkgs$op[i]
    req_v <- required_pkgs$req_version[i]
    remote <- required_pkgs$remote[i]
    
    # Check if installed
    pkg_status <- statuses[statuses$package == pkg, ]
    
    is_installed <- nrow(pkg_status) > 0 && !is.na(pkg_status$version)
    needs_update <- FALSE
    
    if (is_installed && !is.na(req_v)) {
      curr_v <- pkg_status$version
      if (!do.call(op, list(curr_v, req_v))) {
        needs_update <- TRUE
      }
    }
    
    if (!is_installed || needs_update) {
      reason <- if (!is_installed) "missing" else paste("outdated (need", op, req_v, ")")
      cli::cli_alert_warning("Package {.pkg {pkg}} is {reason}.")
      to_install <- c(to_install, remote)
    }
  }
  
  # Perform install if needed
  if (length(to_install) > 0) {
    if (install) {
      cli::cli_alert_info("Attempting to install/update {length(to_install)} package{?s}...")
      # Use pak to install the specific remotes
      pak::pkg_install(to_install, dependencies = TRUE)
      cli::cli_alert_success("install complete. Please re-run verify_setup() to confirm.")
    } else {
      cli::cli_abort(c(
        "Setup incomplete.", 
        "i" = "Run {.fn InsightNetApr26::verify_setup} to fix automatically."
      ))
    }
  } else {
    cli::cli_alert_success("All required packages are installed and up to date!")
  }

  invisible(TRUE)
}
