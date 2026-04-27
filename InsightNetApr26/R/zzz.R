.onAttach <- function(libname, pkgname) {
  # Run verification silently, but alert on issues
  tryCatch(
    {
      verify_setup()
    },
    error = function(e) {
      packageStartupMessage(paste("Note:", e$message))
    }
  )
}
