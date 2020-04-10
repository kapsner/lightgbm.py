.onLoad <- function(libname, pkgname) { # nolint
  reticulate::configure_environment(pkgname)
}

#' @title Install lightgbm python module via reticulate.
#'
#' @export
#'
install_py_lightgbm <- function() {

  package <- "lightgbm"

  stopifnot(
    reticulate::py_numpy_available()
  )

  tryCatch(
    expr = {
      reticulate::py_config()
    }, error = function(e) {
      stop(paste0("\nAn error occured while testing the ",
                  "reticulate configguration.\nPlease make sure ",
                  "the "))
    }
  )

  if (!reticulate::py_module_available(package)) {
    message(paste0("\nModule not yet installed.... ",
                   "trying to install '", package, "'\n"))
    reticulate::py_install(
      packages = package,
      method = "auto",
      conda = "auto"
    )
  } else {
    message("\nModule 'lightgbm' is already installed\n")
  }
}
