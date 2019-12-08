#% .onLoad <- function(libname, pkgname) {
#%   reticulate::py_config()
#%   if (isFALSE(reticulate::py_module_available("lightgbm"))) {
#%     reticulate::py_install("lightgbm", method = "auto", conda = "auto")
#%   }
#%   lightgbm <- reticulate::import("lightgbm", delay_load = TRUE)
#% }

#' @title Install lightgbm python module via reticulate.
#'
#' @export
#'
install_py_lightgbm <- function() {
  reticulate::py_config()

  if (reticulate::py_available()) {
    reticulate::py_config()
    if (isFALSE(reticulate::py_module_available("lightgbm"))) {
      reticulate::py_install("lightgbm", method = "auto", conda = "auto")
    }
  } else {
    stop(paste0("Python is not available. Please install python ",
                "on your system to be able to use the 'lightgbm.py' ",
                "R package!"))
  }
}