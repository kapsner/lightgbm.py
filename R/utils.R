#% .onLoad = function(libname, pkgname) {
#%
#%   # use superassignment to update global reference to lightgbm
#%   reticulate::py_config()
#%   if (isFALSE(reticulate::py_module_available("lightgbm"))) {
#%     reticulate::py_install("lightgbm", method = "auto", conda = "auto")
#%   }
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
  }
}