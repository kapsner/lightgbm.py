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

  package <- "lightgbm"

  tryCatch(
    expr = {
      message("Testing, if 'reticulate' is already configured")

      testing_vector <- c()

      for (trying in c("miniconda", "conda", "python", "virtualenv")) {

        if (trying == "conda") {
          ret <- tryCatch(
            expr = {
              message("Trying to use conda")
              reticulate::use_condaenv(
                required = T
              )
              ret <- TRUE
              ret
            }, error = function(e) {
              print(e)
              ret <- FALSE
              ret
            }, finally = function(f) {
              return(ret)
            }
          )
        } else if (trying == "miniconda") {
          ret <- tryCatch(
            expr = {
              message("Trying to use miniconda")
              reticulate::use_miniconda(
                condaenv = reticulate::miniconda_path(),
                required = TRUE
              )
              ret <- TRUE
              ret
            }, error = function(e) {
              print(e)
              ret <- FALSE
              ret
            }, finally = function(f) {
              return(ret)
            }
          )
        } else if (trying == "virtualenv") {
          ret <- tryCatch(
            expr = {
              message("Trying to use virtualenv 'r-reticulate'")
              reticulate::use_virtualenv(
                virtualenv = "r-reticulate",
                required = T
              )
              ret <- TRUE
              ret
            }, error = function(e) {
              print(e)
              ret <- FALSE
              ret
            }, finally = function(f) {
              return(ret)
            }
          )
        } else if (trying == "python") {
          ret <- tryCatch(
            expr = {
              message("Trying to use python")
              reticulate::use_python(
                python = Sys.which("python3"),
                required = T
              )
              ret <- TRUE
              ret
            }, error = function(e) {
              print(e)
              ret <- FALSE
              ret
            }, finally = function(f) {
              return(ret)
            }
          )
        }

        testing_vector <- c(testing_vector, ret)

        if (isTRUE(ret)) {
          message(paste0("Found ", trying, ""))
          break
        }
      }

      if (any(testing_vector)) {

        message(paste0("Test, if module '", package, "' is already installed"))
        if (!reticulate::py_module_available(package)) {
          cat(paste0("Module not yet installed.... trying to install '",
                     package, "'"))

          if (trying == "conda") {
            reticulate::conda_install(
              packages = package,
              conda = "auto"
            )
          } else if (trying == "miniconda") {
            reticulate::conda_install(
              envname = reticulate::miniconda_path(),
              packages = package,
              conda = "auto"
            )
          } else if (trying == "virtualenv") {
            reticulate::virtualenv_install(
              packages = package
            )
          } else if (trying == "python") {
            reticulate::py_install(
              packages = package,
              envname = "r-reticulate",
              method = "auto",
              conda = "auto"
            )
          }
        } else {
          message(paste0("Module '", package, "' is already installed"))
        }
      } else {
        stop("No properly configured 'reticulate' installation found...")
      }
    }, error = function(e) {
      print(e)
      message("Falling back to use miniconda installation")
      tryCatch(
        expr = {
          message("Trying to install 'miniconda'....")
          reticulate::install_miniconda(
            path = reticulate::miniconda_path(),
            update = TRUE,
            force = FALSE
          )
        }, error = function(e) {
          print(e)
          message("'miniconda' is already installed")
        }
      )
      reticulate::use_miniconda(
        condaenv = reticulate::miniconda_path(),
        required = TRUE
      )
      reticulate::py_config()
      message(paste0("Test, if module '", package,
                     "' is already installed"))
      if (!reticulate::py_module_available(package)) {
        message(paste0("Module not yet installed.... trying to install '",
                       package, "'"))
        reticulate::conda_install(
          envname = reticulate::miniconda_path(),
          packages = package,
          conda = "auto"
        )
      } else {
        message("Module 'lightgbm' is already installed")
      }
    }
  )
}
