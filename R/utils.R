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

  tryCatch(
    expr = {
      cat("\nTesting, if 'reticulate' is already configured\n")
      reticulate::py_config()

      testing_vector <- c()

      for (trying in c("conda", "python", "virtualenv")) {

        if (trying == "conda") {
          ret <- tryCatch(
            expr = {
              cat("\nTrying to use conda\n")
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
        } else if (trying == "virtualenv") {
          ret <- tryCatch(
            expr = {
              cat("\nTrying to use virtualenv 'r-reticulate'\n")
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
              cat("\nTrying to use python\n")
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
          cat(paste0("\nFound ", trying, "\n"))
          break
        }
      }

      if (any(testing_vector)) {

        reticulate::py_config()

        cat("\nTest, if module 'lightgbm' is already installed\n")
        if (!reticulate::py_module_available("lightgbm")) {
          cat("\nModule not yet installed.... trying to install 'lightgbm'\n")

          if (trying == "conda") {
            reticulate::conda_install(
              packages = "lightgbm",
              conda = "auto"
            )
          } else if (trying == "virtualenv") {
            reticulate::virtualenv_install(
              packages = "lightgbm"
            )
          } else if (trying == "python") {
            reticulate::py_install(
              packages = "lightgbm",
              envname = "r-reticulate",
              method = "auto",
              conda = "auto"
            )
          }
        } else {
          cat("\nModule 'lightgbm' is already installed\n")
        }
      } else {
        stop("No properly configured 'reticulate' installation found...")
      }
    }, error = function(e) {
      print(e)
      cat("\nFalling back to use miniconda installation\n")
      tryCatch(
        expr = {
          cat("\nTrying to install 'miniconda'....\n")
          reticulate::install_miniconda(
            path = reticulate::miniconda_path(),
            update = TRUE,
            force = FALSE
          )
        }, error = function(e) {
          print(e)
          cat("\n'miniconda' is already installed\n")
        }
      )
      reticulate::use_miniconda(
        condaenv = reticulate::miniconda_path(),
        required = TRUE
      )
      reticulate::py_config()
      cat("\nTest, if module 'lightgbm' is already installed\n")
      if (!reticulate::py_module_available("lightgbm")) {
        cat("\nModule not yet installed.... trying to install 'lightgbm'\n")
        reticulate::conda_install(packages = "lightgbm")
      } else {
        cat("\nModule 'lightgbm' is already installed\n")
      }
    }
  )
}
