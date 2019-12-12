context("lints")

if (dir.exists("../../00_pkg_src")) {
  prefix <- "../../00_pkg_src/lightgbm.py/"
} else if (dir.exists("../../R")) {
  prefix <- "../../"
} else if (dir.exists("./R")) {
  prefix <- "./"
}


test_that(
  desc = "test lints",
  code = {

    # skip on covr
    skip_on_covr()

    lintlist <- list(
      "R" = list(
        "lgbparams.R" = NULL,
        "lightgbm_train.R" = list(
          list(message = "cyclomatic complexity", line_number = 13),
          list(message = "snake_case", line_number = 13)
        ),
        "sklearn_train_test_split.R" = NULL,
        "utils.R" = NULL #%list(
          #% list(message = "snake_case", line_number = 1),
          #% list(message = "lightgbm", line_number = 3)
        #% )
      ),
      "tests/testthat" = list(
        "test-lints.R" = NULL
      )
    )
    for (directory in names(lintlist)) {
      print(directory)
      for (fname in names(lintlist[[directory]])) {
        print(fname)
        #% print(list.files(prefix))

        lintr::expect_lint(
          file = paste0(
            prefix,
            directory,
            "/",
            fname
          ),
          checks = lintlist[[directory]][[fname]]
        )
      }
    }
  }
)