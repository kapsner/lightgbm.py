packagename <- "lightgbm.py"

# remove existing description object
unlink("DESCRIPTION")

# Create a new description object
my_desc <- desc::description$new("!new")

# Set your package name
my_desc$set("Package", packagename)

# Set author names 2
my_desc$set_authors(c(
  person("Lorenz A.", "Kapsner", email = "lorenz.kapsner@gmail.com", role = c("cre", "aut"),
         comment = c(ORCID = "0000-0003-1866-860X"))
))

# Remove some author fields
my_desc$del("Maintainer")

# Set the version
my_desc$set_version("0.0.1.9004")

# The title of your package
my_desc$set(Title = "Use Python's LightGBM Module in R")

# The description of your package
my_desc$set(Description = paste0("LightGBM in R by making use of its python module."))

# The date when description file has been created
my_desc$set("Date" = as.character(Sys.Date()))

# The urls
my_desc$set("URL", "https://github.com/kapsner/lightgbm.py")
my_desc$set("BugReports", "https://github.com/kapsner/lightgbm.py/issues")

# Vignette Builder
my_desc$set("VignetteBuilder" = "knitr")

# License
my_desc$set("License", "GPL-3")

# Reticulate
my_desc$set(
  "Config/reticulate",
  "\nlist(
    packages = list(
      list(package = \"lightgbm\"),
      list(package = \"scikit-learn\")
    )
  )")

# Save everyting
my_desc$write(file = "DESCRIPTION")

# License
#usethis::use_gpl3_license(name="Lorenz Kapsner")


# add Imports and Depends
# Listing a package in either Depends or Imports ensures that it’s installed when needed
# Imports just loads the package, Depends attaches it
# Loading will load code, data and any DLLs; register S3 and S4 methods; and run the .onLoad() function.
##      After loading, the package is available in memory, but because it’s not in the search path,
##      you won’t be able to access its components without using ::.
##      Confusingly, :: will also load a package automatically if it isn’t already loaded.
##      It’s rare to load a package explicitly, but you can do so with requireNamespace() or loadNamespace().
# Attaching puts the package in the search path. You can’t attach a package without first loading it,
##      so both library() or require() load then attach the package.
##      You can see the currently attached packages with search().

# Depends
usethis::use_package("R", min_version = "2.10", type = "Depends")

# Imports
usethis::use_package("data.table", type="Imports")
usethis::use_package("R6", type="Imports")
usethis::use_package("ggplot2", type="Imports")
usethis::use_package("paradox", type="Imports")
usethis::use_package("reticulate", type="Imports", min_version = "1.14")

# Suggests
usethis::use_package("testthat", type = "Suggests")
usethis::use_package("lintr", type = "Suggests")
usethis::use_package("grDevices", type = "Suggests")
usethis::use_package("knitr", type = "Suggests")

# for vignettes
usethis::use_package("rmarkdown", type = "Suggests")
usethis::use_package("qpdf", type = "Suggests")
usethis::use_package("knitr", type = "Suggests")
usethis::use_package("mlbench", type = "Suggests")
usethis::use_package("MLmetrics", type = "Suggests")


# buildignore
usethis::use_build_ignore("LICENSE.md")
usethis::use_build_ignore(".gitlab-ci.yml")
usethis::use_build_ignore("data-raw")

# gitignore
usethis::use_git_ignore("/*")
usethis::use_git_ignore("/*/")
usethis::use_git_ignore("*.log")
usethis::use_git_ignore("!/.gitignore")
usethis::use_git_ignore("!/.gitlab-ci.yml")
usethis::use_git_ignore("!/data-raw/")
usethis::use_git_ignore("!/DESCRIPTION")
usethis::use_git_ignore("!/inst/")
usethis::use_git_ignore("!/LICENSE.md")
usethis::use_git_ignore("!/man/")
usethis::use_git_ignore("!NAMESPACE")
usethis::use_git_ignore("!/R/")
usethis::use_git_ignore("!/vignettes/")
usethis::use_git_ignore("/vignettes/png")
usethis::use_git_ignore("!/README.md")
usethis::use_git_ignore("!/tests/")
usethis::use_git_ignore("/.Rhistory")
usethis::use_git_ignore("/*.Rproj")
usethis::use_git_ignore("/.Rproj*")
usethis::use_git_ignore("/.RData")
usethis::use_git_ignore("!/.Rbuildignore")

# code coverage
#covr::package_coverage()

# lint package
#lintr::lint_package()

# Using Config/reticulate in description
# https://cran.r-project.org/web/packages/reticulate/vignettes/python_dependencies.html
# manually copy the following into the description file:
# Config/reticulate:
# list(
#   packages = list(
#     list(package = "lightgbm"),
#     list(package = "scikit-learn")
#   )
# )