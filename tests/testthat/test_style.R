if (requireNamespace("lintr", quietly = TRUE)) {
  library(lintr)

  context("lints")
  test_that("Package Style", {
    skip_on_cran()
    skip_on_travis()  

    # A duplicate copy of the find_package function from lintr
    find_package <- function(path) {
      path <- normalizePath(path, mustWork = FALSE)

      while (!file.exists(file.path(path, "DESCRIPTION"))) {
        path <- dirname(path)
        if (identical(path, dirname(path))) {
          return(NULL)
        }
      }

      path
    }

    if (!is.null(find_package("."))) {
      expect_lint_free()
    }
  })
}
