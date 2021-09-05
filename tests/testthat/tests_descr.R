library(Datavar)
library(testthat)

# Vérifier le fonctionnement de base
test_that("Basic functioning of the package.", {
  datavar_mtcars <- create_datavar(mtcars,
                                   colonnes = c("mpg", "cyl", "am", "vs"),
                                   default_datavar = options_datavar(coefbin = TRUE))
  expect_output(str(descr(mtcars, datavar = datavar_mtcars)), "m_df")
  expect_output(str(descr(mtcars, y = vs, datavar = datavar_mtcars)), "m_df")
})
