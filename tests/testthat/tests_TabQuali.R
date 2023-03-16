library(Datavar)
library(testthat)
library(magrittr)
library(waldo)

# Retourne les bonnes valeurs en descriptions croisée et univariée
test_that("Tab_quali gives the correct output for uni- and bivariate description of cyl in mtcars.", {
  expect_equal(
    TabQuali(.Data = mtcars, x = cyl),
    structure(list(Variable = c("cyl (n, %)", "cyl (n, %)", "cyl (n, %)"
    ), Label = c("  8", "  4", "  6"), Statistics = c("14/32 (44%)",
                                                      "11/32 (34%)", "7/32 (22%)")), row.names = c(NA, 3L), class = c("tab_datavar",
                                                                                                                      "data.frame")))
  expect_equal(
    TabQuali(.Data = mtcars, x = cyl, y = am),
    structure(list(Variable = c("cyl (n, %)", "cyl (n, %)", "cyl (n, %)"
    ), Label = c("  8", "  4", "  6"), `Statistics (am=1)` = c("12/19 (63%)",
                                                               "3/19 (16%)", "4/19 (21%)"), `Statistics (am=0)` = c("2/13 (15%)",
                                                                                                                    "8/13 (62%)", "3/13 (23%)")), row.names = c(NA, 3L), class = c("tab_datavar",
                                                                                                                                                                                   "data.frame")))
})

# Les Pvalues sont-elles bonnes ?
test_that("Pvalues displayed are correct.", {
  expect_equal(TabQuali(mtcars, cyl, am, Test = "chisq", Prec = 2)[1, 5],
               "0.01")
  expect_equal(TabQuali(mtcars, cyl, am, Test = "fisher", Prec = 2)[1, 5],
               "<0.01")
})

# Check the confidence intervals
test_that("Confidence intervals displayed are correct.", {
  expect_equal(TabQuali(mtcars, cyl, am, ConfInter = "normal")[1, 3],
               "12/19 (63%[41;85])")
  expect_equal(TabQuali(mtcars, cyl, am, ConfInter = "exact")[1, 3],
               "12/19 (63%[38;84])")
  expect_equal(TabQuali(mtcars, cyl, am, ConfInter = "jeffreys")[1, 3],
               "12/19 (63%[41;82])")
})
