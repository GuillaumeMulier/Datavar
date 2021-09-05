library(Datavar)
library(testthat)
library(magrittr)
library(waldo)

# Retourne les bonnes valeurs en descriptions croisée et univariée
test_that("Tab_quali gives the correct output for uni- and bivariate description of cyl in mtcars.", {
  expect_equal(compare(
    tab_quali(data = mtcars, x = cyl),
    structure(list(Variable = c("Total", "- cyl -", "** 8", "** 4", "** 6"),
                   Count = c("32", "n=32", "", "", ""),
                   Statistics = c("", "", "14(44%)", "11(34%)", "7(22%)")),
              row.names = c(NA, 5L),
              class = "data.frame")
  ) %>% length(),
  0)
  expect_equal(compare(
    tab_quali(data = mtcars, x = cyl, y = am),
    structure(list(Variable = c("Total", "- cyl -", "** 8", "** 4", "** 6"),
                   `Count (am:0)` = c("19", "n=19", "", "", ""),
                   `Statistics (am:0)` = c("", "", "12(63%)", "3(16%)", "4(21%)"),
                   `Count (am:1)` = c("13", "n=13", "", "", ""),
                   `Statistics (am:1)` = c("", "", "2(15%)", "8(62%)", "3(23%)")),
              row.names = c(NA, 5L),
              class = "data.frame")
  ) %>% length(),
  0)
})

# Les Pvalues sont-elles bonnes ?
test_that("Pvalues displayed are correct.", {
  expect_equal(tab_quali(mtcars, cyl, am, test = "chisq", prec = 2)[2, 6],
               "Faire des regroupements")
})
