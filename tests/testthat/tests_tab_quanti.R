library(Datavar)
library(testthat)
library(magrittr)
library(waldo)

# Retourne les bonnes valeurs en descriptions croisée et univariée
test_that("Tab_quanti gives the correct output for uni- and bivariate description of mpg in mtcars.", {
  expect_equal(compare(
    tab_quanti(data = mtcars, x = mpg, y = am, prec = 1),
    structure(list(Variable = "- mpg -", `Count (am:0)` = "n=19",
                   `Statistics (am:0)` = "17.3(14.9;19.2)[10.4;24.4]", `Count (am:1)` = "n=13",
                   `Statistics (am:1)` = "22.8(21.0;30.4)[15.0;33.9]"), row.names = c(NA, -1L), class = "data.frame")
  ) %>% length(),
  0)
  expect_equal(compare(
    tab_quanti(data = mtcars, x = mpg, prec = 1),
    structure(list(Variable = "- mpg -", Count = "n=32", Statistics = "19.2(15.4;22.8)[10.4;33.9]"),
              row.names = c(NA, -1L), class = "data.frame")
  ) %>% length(),
  0)
})

# Les Pvalues sont-elles les bonnes ?
test_that("Pvalues displayed are correct.", {
  expect_equal(tab_quanti(data = mtcars, x = mpg, y = am, prec = 1, test = "stud", chif_pval = 4)[1, 6] %>% as.numeric(),
               round(t.test(mtcars$mpg ~ mtcars$am)$p.value, 4))
  expect_warning(tab_quanti(data = mtcars, x = mpg, y = am, prec = 1, test = "wilc", chif_pval = 4))
})

# Noms de colonnes pas de la bonne longueur
test_that("Checking the length of nomcol specified.", {
  expect_error(tab_quanti(data = mtcars, x = mpg, nomcol = c("Test", "of", "nomcol's", "length")))
  expect_error(tab_quanti(data = mtcars, x = mpg, y = am, nomcol = c("Test", "of", "nomcol's", "length")))
})

# Checking for the crossing variable
test_that("Checking for the crossing variable.", {
  mtcars$am <- 1
  expect_error(tab_quanti(data = mtcars, x = mpg, y = am))
  expect_message(tab_quanti(data = mtcars, x = mpg, y = cyl))
})

# Il faut donner une variable numérique
test_that("Checking for the variable to describe.", {
  mtcars$mpg <- as.character(mtcars$mpg)
  expect_error(tab_quanti(data = mtcars, x = mpg))
  mtcars$disp <- as.factor(mtcars$disp)
  expect_error(tab_quanti(data = mtcars, x = disp))
})
