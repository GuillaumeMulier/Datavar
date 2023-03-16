library(Datavar)
library(testthat)
library(magrittr)
library(waldo)

# Retourne les bonnes valeurs en descriptions croisée et univariée
test_that("TabQuanti gives the correct output for uni- and bivariate description of mpg in mtcars.", {
  expect_equal(
    TabQuanti(.Data = mtcars, x = mpg, y = am, Prec = 1),
    structure(list(Variable = c("mpg", "mpg", "mpg"), Label = c("N",
                                                                "Median, (Q1-Q3)", "[min-max]"), `Statistics (am=1)` = c("19",
                                                                                                                         "17.3, (14.9-19.2)", "[10.4-24.4]"), `Statistics (am=0)` = c("13",
                                                                                                                                                                                      "22.8, (21.0-30.4)", "[15.0-33.9]")), row.names = c("1", "3",
                                                                                                                                                                                                                                          "4"), crossed = "multivariate", class = c("tab_datavar", "data.frame"
                                                                                                                                                                                                                                          )))
  expect_equal(
    TabQuanti(.Data = mtcars, x = mpg, Prec = 1),
    structure(list(Variable = c("mpg", "mpg", "mpg"), Label = c("N",
                                                                "Median, (Q1-Q3)", "[min-max]"), Statistics = c("32", "19.2, (15.4-22.8)",
                                                                                                                "[10.4-33.9]")), row.names = c("1", "3", "4"), crossed = "univariate", class = c("tab_datavar",
                                                                                                                                                                                                 "data.frame")))
  expect_equal(
    TabQuanti(.Data = mtcars %>% mutate(mpg = ifelse(row_number() == 1, NA, mpg)), x = mpg, y = am),
    structure(list(Variable = c("mpg", "mpg", "mpg"), Label = c("N, N~Miss~",
                                                                "Median, (Q1-Q3)", "[min-max]"), `Statistics (am=1)` = c("19, 0",
                                                                                                                         "17, (15-19)", "[10-24]"), `Statistics (am=0)` = c("12, 1", "24, (21-30)",
                                                                                                                                                                            "[15-34]")), class = c("tab_datavar", "data.frame"), row.names = c("1",
                                                                                                                                                                                                                                               "3", "4"), crossed = "multivariate"))
})

# Les Pvalues sont-elles les bonnes ?
test_that("Pvalues displayed are correct.", {
  expect_equal(TabQuanti(.Data = mtcars, x = mpg, y = am, Prec = 1, Test = "stud", ChifPval = 4)[1, 5] %>% as.numeric(),
               round(t.test(mtcars$mpg ~ mtcars$am)$p.value, 4))
  expect_equal(suppressWarnings(TabQuanti(.Data = mtcars, x = mpg, y = am, Prec = 1, Test = "wilc", ChifPval = 4))[1, 5] %>% as.numeric(),
               round(suppressWarnings(wilcox.test(mtcars$mpg ~ mtcars$am))$p.value, 4))
})

# Noms de colonnes pas de la bonne longueur
test_that("Checking the length of NomCol specified.", {
  expect_error(TabQuanti(.Data = mtcars, x = mpg, NomCol = c("Test", "of", "nomcol's", "length")))
  expect_error(TabQuanti(.Data = mtcars, x = mpg, y = am, NomCol = c("Test", "of", "nomcol's", "length", ".")))
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
