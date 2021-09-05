library(Datavar)
library(testthat)
library(magrittr)
library(waldo)

# Retourne les bonnes valeurs en descriptions croisée et univariée
test_that("Tab_binaire gives the correct output for uni- and bivariate description of vs in mtcars.", {
  expect_equal(compare(
    tab_binaire(data = mtcars, x = vs),
    structure(list(Variable = c("Total", "** vs:1"), Count = c("32",
                                                               "n=32"), Statistics = c("", "14(44%)")), row.names = 1:2, class = "data.frame")
  ) %>% length(),
  0)
  expect_equal(compare(
    tab_binaire(data = mtcars, x = vs, y = am),
    structure(list(Variable = c("Total", "** vs:1"), `Count (am:0)` = c("19",
                                                                        "n=19"), `Statistics (am:0)` = c("", "7(37%)"), `Count (am:1)` = c("13",
                                                                                                                                           "n=13"), `Statistics (am:1)` = c("", "7(54%)")), row.names = 1:2, class = "data.frame")
  ) %>% length(),
  0)
})

# Les Pvalues sont-elles bonnes ?
test_that("Displayed Pvalues are correct.", {
  expect_equal(tab_binaire(data = mtcars, x = vs, y = am, test = "chisq", chif_pval = 2)[2, 6] %>% as.numeric(),
               round(chisq.test(x = mtcars$vs, y = mtcars$am, correct = FALSE)$p.value, 2))
  expect_equal(tab_binaire(data = mtcars, x = vs, y = am, test = "fish", chif_pval = 2)[2, 6] %>% as.numeric(),
               round(fisher.test(x = mtcars$vs, y = mtcars$am)$p.value, 2))
})

# Erreur dans longueur du vecteur de nom de colonnes
test_that("Checking the length of nomcol.", {
  expect_error(tab_binaire(data = mtcars, x = vs, nomcol = c("Test", "of", "nomcol's", "length")))
  expect_error(tab_binaire(data = mtcars, x = vs, y = am, nomcol = c("Test", "of", "nomcol's", "length")))
})

# Checking for the crossing variable
test_that("Checking for the crossing variable.", {
  mtcars$am <- 1
  expect_error(tab_binaire(data = mtcars, x = vs, y = am))
  expect_message(tab_binaire(data = mtcars, x = vs, y = cyl))
})

# Il faut donner une variable binaire
test_that("Checking for the variable to describe.", {
  expect_error(tab_binaire(data = mtcars, x = cyl))
  mtcars$am2 <- 1
  expect_message(tab_binaire(data = mtcars, x = am2))
})
