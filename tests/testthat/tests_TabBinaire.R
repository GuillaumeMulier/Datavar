library(Datavar)
library(testthat)
library(magrittr)
library(waldo)

# Retourne les bonnes valeurs en descriptions croisée et univariée
test_that("TabBinaire gives the correct output for uni- and bivariate description of vs in mtcars.", {
  expect_equal(
    suppressWarnings(TabBinaire(.Data = mtcars, x = vs)),
    structure(list(Variable = c("vs=1", "vs=1"), Label = c("n, %",
                                                           "  Missings"), Statistics = c("14/32 (44%)", "0")), class = c("tab_datavar",
                                                                                                                         "data.frame"), row.names = c(NA, -2L), crossed = "univariate"))
  expect_equal(
    suppressWarnings(TabBinaire(.Data = mtcars, x = vs, y = am)),
    structure(list(Variable = c("vs=1", "vs=1"), Label = c("n, %",
                                                           "  Missings"), `Statistics (am=1)` = c("7/19 (37%)", "0"), `Statistics (am=0)` = c("7/13 (54%)",
                                                                                                                                              "0")), class = c("tab_datavar", "data.frame"), row.names = c(NA,
                                                                                                                                                                                                           -2L), crossed = "multivariate"))
  expect_equal(
    suppressWarnings(TabBinaire(.Data = mtcars %>% mutate(vs = ifelse(row_number() == 1, NA, vs)), x = vs, y = am)),
    structure(list(Variable = c("vs=1", "vs=1"), Label = c("n, %",
                                                           "  Missings"), `Statistics (am=1)` = c("7/19 (37%)", "0"), `Statistics (am=0)` = c("7/12 (58%)",
                                                                                                                                              "1")), class = c("tab_datavar", "data.frame"), row.names = c(NA,
                                                                                                                                                                                                           -2L), crossed = "multivariate"))
})

# Les Pvalues sont-elles bonnes ?
test_that("Displayed Pvalues are correct.", {
  expect_equal(TabBinaire(.Data = mtcars, x = vs, y = am, Test = "chisq", ChifPval = 2)[1, 5] %>% as.numeric(),
               round(chisq.test(x = mtcars$vs, y = mtcars$am, correct = FALSE)$p.value, 2))
  expect_equal(TabBinaire(.Data = mtcars, x = vs, y = am, Test = "fisher", ChifPval = 2)[1, 5] %>% as.numeric(),
               round(fisher.test(x = mtcars$vs, y = mtcars$am)$p.value, 2))
})

# Erreur dans longueur du vecteur de nom de colonnes
test_that("Checking the length of nomcol.", {
  expect_error(TabBinaire(.Data = mtcars, x = vs, NomCol = c("Test", "of", "nomcol's", "length")))
  expect_error(TabBinaire(.Data = mtcars, x = vs, y = am, NomCol = c("Test", "of", "nomcol's", "length", ".")))
})

# Checking for the crossing variable
test_that("Checking for the crossing variable.", {
  mtcars$am <- 1
  expect_error(TabBinaire(.Data = mtcars, x = vs, y = am))
})

# Il faut donner une variable binaire
test_that("Checking for the variable to describe.", {
  expect_error(TabBinaire(.Data = mtcars, x = cyl))
  mtcars$am2 <- 1
  expect_warning(TabBinaire(.Data = mtcars, x = am2))
})

# Check des intervalles de confiance
test_that("Displayed confidence intervals are correct.", {
  expect_equal(TabBinaire(.Data = mtcars, x = vs, ConfInter = "normal")[1, 3],
               "14/32 (44%[27;61])")
  expect_equal(TabBinaire(.Data = mtcars, x = vs, ConfInter = "exact")[1, 3],
               "14/32 (44%[26;62])")
  expect_equal(TabBinaire(.Data = mtcars, x = vs, ConfInter = "jeffreys")[1, 3],
               "14/32 (44%[28;61])")
})
