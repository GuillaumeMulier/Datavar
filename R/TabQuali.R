#' tab_quali
#'
#' Description of categorical variables
#'
#' A function to describe categorical variables with count and percentage (+ binomial confidence interval if supplied).
#'
#' You can also use it to cross it with a categorial variable and perform a comparison test (chisq test).
#'
#' The condition to perform the Chi-square test is that no more than 20% of the expected sizes are below 5 (Bouyer, 1996, p. 254).
#'
#' @param .Data The dataset that contains the variables.
#' @param x The categorical variable to describe. Should be the name without "".
#' @param y The qualitative variable to perform the bivariate description. Should be the name without "". If unspecified, univariate description.
#' @param prec Number of decimals for the percentages.
#' @param coefbin If TRUE displays the binomial confidence intervals.
#' @param test String giving the name of the comparison test performed.
#' @param chif_pval Number of decimals for the Pvalue if test is performed.
#' @param langue "fr" for french and "eng" for english. For the display in the table.
#' @param nomcol Vector of strings to name each column of the output. Automatic display if unspecified.
#' @param ordonnee If FALSE, displays the classes of x in decreasing count order. If TRUE, displays in the original order.
#' @param nomvariable The Name of the variable you want to display. If not supplied, the name of the variable in data is used.
#' @param simplif Not usefull, if TRUE will delete unused column 'Pvalue'.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @seealso \code{\link{descr}}
#'
#' @references Bouyer, J. (1996). \emph{Méthodes statistiques: médecine-biologie}. Estem.
#'
#' @examples
#' tab_quali(mtcars, cyl)
#' tab_quali(mtcars, cyl, am)
#' tab_quali(mtcars, cyl, am,
#'           nomvariable = "Number of cylinders",
#'           test = "chisq", chif_pval = 3)
TabQuali <- function(.Data,
                      x,
                      y = NULL,
                     Langue = "eng",
                      Prec = 0,
                     ConfInter = c("none", "normal", "exact", "jeffreys"),
                     ConfLevel = .95,
                      test = "none",
                      chif_pval = 2,
                      nomcol = NULL,
                      ordonnee = FALSE,
                      nomvariable = NULL,
                      simplif = TRUE) {

  # Interest variables defused so that it is possible to give unquoted arguments
  x <- rlang::enexpr(x)
  VarQuali <- rlang::eval_tidy(x, data = .Data)
  y <- rlang::enexpr(y)

  # Verifications
  Langue <- VerifArgs(Langue)
  Prec <- VerifArgs(Prec)
  ConfInter <- VerifArgs(ConfInter)
  ConfLevel <- VerifArgs(ConfLevel)
  VarBinaire <- VerifArgs(VarBinaire, NomCateg, x)
  NomLabel <- VerifArgs(NomLabel, VarBinaire, x)
  if (sum(!is.na(varquali)) == 0) stop(paste0("Variable ", cli::style_underline(cli::bg_br_red(cli::col_br_white(rlang::quo_name(x)))), " has 0 non missing values."), call. = FALSE)
  if (length(prec) != 1 || !is.numeric(prec) || prec %% 1 != 0 || prec < 0) stop("\"prec\" should be a whole positive number.", call. = FALSE)
  if (length(unique(varquali[!is.na(varquali)])) == 1) message(paste0("The variable to describe \"", rlang::quo_name(x), "\" only have 1 class."))
  if (is.null(nomvariable)) nomvariable <- paste0("- ", rlang::quo_name(x), " -")

  fprec <- paste0("%.", prec, "f")

  if (is.null(crois)) { # Description univariée

    # Stockage des pourcentages et des effectifs
    effectifs <- table(varquali)
    pourcents <- sprintf(fprec, prop.table(effectifs, margin = NULL) * 100)
    icinf <- purrr::map_chr(.x = as.numeric(effectifs),
                            .f = ~ sprintf(fprec, binom.test(.x, sum(!is.na(varquali)))$conf.int[1] * 100))
    icsup <- purrr::map_chr(.x = as.numeric(effectifs),
                            .f = ~ sprintf(fprec, binom.test(.x, sum(!is.na(varquali)))$conf.int[2] * 100))
    if (coefbin) {statist <- paste0(effectifs, "(", pourcents, "%[", icinf, "%;", icsup, "%])")} else {statist <- paste0(effectifs, "(", pourcents, "%)")}

    # Création du tableau de résultats
    nomtable <- data.frame(var = c("Total", nomvariable, paste0("** ", names(effectifs)), ifelse(langue == "fr", "** Manquants", ifelse(langue == "eng", "** Missings", "** ..."))),
                           eff = c(length(varquali), paste0("n=", sum(!is.na(varquali))), rep("", length(effectifs) + 1)),
                           stats = c("", "", statist, sum(is.na(varquali))),
                           stringsAsFactors = FALSE)

    # Noms des colonnes
    if (is.null(nomcol)) {
      if (langue == "fr") {colnames(nomtable) <- c("Variable", "Effectif", "Statistiques")}
      else if (langue == "eng") {colnames(nomtable) <- c("Variable", "Count", "Statistics")}
    } else {
      if (length(nomcol) != ncol(nomtable)) stop(paste0("\"nomcol\" argument isn't of length", ncol(nomtable), "."), call. = FALSE)
      colnames(nomtable) <- nomcol
    }

    # Si ordonnee est mis FALSE, va réarranger par effectif décroissant (valeur par défaut)
    if (!ordonnee) {
      nomtable[(3:(nrow(nomtable) - 1)), ] <- nomtable[order(as.numeric(gsub("^(\\d+)\\(.*$", "\\1", nomtable[(3:(nrow(nomtable) - 1)), 3])), decreasing = TRUE) + 2, ]
    }

  } else { # Description bivariée

    y <- rlang::sym(rlang::enexpr(y))
    y <- rlang::enquo(y)
    varcroise <- rlang::eval_tidy(y, data = .Data)

    # Vérifications supplémentaires
    test <- verif_test(test, type_var = "quali")
    if (length(unique(varcroise[!is.na(varcroise)])) < 2) stop("There must be at least 2 classes to perform a crossed description.", call. = FALSE)
    if (length(unique(varcroise[!is.na(varcroise)])) > 2) message("There are more than 2 classes. Some tests can be inappropriate.")
    if (!is.null(nomcol) & length(nomcol) != 2 * (length(unique(varcroise[!is.na(varcroise)])) + 1)) stop(paste0("\"nomcol\" must be a vector of length ", 2 * (length(unique(varcroise[!is.na(varcroise)])) + 1), "."), call. = FALSE)
    if (test != "none" & length(unique(varquali[!is.na(varquali)])) == 1) {
      stop(paste0("Variable \"", rlang::enquo(y), "\" with only 1 class, no feasable test.\nChange to test = \"none\"."), call. = FALSE)
    }

    # Stockage des pourcentages et des effectifs
    tab_crois      <- table(varquali, varcroise)
    tab            <- table(varquali)
    tab_tot        <- apply(tab_crois, 2, sum)
    tab_crois_prop <- prop.table(tab_crois, 2)
    tot_nona       <- tapply(varquali, varcroise, length)
    tot_na         <- tapply(varquali, varcroise, function(x) sum(is.na(x)))
    statist <- purrr::map_dfc(.x = colnames(tab_crois) %>%
                                rlang::set_names(paste0(rlang::as_label(rlang::enquo(y)), "_", colnames(tab_crois))),
                              .f = function(x) {
                                donnees <- tab_crois[, x]
                                prop <- tab_crois_prop[, x]
                                donnees_tot <- tab_tot[x]
                                purrr::map2_chr(donnees, prop,
                                                ~ if (coefbin) {
                                                  if (donnees_tot != 0) {
                                                    paste0(.x, "(", sprintf(fprec, 100 * .y), "%[",
                                                           sprintf(fprec, binom.test(.x, donnees_tot)$conf.int[1] * 100),
                                                           "%;", sprintf(fprec, binom.test(.x, donnees_tot)$conf.int[2] * 100), "%])")
                                                  } else {
                                                    paste0("##")
                                                  }
                                                } else {
                                                  paste0(.x, "(", sprintf(fprec, 100 * .y), "%)")
                                                })
                              })

    # Création du tableau vide qui recevra les résultats
    nomtable <- data.frame(matrix("", ncol = (ncol(tab_crois) + 1) * 2, nrow = length(unique(varquali[!is.na(varquali)])) + 3), stringsAsFactors = FALSE)
    if (!is.null(nomcol)) {
      colnames(nomtable) <- nomcol
    } else {
      if (langue == "fr") {
        colnames(nomtable) <- c("Variable",
                                paste0(rep(c("Effectif", "Statistiques"), length.out = ncol(nomtable) - 2),
                                       " (", rlang::quo_name(y), ":", rep(names(table(varcroise[!is.na(varcroise)])), each = 2), ")"),
                                "Pvalue")
      } else if (langue == "eng") {
        colnames(nomtable) <- c("Variable",
                                paste0(rep(c("Count", "Statistics"), length.out = ncol(nomtable) - 2),
                                       " (", rlang::quo_name(y), ":", rep(names(table(varcroise[!is.na(varcroise)])), each = 2), ")"),
                                "Pvalue")
      }
    }

    # Remplissage du tableau avec les valeurs
    nb_classes <- length(unique(varcroise[!is.na(varcroise)]))
    nomtable[1, (2 * seq_len(nb_classes))] <- tot_nona
    nomtable[1, 1] <- "Total"
    nomtable[nrow(nomtable), (2 * seq_len(nb_classes)) + 1] <- tot_na
    nomtable[nrow(nomtable), 1] <- ifelse(langue == "fr", "** Manquants", ifelse(langue == "eng", "** Missings", "** ..."))
    nomtable[2, (2 * seq_len(nb_classes))] <- paste0("n=", tab_tot)
    nomtable[3:(nrow(nomtable) - 1), 1] <- paste0("** ", names(tab))
    nomtable[3:(nrow(nomtable) - 1), (2 * seq_len(nb_classes)) + 1] <- statist
    nomtable[2, 1] <- nomvariable

    # Si ordonnee est mis FALSE, va réarranger par effectif décroissant (valeur par défaut)
    if (!ordonnee) {
      nomtable[(3:(nrow(nomtable) - 1)), ] <- nomtable[order(tab, decreasing = TRUE) + 2, ]
    }

    # S'il faut faire un test de comparaison (Fisher ou Chi2)
    if (test != "none") {
      if (length(chif_pval) != 1 || !is.numeric(chif_pval) || chif_pval %% 1 != 0) stop("\"chif_pval\" should be a whole positive number.", call. = FALSE)
      if (suppressWarnings(mean(chisq.test(varquali, varcroise)$expected <= 5) <= .2)) {
        suppressWarnings(testval <- arrondi_pv(chisq.test(varquali, varcroise, correct = FALSE)$p.value, chif_pval))
        } else {
          testval <- "Faire des regroupements"
        }
      nomtable[2, "Pvalue"] <- testval
    }

    if (simplif && all(nomtable[, "Pvalue"] == "")) nomtable <- nomtable[, -ncol(nomtable)]

  }

  if (sum(is.na(varquali)) == 0) nomtable <- nomtable[- nrow(nomtable), ]

  return(nomtable)

}

