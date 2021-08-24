#' tab_binaire
#'
#' Description of binary variables
#'
#' A function to describe binary variables with count and percentage (+ binomial confidence interval if supplied).
#'
#' You can also use it to cross it with a categorial variable and perform a comparison test (chisq or fisher test). For now no tests are performed with crossing with a variable that has more than 2 classes.
#'
#' @param data The dataset that contains the variables.
#' @param x The binary variable to describe.
#' @param y The qualitative variable to perform the bivariate description. If unspecified, univariate description.
#' @param prec Number of decimals for the percentages.
#' @param coefbin If TRUE displays the binomial confidence intervals.
#' @param chif_pval Number of decimals for the Pvalue if test is performed.
#' @param test String giving the name of the comparison test performed.
#' @param langue "fr" for french and "eng" for english. For the display in the table.
#' @param nomcol Vector of strings to name each column of the output. Automatic display if unspecified.
#' @param nomcateg Sting giving the name of the class that you want to display in the table.
#' @param label The label you want to display for nomcateg.
#' @param simplif Not usefull, if TRUE will delete unused column 'Pvalue'.
#'
#' @export
#'
#' @seealso \code{\link{descr}}
#'
#' @examples
#' tab_binaire(mtcars, am)
#' tab_binaire(mtcars, am, vs)
#' tab_binaire(mtcars, am, vs,
#'             nomcateg = "0", label = "Automatic transmission",
#'             test = "fisher")
tab_binaire <- function(data,
                        x,
                        y = NULL,
                        prec = 0,
                        coefbin = FALSE,
                        chif_pval = 2,
                        test = "none",
                        langue = "eng",
                        nomcol = NULL,
                        nomcateg = NULL,
                        label = NULL,
                        simplif = TRUE) {

  x <- rlang::sym(rlang::enexpr(x))
  x <- rlang::enquo(x)
  varbinaire <- rlang::eval_tidy(x, data = data)

  crois <- tryCatch(
    expr = {y <- rlang::sym(rlang::enexpr(y));rlang::eval_tidy(rlang::enquo(y), data = data)},
    error = function(e) NULL
  )

  # Vérifications avant de faire tourner la fonction
  langue <- verif_langue(langue)
  if (sum(!is.na(varbinaire)) == 0) stop(paste0("Variable ", rlang::quo_name(x), " has 0 non missing values."), call. = FALSE)
  if (length(prec) != 1 || !is.numeric(prec) || prec %% 1 != 0 || prec < 0) stop("\"prec\" should be a whole positive number.", call. = FALSE)
  if (length(unique(varbinaire[!is.na(varbinaire)])) > 2) stop(paste0("The variable to describe \"", rlang::quo_name(x), "\" isn't a binary variable."), call. = FALSE)
  if (length(unique(varbinaire[!is.na(varbinaire)])) == 1) message(paste0("The variable to describe \"", rlang::quo_name(x), "\" only have 1 class."))
  if (!is.null(nomcateg) && nomcateg == "") nomcateg <- NULL
  if (!is.null(label) && is.null(nomcateg)) {
    warning(
      paste0("Specified label (\"", label, "\") with unspecified nomcateg. Check if the given label corresponds to the class taken (\"",
             names(table(varbinaire))[length(names(table(varbinaire)))], "\")."),
      call. = FALSE
    )
  }
  if (!is.null(nomcateg)) {
    if (nomcateg %nin% names(table(varbinaire))) {
      stop(paste0("Class '", nomcateg, "' isn't in variable: ", rlang::quo_name(x),
                  "\nPossible classes: '", paste(names(table(varbinaire)), collapse = "', '"), "'."),
           call. = FALSE)
    }
  } else {
    nomcateg <- names(table(varbinaire))[length(names(table(varbinaire)))]
  }
  if (is.null(label)) label <- paste0(rlang::quo_name(x), ":", nomcateg)

  fprec <- paste0("%.", prec, "f")

  if (is.null(crois)) { # Description univariée

    if (sum(!is.na(varbinaire)) == 0) stop("The variable \"", rlang::quo_name(x), "\" only has NA values. It's useless to describe it.", call. = FALSE)

    # Stockage des effectifs et des pourcentages (+ IC)
    effectifs <- as.numeric(table(varbinaire == rep(nomcateg, length(varbinaire))))
    pourcents <- sprintf(fprec, prop.table(effectifs, margin = NULL) * 100)
    effectifs <- effectifs[length(effectifs)]
    pourcents <- pourcents[length(pourcents)]
    icinf <- sprintf(fprec, binom.test(as.numeric(as.character(effectifs)), sum(!is.na(varbinaire)))$conf.int[1] * 100)
    icsup <- sprintf(fprec, binom.test(as.numeric(as.character(effectifs)), sum(!is.na(varbinaire)))$conf.int[2] * 100)
    if (coefbin) {statist <- paste0(effectifs, "(", pourcents, "%[", icinf, "%;", icsup, "%])")} else {statist <- paste0(effectifs, "(", pourcents, "%)")}

    # Création du tableau de résultats
    nomtable <- data.frame(var = c("Total", paste0("** ", label), ifelse(langue == "fr", "** Manquants", ifelse(langue == "eng", "** Missings", "** ..."))),
                           eff = c(length(varbinaire), paste0("n=", sum(!is.na(varbinaire))), ""),
                           stats = c("", statist, sum(is.na(varbinaire))),
                           stringsAsFactors = FALSE)

    # Noms des colonnes
    if (is.null(nomcol)) {
      if (langue == "fr") {colnames(nomtable) <- c("Variable", "Effectif", "Statistiques")}
      else if (langue == "eng") {colnames(nomtable) <- c("Variable", "Count", "Statistics")}
    } else {
      if (length(nomcol) != ncol(nomtable)) stop(paste0("'nomcol' argument isn't of length", ncol(nomtable), "."), call. = FALSE)
      colnames(nomtable) <- nomcol
    }

  } else { # Description bivariée

    y <- rlang::sym(rlang::enexpr(y))
    y <- rlang::enquo(y)
    varcroise <- rlang::eval_tidy(y, data = data)

    # Vérifications supplémentaires
    test <- verif_test(test, type_var = "binaire")
    if (length(unique(varcroise[!is.na(varcroise)])) < 2) stop("There must be at least 2 classes to perform a crossed description.", call. = FALSE)
    if (length(unique(varcroise[!is.na(varcroise)])) > 2) message("There are more than 2 classes. Some tests can be inappropriate.")
    if (!is.null(nomcol) & length(nomcol) != 2 * (length(unique(varcroise[!is.na(varcroise)])) + 1)) stop(paste0("\"nomcol\" must be a vector of length ", 2 * (length(unique(varcroise[!is.na(varcroise)])) + 1), "."), call. = FALSE)
    if (test != "none" & length(unique(varbinaire[!is.na(varbinaire)])) == 1) {
      stop(paste0("Variable \"", rlang::enquo(y), "\" with only 1 class, no feasable test.\nChange to test = \"none\"."), call. = FALSE)
    }

    # Effectifs et pourcentages en colonne
    tab      <- table(varbinaire == rep(nomcateg, length(varbinaire)), varcroise)
    tab_prop <- prop.table(tab, 2)
    tab_tot  <- apply(tab, 2, sum)
    tot_nona <- tapply(varbinaire, varcroise, length)
    tot_na   <- tapply(varbinaire, varcroise, function(x) sum(is.na(x)))
    tab      <- tab[nrow(tab), ]
    tab_prop <- sprintf(fprec, 100 * tab_prop[nrow(tab_prop), ])
    icinf    <- purrr::map2_chr(.x = tab, .y = tab_tot,
                                .f = function(x, y) {
                                  if (y == 0) {
                                    return("##")
                                  } else {
                                    sprintf(fprec,
                                            binom.test(x, y)$conf.int[1] * 100)
                                  }
                                })
    icsup    <- purrr::map2_chr(.x = tab, .y = tab_tot,
                             .f = function(x, y) {
                               if (y == 0) {
                                 return("##")
                               } else {
                                 sprintf(fprec,
                                         binom.test(x, y)$conf.int[2] * 100)
                               }
                             })

    # Création du tableau vide qui recevra les résultats
    nomtable <- data.frame(matrix("", ncol = (length(tab) + 1) * 2, nrow = 3), stringsAsFactors = FALSE)
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
    nomtable[3, (2 * seq_len(nb_classes)) + 1] <- tot_na
    nomtable[3, 1] <- ifelse(langue == "fr", "** Manquants", ifelse(langue == "eng", "** Missings", "** ..."))
    nomtable[2, (2 * seq_len(nb_classes))] <- paste0("n=", tab_tot)
    if (coefbin) {
      nomtable[2, (2 * seq_len(nb_classes)) + 1] <- paste0(tab, "(", tab_prop, "%[", icinf, "%;", icsup, "%])")
    } else {
      nomtable[2, (2 * seq_len(nb_classes)) + 1] <- paste0(tab, "(", tab_prop, "%)")
    }
    nomtable[2, 1] <- paste0("** ", label)

    # S'il faut faire un test de comparaison (Fisher ou Chi2)
    if (test != "none") {
      if (length(chif_pval) != 1 || !is.numeric(chif_pval) || chif_pval %% 1 != 0) stop("\"chif_pval\" should be a whole positive number.", call. = FALSE)
      if (test == "fisher") {
        testval <- arrondi_pv(fisher.test(varbinaire, varcroise)$p.value, chif_pval)
      } else if (test == "chisq") {
        if (suppressWarnings(all(chisq.test(varbinaire, varcroise)$expected >= 5))) {
          testval <- arrondi_pv(chisq.test(varbinaire, varcroise, correct = FALSE)$p.value, chif_pval)
        } else if (suppressWarnings(all(chisq.test(varbinaire, varcroise)$expected >= 3))) {
          testval <- arrondi_pv(chisq.test(varbinaire, varcroise, correct = TRUE)$p.value, chif_pval)
        } else {
          testval <- "Hors conditions"
        }
      }
      nomtable[2, "Pvalue"] <- testval
    }

    if (simplif && all(nomtable[, "Pvalue"] == "")) nomtable <- nomtable[, -ncol(nomtable)]

  }

  if (sum(is.na(varbinaire)) == 0) nomtable <- nomtable[- nrow(nomtable), ]

  return(nomtable)

}
