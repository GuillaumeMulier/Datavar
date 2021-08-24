#' tab_quanti
#'
#' Description of quantitative variables
#'
#' A function to describe quantitative variables with mean, median, standard deviation, interquartile and range.
#'
#' You can also use it to cross it with a categorial variable and perform a comparison test (student or wilcoxon test). For now no tests are performed with crossing with a variable that has more than 2 classes.
#'
#' @param data The dataset that contains the variables.
#' @param x The quantitative variable to describe.
#' @param y The qualitative variable to perform the bivariate description. If unspecified, univariate description.
#' @param prec Number of decimals for the statistics.
#' @param chif_pval Number of decimals for the Pvalue if needed.
#' @param mode String to indicate what statistics to display. If unspecified, by default give median, interquartile and range ("med" for
#' median, "moy" for mean, "sd" for standard deviation, "iq" for interquartile and "rg" fo range).
#' @param test String giving the test to compare the statistic between the 2 groups if needed. None if unspecified.
#' @param langue "fr" for french and "eng" for english. For the display in the table.
#' @param nomcol Vector of strings to name each column of the output. Automatic display if unspecified.
#' @param nomvariable String giving the name you want to display in the table. Automatic display if unspecified.
#' @param simplif Not usefull, if TRUE will delete unused column 'Pvalue'.
#'
#' @export
#'
#' @seealso \code{\link{descr}}
#'
#' @examples
#' tab_quanti(data = mtcars, x = mpg, prec = 1)
#' tab_quanti(data = mtcars, x = mpg, y = am, prec = 1)
#' tab_quanti(data = mtcars, x = mpg, y = am, prec = 1,
#'            nomvariable = "Miles per gallon",
#'            test = "stud", chif_pval = 3)
tab_quanti <- function(data,
                       x,
                       y = NULL,
                       prec = 0,
                       chif_pval = 2,
                       mode = "mediqrg",
                       test = "none",
                       langue = "eng",
                       nomcol = NULL,
                       nomvariable = NULL,
                       simplif = TRUE) {

  x <- rlang::sym(rlang::enexpr(x))
  x <- rlang::enquo(x)
  varquanti <- rlang::eval_tidy(x, data = data)
  if (is.null(nomvariable)) {
    nomvariable <- paste0("- ", rlang::quo_name(x), " -")
  } else {
    nomvariable <- paste0("- ", nomvariable, " -")
  }

  crois <- tryCatch(
    expr = {y <- rlang::sym(rlang::enexpr(y));rlang::eval_tidy(rlang::enquo(y), data = data)},
    error = function(e) NULL
  )

  # Vérifications avant de faire tourner la fonction
  langue <- verif_langue(langue)
  mode   <- verif_mode(mode)
  if (sum(!is.na(varquanti)) == 0) stop(paste0("Variable ", rlang::quo_name(x), " has 0 non missing values."), call. = FALSE)
  if (length(prec) != 1 || !is.numeric(prec) || prec %% 1 != 0 || prec < 0) stop("\"prec\" should be a whole positive number.", call. = FALSE)
  if (!is.numeric(varquanti)) stop(paste0("The variable to describe \"", rlang::quo_name(x), "\" isn't of numeric type.", call. = FALSE))

  fprec <- paste0("%.", prec, "f")

  if (is.null(crois)) { # Description univariée

    # Vérification supplémentaire
    if (!is.null(nomcol) & length(nomcol) != 3) stop("\"nomcol\" must be a vector of length 3.", call. = FALSE)

    # Stockage des statistiques
    numb     <- sum(!is.na(varquanti))
    mediane  <- sprintf(fprec, median(varquanti, na.rm = TRUE))
    moyenne  <- sprintf(fprec, mean(varquanti, na.rm = TRUE))
    ecartype <- sprintf(fprec, sd(varquanti, na.rm = TRUE))
    q1       <- sprintf(fprec, as.numeric(Q1(varquanti)))
    q3       <- sprintf(fprec, as.numeric(Q3(varquanti)))
    mmin     <- sprintf(fprec, min(varquanti, na.rm = TRUE))
    mmax     <- sprintf(fprec, max(varquanti, na.rm = TRUE))

    # Création du tableau pour recevoir les résultats
    nomtable <- data.frame(matrix("", nrow = 1, ncol = 3), stringsAsFactors = FALSE)
    if (is.null(nomcol)) {
      if (langue == "fr") {colnames(nomtable) <- c("Variable", "Effectif", "Statistiques")}
      else if (langue == "eng") {colnames(nomtable) <- c("Variable", "Count", "Statistics")}
    } else {
      if (length(nomcol) != ncol(nomtable)) stop(paste0("'nomcol' argument isn't of length", ncol(nomtable), "."), call. = FALSE)
      colnames(nomtable) <- nomcol
    }

    # Remplissage des valeurs dans le tableau
    nomtable[1, 2] <- paste0("n=", numb)
    nomtable[1, 3] <- paste0(paste(ifelse(grepl(x = mode, pattern = "moy"), moyenne, ""),
                                   ifelse(grepl(x = mode, pattern = "med"), mediane, ""),
                                   sep = ifelse(grepl(x = mode, pattern = "moy") && grepl(x = mode, pattern = "med"), "/", "")),
                             ifelse(grepl(x = mode, pattern = "sd"), paste0("{", ecartype, "}"), ""),
                             ifelse(grepl(x = mode, pattern = "iq"), paste0("(", q1, ";", q3, ")"), ""),
                             ifelse(grepl(x = mode, pattern = "rg"), paste0("[", mmin, ";", mmax, "]"), ""))

  } else { # Description bivariée

    y <- rlang::sym(rlang::enexpr(y))
    y <- rlang::enquo(y)
    varcroise <- rlang::eval_tidy(y, data = data)

    # Vérifications supplémentaires
    test <- verif_test(test, type_var = "quanti")
    if (length(unique(varcroise[!is.na(varcroise)])) < 2) stop("There must be at least 2 classes to perform a crossed description.", call. = FALSE)
    if (length(unique(varcroise[!is.na(varcroise)])) > 2) message("There are more than 2 classes. Some tests can be inappropriate.")
    if (!is.null(nomcol) & length(nomcol) != 2 * (length(unique(varcroise[!is.na(varcroise)])) + 1)) stop(paste0("\"nomcol\" must be a vector of length ", 2 * (length(unique(varcroise[!is.na(varcroise)])) + 1), "."), call. = FALSE)

    # Stockage des statistiques
    numb     <- tapply(X = varquanti, INDEX = varcroise, FUN = function (x) {sum(!is.na(x))})
    mediane  <- sprintf(fprec, tapply(varquanti, varcroise, median, na.rm = TRUE))
    moyenne  <- sprintf(fprec, tapply(varquanti, varcroise, mean, na.rm = TRUE))
    ecartype <- sprintf(fprec, tapply(varquanti, varcroise, sd, na.rm = TRUE))
    q1       <- sprintf(fprec, tapply(varquanti, varcroise, Q1))
    q3       <- sprintf(fprec, tapply(varquanti, varcroise, Q3))
    mmin     <- sprintf(fprec, tapply(varquanti, varcroise, min, na.rm = TRUE))
    mmax     <- sprintf(fprec, tapply(varquanti, varcroise, max, na.rm = TRUE))

    # Création du tableau pour recevoir les résultats
    nomtable <- data.frame(matrix("", ncol = 2 * (length(unique(varcroise[!is.na(varcroise)])) + 1), nrow = 1), stringsAsFactors = FALSE)
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
    nomtable[1, (2 * seq_len(nb_classes))] <- paste0("n=", numb)
    nomtable[1, (2 * seq_len(nb_classes) + 1)] <- paste0(paste(if (grepl(x = mode, pattern = "moy")) {moyenne} else {rep("", times = length(moyenne))},
                                                               if (grepl(x = mode, pattern = "med")) {mediane} else {rep("", times = length(mediane))},
                                                               sep = ifelse(grepl(x = mode, pattern = "moy") && grepl(x = mode, pattern = "med"), "/", "")),
                                                         if (grepl(x = mode, pattern = "sd")) {paste0("{", ecartype, "}")} else {rep("", times = length(ecartype))},
                                                         if (grepl(x = mode, pattern = "iq")) {paste0("(", q1, ";", q3, ")")} else {rep("", times = length(q1))},
                                                         if (grepl(x = mode, pattern = "rg")) {paste0("[", mmin, ";", mmax, "]")} else {rep("", times = length(mmin))})
    if (test != "none") {
      if (length(chif_pval) != 1 || !is.numeric(chif_pval) || chif_pval %% 1 != 0) stop("\"chif_pval\" should be a whole positive number.", call. = FALSE)
      if (test == "student") {
        nomtable[1, "Pvalue"] <- arrondi_pv(t.test(varquanti ~ varcroise)$p.value, chif_pval)
      } else if (test == "wilcoxon") {
        nomtable[1, "Pvalue"] <- arrondi_pv(wilcox.test(varquanti ~ varcroise)$p.value, chif_pval)
      }
    }

    if (simplif && nomtable[1, "Pvalue"] == "") nomtable <- nomtable[, -ncol(nomtable)]

  }

  nomtable[1, 1] <- nomvariable

  return(nomtable)

}
