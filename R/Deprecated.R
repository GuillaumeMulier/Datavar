#' descr_var
#'
#' @param data Data.frame of the data.
#' @param ligne_datavar The line in datavar of the variable to describe.
#' @param y The variable used to cross.
#' @param nomcol Vector of the names of the columns.
#' @param langue Language.
#'
#' @importFrom rlang !!
#'
#' @export
descr_var <- function(data,
                      ligne_datavar,
                      y,
                      nomcol,
                      langue) {

  x <- as.character(ligne_datavar[[1]])
  crois <- tryCatch(
    expr = {y <- rlang::expr_text(rlang::ensym(y))},
    error = function(e) NULL
  )

  if (is.null(crois)) {

    if (ligne_datavar[[2]] == "quanti") {

      tableau <- tab_quanti(data = data,
                            x = !!x,
                            prec = ifelse(!is.na(ligne_datavar[[4]]), as.numeric(ligne_datavar[[4]]), 0),
                            chif_pval = ifelse(!is.na(ligne_datavar[[11]]), as.numeric(ligne_datavar[[11]]), 2),
                            mode = ifelse(!is.na(ligne_datavar[[9]]), as.character(ligne_datavar[[9]]), "mediqrg"),
                            test = ifelse(!is.na(ligne_datavar[[10]]), as.character(ligne_datavar[[10]]), "none"),
                            langue = langue,
                            nomcol = nomcol,
                            nomvariable = if (!is.na(ligne_datavar[[7]])) {as.character(ligne_datavar[[7]])} else {NULL},
                            simplif = FALSE)

    } else if (ligne_datavar[[2]] == "binary") {

      tableau <- tab_binaire(data = data,
                             x = !!x,
                             Prec = ifelse(!is.na(ligne_datavar[[4]]), as.numeric(ligne_datavar[[4]]), 0),
                             coefbin = ifelse(!is.na(ligne_datavar[[3]]), as.logical(ligne_datavar[[3]]), FALSE),
                             chif_pval = ifelse(!is.na(ligne_datavar[[11]]), as.numeric(ligne_datavar[[11]]), 2),
                             test = ifelse(!is.na(ligne_datavar[[10]]), as.character(ligne_datavar[[10]]), "none"),
                             Langue = langue,
                             nomcol = nomcol,
                             nomcateg = if (!is.na(ligne_datavar[[5]])) {as.character(ligne_datavar[[5]])} else {NULL},
                             label = if (!is.na(ligne_datavar[[6]])) {as.character(ligne_datavar[[6]])} else {NULL},
                             simplif = FALSE)

    } else if (ligne_datavar[[2]] == "quali") {

      tableau <- tab_quali(data = data,
                           x = !!x,
                           prec = ifelse(!is.na(ligne_datavar[[4]]), as.numeric(ligne_datavar[[4]]), 0),
                           coefbin = ifelse(!is.na(ligne_datavar[[3]]), as.logical(ligne_datavar[[3]]), FALSE),
                           test = ifelse(!is.na(ligne_datavar[[10]]), as.character(ligne_datavar[[10]]), "none"),
                           chif_pval = ifelse(!is.na(ligne_datavar[[11]]), as.numeric(ligne_datavar[[11]]), 2),
                           langue = langue,
                           nomcol = nomcol,
                           ordonnee = ifelse(!is.na(ligne_datavar[[8]]), as.logical(ligne_datavar[[8]]), FALSE),
                           nomvariable = if (!is.na(ligne_datavar[[7]])) {as.character(ligne_datavar[[7]])} else {NULL},
                           simplif = FALSE)

    }

  } else {

    y <- rlang::expr_text(rlang::ensym(y))

    if (ligne_datavar[[2]] == "quanti") {

      tableau <- tab_quanti(data = data,
                            x = !!x,
                            y = !!y,
                            Prec = ifelse(!is.na(ligne_datavar[[4]]), as.numeric(ligne_datavar[[4]]), 0),
                            chif_pval = ifelse(!is.na(ligne_datavar[[11]]), as.numeric(ligne_datavar[[11]]), 2),
                            mode = ifelse(!is.na(ligne_datavar[[9]]), as.character(ligne_datavar[[9]]), "mediqrg"),
                            test = ifelse(!is.na(ligne_datavar[[10]]), as.character(ligne_datavar[[10]]), "none"),
                            Langue = langue,
                            nomcol = nomcol,
                            nomvariable = if (!is.na(ligne_datavar[[7]])) {as.character(ligne_datavar[[7]])} else {NULL},
                            simplif = FALSE)

    } else if (ligne_datavar[[2]] == "binary") {

      tableau <- tab_binaire(data = data,
                             x = !!x,
                             y = !!y,
                             prec = ifelse(!is.na(ligne_datavar[[4]]), as.numeric(ligne_datavar[[4]]), 0),
                             coefbin = ifelse(!is.na(ligne_datavar[[3]]), as.logical(ligne_datavar[[3]]), FALSE),
                             chif_pval = ifelse(!is.na(ligne_datavar[[11]]), as.numeric(ligne_datavar[[11]]), 2),
                             test = ifelse(!is.na(ligne_datavar[[10]]), as.character(ligne_datavar[[10]]), "none"),
                             langue = langue,
                             nomcol = nomcol,
                             nomcateg = if (!is.na(ligne_datavar[[5]])) {as.character(ligne_datavar[[5]])} else {NULL},
                             label = if (!is.na(ligne_datavar[[6]])) {as.character(ligne_datavar[[6]])} else {NULL},
                             simplif = FALSE)

    } else if (ligne_datavar[[2]] == "quali") {

      tableau <- tab_quali(data = data,
                           x = !!x,
                           y = !!y,
                           prec = ifelse(!is.na(ligne_datavar[[4]]), as.numeric(ligne_datavar[[4]]), 0),
                           coefbin = ifelse(!is.na(ligne_datavar[[3]]), as.logical(ligne_datavar[[3]]), FALSE),
                           test = ifelse(!is.na(ligne_datavar[[10]]), as.character(ligne_datavar[[10]]), "none"),
                           chif_pval = ifelse(!is.na(ligne_datavar[[11]]), as.numeric(ligne_datavar[[11]]), 2),
                           langue = langue,
                           nomcol = nomcol,
                           ordonnee = ifelse(!is.na(ligne_datavar[[8]]), as.logical(ligne_datavar[[8]]), FALSE),
                           nomvariable = if (!is.na(ligne_datavar[[7]])) {as.character(ligne_datavar[[7]])} else {NULL},
                           simplif = FALSE)

    }

  }

  return(tableau)

}


#' descr
#'
#' Description of a data.frame
#'
#' A function using the 3 other functions (\code{tab_quanti}, \code{tab_quali} and \code{tab_binaire}) to describe the variables of a data.frame.
#'
#' You can also use it to cross it with a categorial variable and perform a comparison test (chisq or fisher test). For now no tests are performed with crossing with a variable that has more than 2 classes.
#'
#' The function is now updated as function \code{Description} and one should use the later instead.
#'
#' @param data The dataset.
#' @param y The crossing variable. NULL if you only want univariate description.
#' @param datavar The datavar.
#' @param listevar A vector of the name of the columns you want to describe. By default, it is the whole set of variables present in the datavar.
#' @param nomcol The vector of the names you want to give to the columns of the output. NULL for automatic naming.
#' @param langue "fr" for French and "eng" for English.
#' @param comparer TRUE won't alterate the datavar, but FALSE will ensure that no test will be performed even if supplied in the datavar. In case of crossed description.
#'
#' @export
#'
#' @seealso \code{\link{tab_quali}}, \code{\link{tab_quanti}}, \code{\link{tab_binaire}}, \code{\link{Description}}
#'
#' @examples
#' # The description of the whole mtcars dataset
#' descr(mtcars, datavar = datavarr, langue = "eng")
#' # Only some variables
#' descr(mtcars, datavar = datavarr, listevar = c("mpg", "am", "cyl"), langue = "eng")
#' # Crossed (messages for Wilcoxon's test because of ex aequos)
#' descr(mtcars, datavar = datavarr, y = am, langue = "eng", comparer = FALSE)
#' descr(mtcars, datavar = datavarr, y = am, langue = "eng")
descr <- function(data,
                  y = NULL,
                  datavar,
                  listevar = datavar[[1]],
                  nomcol = NULL,
                  langue = "fr",
                  comparer = TRUE) {

  # Vérification de l'argument langue
  langue <- verif_langue(langue)

  # Convertir en data frame pour éviter les problèmes avec les tibbles
  if (any(class(data) != "data.frame")) data <- as.data.frame(data)
  if (any(class(datavar) != "data.frame")) datavar <- as.data.frame(datavar)

  if (!comparer) datavar$test <- "none"

  crois <- tryCatch(
    expr = {y <- rlang::expr_text(rlang::ensym(y))},
    error = function(e) NULL
  )

  # Retrait de la variable à croiser dans le cas d'une description croisée
  if (!is.null(crois)) listevar <- listevar[listevar != crois]

  # datavar <- verif_datavar(datavar)
  if (any(datavar$type[datavar$var %in% listevar] %nin% c("quanti", "quali", "binary")))
    stop("Second column of datavar should only be comprised of \"quanti\", \"quali\" and \"binary\".")

  tableau <- purrr::map_dfr(
    .x = seq_len(length(listevar)),
    .f = ~ descr_var(data = data,
                     ligne_datavar = datavar[datavar$var == listevar[.x], ],
                     y = !!crois,
                     nomcol = nomcol,
                     langue = langue)
  )

  # Retrait de la colonne Pvalue s'il n'y a aucun test statistique effectué
  if (!is.null(crois) && all(tableau$Pvalue == "")) tableau <- tableau[, -match("Pvalue", names(tableau))]

  # Mise en haut de la ligne des totaux et retrait de toute les autres ligne "Total"
  tableau <- tableau[tableau$Variable != "Total", ]
  if (is.null(crois)) {
    tableau <- rbind(c("Total", length(data[, 1]), ""), tableau)
  } else {
    tableau <- rbind("", tableau)
    tableau$Variable[1] <- "Total"
    tableau[1, seq(2, ncol(tableau) - 1, 2)] <- tapply(data[, 1], data[, y], length)
  }

  # Customisation de la note de bas de tableau
  # D'abord pour comment les résultats sont décrits
  foot <- character(1)
  datavar <- datavar[datavar$var %in% listevar, ]
  if (any(datavar[["coefbin"]][datavar$type %in% c("binary", "quali")])) {
    foot <- paste0("n(%[", ifelse(langue == 'fr', 'IC95%', 'CI95%'), "]), ")
  } else {
    foot <- paste0("n(%), ")
  }
  if (any(grepl("med", datavar[["mode"]][datavar$type == "quanti"]))) {
    foot <- ifelse(langue == "fr",
                   paste0(foot, "med", ifelse(any(grepl("moy", datavar[['mode']][datavar$type == 'quanti'])), "/moy", "")),
                   paste0(foot, "med", ifelse(any(grepl("moy", datavar[['mode']][datavar$type == 'quanti'])), "/mean", "")))
  } else if (any(grepl("moy", datavar[['mode']][datavar$type == 'quanti']))) {
    foot <- ifelse(langue == "fr",
                   paste0(foot, "moy"),
                   paste0(foot, "mean"))
  }
  if (any(grepl("sd", datavar[["mode"]][datavar$type == "quanti"]))) {
    foot <- ifelse(langue == "fr",
                   paste0(foot, "{ds}"),
                   paste0(foot, "{sd}"))
  }
  if (any(grepl("iq", datavar[["mode"]][datavar$type == "quanti"]))) {
    foot <- paste0(foot, ",(Q1;Q3)")
  }
  if (any(grepl("rg", datavar[["mode"]][datavar$type == "quanti"]))) {
    foot <- paste0(foot, ",(min;max)")
  }
  foot <- gsub(", ([med|moy])", replacement = "\\\n\\1", x = foot)

  attr(tableau, "pied") <- foot
  attr(tableau, "class") <- c("data.frame", "m_df")

  return(tableau)

}



#' create_datavar
#'
#' Creation of a default datavar
#'
#' @param data The dataset for which we want a datavar.
#' @param colonnes The columns of the dataset that you want to describe. By default, all the columns of the dataset are to be described.
#' @param default_datavar The default options for the creation. Made by the function \code{options_datavar}.
#'
#' @return A data.frame of 11 variables:
#' \itemize{
#'   \item var: the name of each variable. All described variables should be in the datavar;
#'   \item type: type of variable (quanti for a quantitative variable, quali for a categorial variable with more than 2 items and binary for a binary variable);
#'   \item coefbin: for categorical and binary variables, TRUE if you want to display the confidence interval of the proportion computed with binomial distribution;
#'   \item prec: number of decimals;
#'   \item nomcateg: for binary variables, the name of the class in the data to be displayed;
#'   \item label: for binary variables, the name displayed in the description table;
#'   \item nomvariable: for categorical and quantitative variables, the name of the variable displayed in the description table;
#'   \item ordonnee: for categorical variables, TRUE if you want to let the classes in order and FALSE if you want them by descending count;
#'   \item mode: for quantitative variables, string indicating which statistics to be displayed (moy for mean, med for median, sd for standard error, iq for interquartile interval and rg for range);
#'   \item test: string indicating the statistical test to perform (none for none, student for Student's test, wilcoxon for Wilcoxon's signed rank test, chisq for Chi-2 test and fisher for Fisher's test);
#'   \item chif_pval: number of decimals for Pvalues.
#' }
#'
#' @export
#'
#' @seealso \code{\link{descr}}
#'
#' @examples
#' create_datavar(mtcars)
create_datavar <- function(data,
                           colonnes = colnames(data),
                           default_datavar = options_datavar()) {

  if (!inherits(default_datavar, "default_options_datavar")) stop("Specify default options in default_datavar with function \"options_datavar\".")

  datavar <- data.frame(var         = colonnes,
                        type        = NA_character_,
                        coefbin     = NA,
                        prec        = NA_integer_,
                        nomcateg    = NA_character_,
                        label       = NA_character_,
                        nomvariable = NA_character_,
                        ordonnee    = NA,
                        mode        = NA_character_,
                        test        = NA_character_,
                        chif_pval   = NA_integer_)
  datavar$type <- vapply(datavar$var, detection_type, character(1), data = data, limite_detection = default_datavar$limite_detection)
  datavar$coefbin <- ifelse(datavar$type == "quanti", NA, default_datavar$coefbin)
  datavar$prec <- ifelse(datavar$type == "quanti", default_datavar$prec,
                         ifelse(sum(!is.na(data[, datavar$var])) < 1000, 0L,
                                ifelse(sum(!is.na(data[, datavar$var])) < 10000, 1L, 2L)))
  datavar$ordonnee <- ifelse(datavar$type == "quali", default_datavar$ordonnee, NA)
  datavar$mode <- ifelse(datavar$type == "quanti", default_datavar$mode, NA_character_)
  datavar$test <- ifelse(datavar$type == "quanti", default_datavar$test["quanti"],
                         ifelse(datavar$type == "quali", default_datavar$test["quali"], default_datavar$test["binary"]))
  datavar$chif_pval[datavar$test != "none"] <- default_datavar$chif_pval

  return(datavar)

}


#' detection_type
#'
#' @param data Dataset.
#' @param variable Name of the variable.
#' @param limite_detection Limit of number of items in the variable to be considered quantitative or qualitative.
#'
detection_type <- function(data, variable, limite_detection = 10L) {
  var <- data[, variable]
  if (length(unique(var[!is.na(var)])) <= 2) {
    type <- "binary"
  } else if (!is.numeric(var) | is.factor(var)) {
    type <- "quali"
  } else if (length(unique(var[!is.na(var)])) <= limite_detection) {
    type <- "quali"
  } else {
    type <- "quanti"
  }
  return(type)
}


#' options_datavar
#'
#' Default options for the function \code{create_datavar}
#'
#' @param limite_detection Limit detection for the number of different items in a numeric variable to be considered quantitative.
#' @param coefbin Boolean for the display of the confidence interval of percentages.
#' @param prec Number of decimals for quantitative variables.
#' @param ordonnee TRUE/FALSE for argument ordonnee of function \code{tab_quali}.
#' @param mode mode argument of function \code{tab_quanti}.
#' @param test Statistical test to be performed.
#' @param chif_pval Number of decimals of Pvalues.
#'
#' @return A list of default options.
#'
#' @export
options_datavar <- function(limite_detection = 10L,
                            coefbin = FALSE,
                            prec = 2L,
                            ordonnee = TRUE,
                            mode = "mediqrg",
                            test = "none",
                            chif_pval = 2) {

  if (!is.logical(coefbin)) stop("\"coefbin\" should be a logical.", call. = FALSE)
  if (!is.logical(ordonnee)) stop("\"ordonnee\" should be a logical.", call. = FALSE)
  if (!is.numeric(prec) || prec %% 1 != 0 || prec < 0) stop("\"prec\" should be a positive or nul integer.", call. = FALSE)
  if (!is.numeric(chif_pval) || chif_pval %% 1 != 0 || chif_pval < 0) stop("\"chif_pval\" should be a positive or nul integer.", call. = FALSE)
  if (!is.numeric(limite_detection) || limite_detection %% 1 != 0 || limite_detection <= 0) stop("\"limite_detection\" should be a positive integer.", call. = FALSE)
  if (!is.character(mode) || length(mode) != 1) stop("\"mode\" should be a character string of length 1.")
  if (!grepl("med|moy|sd|iq|rg", mode)) stop("The \"mode\" provided won't produce any output.", call. = FALSE)

  if (length(test) == 1 && test == "none") {
    test <- c(quanti = "none", quali = "none", binary = "none")
  } else {
    if (!is.character(test) || length(test) != 3) stop("You should supply \"none\" or a named character vector of length 3 in test.", call. = FALSE)
    if (is.null(names(test))) {names(test) <- c("quanti", "quali", "binary");warning("test isn't a named vector. Names are set to be \"quanti\", \"quali\" and \"binary\" in that order", call. = FALSE, immediate. = TRUE)}
    if (any(names(test) %nin% c("quanti", "quali", "binary")) | length(unique(names(test))) != 3) stop("Names of test have to be \"quanti\", \"quali\" and \"binary\".", call. = FALSE)
  }

  opt <- structure(
    list(limite_detection = limite_detection,
         coefbin = coefbin,
         prec = prec,
         ordonnee = ordonnee,
         mode = mode,
         test = test,
         chif_pval = chif_pval),
    class = "default_options_datavar"
  )

  invisible(opt)

}




#' tab_binaire
#'
#' Description of binary variables
#'
#' A function to describe binary variables with count and percentage (+ binomial confidence interval if supplied).
#'
#' You can also use it to cross it with a categorial variable and perform a comparison test (chisq or fisher test).
#'
#' The function has been updated to \code{TabBinaire}.
#'
#' @param data The dataset that contains the variables.
#' @param x The binary variable to describe.
#' @param y The qualitative variable to perform the bivariate description. If unspecified, univariate description.
#' @param Prec Number of decimals for the percentages.
#' @param coefbin If TRUE displays the binomial confidence intervals.
#' @param chif_pval Number of decimals for the Pvalue if test is performed.
#' @param test String giving the name of the comparison test performed.
#' @param Langue "fr" for french and "eng" for english. For the display in the table.
#' @param nomcol Vector of strings to name each column of the output. Automatic display if unspecified.
#' @param nomcateg Sting giving the name of the class that you want to display in the table.
#' @param label The label you want to display for nomcateg.
#' @param simplif Not usefull, if TRUE will delete unused column 'Pvalue'.
#'
#' @export
#'
#' @seealso [descr()], [TabBinaire()]
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
                        Prec = 0,
                        coefbin = FALSE,
                        chif_pval = 2,
                        test = "none",
                        Langue = "eng",
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
  Langue <- VerifArgs(Langue)
  if (sum(!is.na(varbinaire)) == 0)
    stop(paste0("Variable ", rlang::as_label(x), " has 0 non missing values."), call. = FALSE)
  Prec <- VerifArgs(Prec)
  if (length(unique(varbinaire[!is.na(varbinaire)])) > 2)
    stop(paste0("The variable to describe \"", rlang::quo_name(x), "\" isn't a binary variable."), call. = FALSE)
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

  fprec <- paste0("%.", Prec, "f")

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
    nomtable <- data.frame(var = c("Total", paste0("** ", label), ifelse(Langue == "fr", "** Manquants", ifelse(Langue == "eng", "** Missings", "** ..."))),
                           eff = c(length(varbinaire), paste0("n=", sum(!is.na(varbinaire))), ""),
                           stats = c("", statist, sum(is.na(varbinaire))),
                           stringsAsFactors = FALSE)

    # Noms des colonnes
    if (is.null(nomcol)) {
      if (Langue == "fr") {colnames(nomtable) <- c("Variable", "Effectif", "Statistiques")}
      else if (Langue == "eng") {colnames(nomtable) <- c("Variable", "Count", "Statistics")}
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
      if (Langue == "fr") {
        colnames(nomtable) <- c("Variable",
                                paste0(rep(c("Effectif", "Statistiques"), length.out = ncol(nomtable) - 2),
                                       " (", rlang::quo_name(y), ":", rep(names(table(varcroise[!is.na(varcroise)])), each = 2), ")"),
                                "Pvalue")
      } else if (Langue == "eng") {
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
    nomtable[3, 1] <- ifelse(Langue == "fr", "** Manquants", ifelse(Langue == "eng", "** Missings", "** ..."))
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
#' @param data The dataset that contains the variables.
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
tab_quali <- function(data,
                      x,
                      y = NULL,
                      prec = 0,
                      coefbin = FALSE,
                      test = "none",
                      chif_pval = 2,
                      langue = "eng",
                      nomcol = NULL,
                      ordonnee = FALSE,
                      nomvariable = NULL,
                      simplif = TRUE) {

  x <- rlang::sym(rlang::enexpr(x))
  x <- rlang::enquo(x)
  varquali <- rlang::eval_tidy(x, data = data)

  crois <- tryCatch(
    expr = {y <- rlang::sym(rlang::enexpr(y));rlang::eval_tidy(rlang::enquo(y), data = data)},
    error = function(e) NULL
  )

  # Vérifications avant de faire tourner la fonction
  langue <- verif_langue(langue)
  if (sum(!is.na(varquali)) == 0) stop(paste0("Variable ", rlang::quo_name(x), " has 0 non missing values."), call. = FALSE)
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
    varcroise <- rlang::eval_tidy(y, data = data)

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
