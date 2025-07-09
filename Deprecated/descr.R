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
                             y = !!y,
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

