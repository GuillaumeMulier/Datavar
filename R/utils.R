#' Inverse of %in%
'%nin%' <- function(x, y) {match(x, y, nomatch = 0) <= 0}


#' Check the statistical test
#'
#' @param x String naming the test.
#' @param type_var Type of variable.
#'
verif_test <- function(x, type_var) {

  type_var <- match.arg(type_var, choices = c("binaire", "quanti", "quali"))
  x <- match.arg(x, c("student", "wilcoxon", "fisher", "chisq", "none"))

  if (type_var == "quanti") {
    if (x %nin% c("student", "wilcoxon", "none")) stop(paste0("Test unadapted to a quantitative variable: ", x), call. = FALSE)
  } else if (type_var %in% c("binaire")) {
    if (x %nin% c("fisher", "chisq", "none")) stop(paste0("Test unadapted to a binary variable: ", x), call. = FALSE)
  } else if (type_var %in% c("quali")) {
    if (x %nin% c("chisq", "none")) stop(paste0("Test unadapted to a categorial variable: ", x), call. = FALSE)
  }

  return (x)
}


#' Check the language of display.
#'
#' @param x Language chosen.
#'
verif_langue <- function(x) {

  if (x %nin% c("fr", "eng")) stop(paste0("Unrecognized language: ", x, "\nChoose betwwen: 'fr', 'eng'."), call. = FALSE)

  return(x)
}


#' Check the statistical presentation
#'
#' @param x String to indicate the presentation
#'
verif_mode <- function(x) {

  if (!grepl("med|moy|iqr|sd|rg", x)) stop("Unrecognized statistical presentation. Please type at least 1 of the following strings: \"med\", \"moy\", \"sd\", \"iqr\", \"rg\".", call. = FALSE)

  return(x)
}


#' Check the datavar
#'
#' @param x The datavar
#'
verif_datavar <- function(x) {

  if (!any(class(x) == "data.frame")) stop("The datavar should be a data.frame.", call. = FALSE)
  if (ncol(x) != 11) stop("The datavar should have 11 columns.", call. = FALSE)
  if (!all(colnames(datavarr) == c("var", "type", "coefbin", "prec", "nomcateg", "label", "nomvariable", "ordonnee", "mode", "test", "chif_pval")))
    stop("The columns of the datavar should be the following: \"var\", \"type\", \"coefbin\", \"prec\", \"nomcateg\", \"label\", \"nomvariable\", \"ordonnee\", \"mode\", \"test\" and \"chif_pval\".", call. = FALSE)
  if (!is.character(x$var) || !is.character(x$type) || !is.character(x$mode) || !is.character(x$test) || !is.character(x$label) || !is.character(x$nomvariable))
    stop("Columns \"var\", \"type\", \"mode\", \"test\", \"label\" and \"nomvariable\" should contain characters.", call. = FALSE)
  if (!is.logical(x$coefbin) || !is.logical(x$ordonnee)) stop("Columns \"coefbin\" and \"ordonnee\" should be booleans.", call. = FALSE)
  if (!is.numeric(prec) || !is.numeric(chif_pval)) stop("Columns \"prec\" and \"chif_pval\" should be numerics.", call. = FALSE)

  return(x)

}

#' Round the Pvalues
#'
#' @param x Pvalue.
#' @param nb_chiffre Number of decimals wanted.
#'
arrondi_pv <- function(x, nb_chiffre) {

  if (length(nb_chiffre) != 1 || !is.numeric(nb_chiffre) || nb_chiffre %% 1 != 0) stop("\"nb_chiffre\" should be a whole number.", call. = FALSE)

  prec <- paste0("%.", nb_chiffre, "f")
  result <- sprintf(fmt = prec, x)
  if (!grepl("[1-9]", result)) result <- paste0("<", gsub("^(.*)0$", "\\11", result))

  return(result)
}


#' Get the 1st quartile
Q1 <- function(x, na.rm = TRUE) {
  as.numeric(quantile(x, probs = 0.25, na.rm = na.rm))
}


#' Get the 3rd quartile
Q3 <- function(x, na.rm = TRUE) {
  as.numeric(quantile(x, probs = 0.75, na.rm = na.rm))
}
