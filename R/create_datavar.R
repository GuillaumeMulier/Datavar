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
#' @example
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
