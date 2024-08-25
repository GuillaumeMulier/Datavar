#' CreateDatavar
#'
#' Creation of a default datavar for a dataset given the default options provided by function \code{DefaultOptions}.
#'
#' @param .Data The dataset for which we want a datavar.
#' @param colonnes The columns of the dataset that you want to describe. By default, all the columns of the dataset are to be described.
#' @param DefaultOptions The default options for the creation. Made by the function \code{options_datavar}.
#'
#' @return A data.frame of 12 variables:
#' \itemize{
#'   \item var: the name of each variable. All described variables should be in the datavar;
#'   \item col_name: the original name of the column in the dataframe;
#'   \item type: type of variable (quanti for a quantitative variable, quali for a categorial variable with more than 2 items and binary for a binary variable);
#'   \item conf_inter: string for categorical variables indicating which type of confidence interval should be computed for percentage;
#'   \item conf_level: numeric between 0 and 1 for categorical variables indicating the level for confidence intervals for percentages;
#'   \item prec: number of decimals;
#'   \item nomcateg: for binary variables, the name of the class in the data to be displayed;
#'   \item label: for binary variables, the name displayed in the description table;
#'   \item nomvariable: for categorical and quantitative variables, the name of the variable displayed in the description table;
#'   \item ordonnee: for categorical variables, TRUE if you want to let the classes in order and FALSE if you want them by descending count;
#'   \item mode: for quantitative variables, string indicating which statistics to be displayed (moy for mean, med for median, sd for standard error, iq for interquartile interval and rg for range);
#'   \item test: string indicating the statistical test to perform (none for none, student for Student's test, wilcoxon for Wilcoxon's signed rank test, chisq for Chi-2 test and fisher for Fisher's test);
#'   \item mu0: string indicating the theoretical mean to compare to for quantitative variables and the proportion of each class separated by ';' for binary and categorical variables;
#'   \item chif_pval: number of decimals for Pvalues.
#' }
#'
#' @export
#'
#' @seealso [DefaultOptions()]
#'
#' @examples
#' CreateDatavar(mtcars)
CreateDatavar <- function(.Data,
                          colonnes = colnames(.Data),
                          Options = DefaultOptions()) {

  if (!inherits(Options, "default_options_datavar"))
    stop("Specify default options in \"", PrintArg("Options"), "\" with function \"", cli::style_italic("DefaultOptions"), "\".")

  Datavar <- data.frame(var         = colonnes,
                        col_name    = colonnes,
                        type        = NA_character_,
                        conf_inter  = NA_character_,
                        conf_level  = NA_real_,
                        prec        = NA_integer_,
                        nomcateg    = NA_character_,
                        label       = NA_character_,
                        nomvariable = NA_character_,
                        ordonnee    = NA,
                        mode        = NA_character_,
                        test        = NA_character_,
                        mu0         = NA_character_,
                        chif_pval   = NA_integer_)

  Datavar$type <- vapply(Datavar$var, DetectionType, character(1), .Data = .Data, LimiteDetection = Options$LimiteDetection)
  Datavar$conf_inter <- ifelse(Datavar$type == "quanti", NA_character_, Options$ConfInter)
  Datavar$conf_level <- ifelse(Datavar$type == "quanti", NA_real_, Options$ConfLevel)
  Datavar$prec <- ifelse(Datavar$type == "quanti",
                         ifelse(rep(is.null(Options$Prec), length(colonnes)),
                                purrr::map_dbl(Datavar$var, ~ ifelse(grepl(pattern = "\\.", x = as.character(.Data[[.x]])),
                                                                     nchar(gsub("^\\d*\\.(\\d*)$", "\\1", as.character(.Data[[.x]]))),
                                                                     0) |> max()),
                                Options$Prec),
                         ifelse(purrr::map_dbl(Datavar$var, ~ sum(!is.na(.Data[[.x]]))) < 1000, 0,
                                ifelse(purrr::map_dbl(Datavar$var, ~ sum(!is.na(.Data[[.x]]))) < 10000, 1, 2)))
  Datavar$ordonnee <- ifelse(Datavar$type == "quali", Options$Ordonnee, NA)
  Datavar$mode <- ifelse(Datavar$type == "quanti", Options$Mode, NA_character_)
  Datavar$test <- ifelse(Datavar$type == "quanti", Options$Test["quanti"],
                         ifelse(Datavar$type == "quali", Options$Test["quali"], Options$Test["binary"]))
  Datavar$mu0[Datavar$type == "quanti"] <- vapply(Datavar$var[Datavar$type == "quanti"], \(x) as.character(round(mean(.Data[[x]], na.rm = TRUE), Datavar$prec[Datavar$var == x])), character(1))
  Datavar$chif_pval <- Options$ChifPval

  return(Datavar)

}


#' DetectionType
#'
#' @param .Data Dataset.
#' @param .Var Name of the variable.
#' @param LimiteDetection Limit of number of items in the variable to be considered quantitative or qualitative.
#'
DetectionType <- function(.Data, .Var, LimiteDetection = 10L) {
  Variable <- .Data[[.Var]]
  if (length(unique(Variable[!is.na(Variable)])) <= 2) {
    Type <- "binary"
  } else if (!is.numeric(Variable) | is.factor(Variable)) {
    Type <- "quali"
  } else if (length(unique(Variable[!is.na(Variable)])) <= LimiteDetection) {
    Type <- "quali"
  } else {
    Type <- "quanti"
  }
  return(Type)
}


#' DefaultOptions
#'
#' Default options for the function \code{CreateDatavar}.
#'
#' @param LimiteDetection Limit detection for the number of different items in a numeric variable to be considered quantitative.
#' @param ConfInter String for categorical variables indicating which type of confidence interval should be computed for percentage.
#' @param ConfLevel Numeric between 0 and 1 for categorical variables indicating the level for confidence intervals for percentages.
#' @param Prec Number of decimals for quantitative variables.
#' @param Ordonnee TRUE/FALSE for argument ordonnee of function \code{tab_quali}.
#' @param Mode mode argument of function \code{tab_quanti}.
#' @param Test Statistical test to be performed.
#' @param ChifPval Number of decimals of Pvalues.
#'
#' @return A list of default options.
#'
#' @seealso [CreateDatavar()]
#'
#' @export
#'
#' @examples
#' print(DefaultOptions(mtcars))
DefaultOptions <- function(LimiteDetection = 10L,
                           ConfInter = "none",
                           ConfLevel = .95,
                           Prec = NULL,
                           Ordonnee = TRUE,
                           Mode = "mediqrg",
                           Test = "none",
                           ChifPval = 2) {

  if (!is.numeric(ConfLevel) || ConfLevel <= 0 || ConfLevel >= 1)
    stop(paste0("\"", PrintArg("ConfLevel"), "\" should be a real between 0 and 1."), call. = FALSE)
  ConfInter <- match.arg(ConfInter, c("none", "normal", "exact", "jeffreys"))
  if (!is.logical(Ordonnee))
    stop(paste0("\"", PrintArg("Ordonnee"), "\" should be a boolean."), call. = FALSE)
  if (!is.null(Prec) & (!is.numeric(Prec) || Prec %% 1 != 0 || Prec < 0))
    stop(paste0("\"", PrintArg("prec"), "\" should be a positive or null integer."), call. = FALSE)
  if (!is.numeric(ChifPval) || ChifPval %% 1 != 0 || ChifPval < 0)
    stop(paste0("\"", PrintArg("ChifPval"), "\" should be a positive or null integer."), call. = FALSE)
  if (!is.numeric(LimiteDetection) || LimiteDetection %% 1 != 0 || LimiteDetection <= 0)
    stop(paste0("\"", PrintArg("LimiteDetection"), "\" should be a positive integer."), call. = FALSE)
  if (!is.character(Mode) || length(Mode) != 1)
    stop(paste0("\"", PrintArg("Mode"), "\" should be a character string of length 1."), call. = FALSE)
  if (!grepl("med|moy|sd|iq|rg", Mode))
    stop(paste0("The \"", PrintArg("Mode"), "\" provided won't produce any output."), call. = FALSE)

  if (length(Test) == 1 && Test == "none") {
    Test <- c(quanti = "none", quali = "none", binary = "none")
  } else {
    if (!is.character(Test) || length(Test) != 3)
      stop(paste0("You should supply \"none\" or a named character vector of length 3 in \"", PrintArg("Test"), "\"."), call. = FALSE)
    if (is.null(names(Test))) {
      names(Test) <- c("quanti", "quali", "binary")
      message(Information(paste0("\"", PrintArg("Test"), "\" isn't a named vector. Names are set to be \"quanti\", \"quali\" and \"binary\" in that order.")))
    }
    if (any(names(Test) %nin% c("quanti", "quali", "binary")) | length(unique(names(Test))) != 3)
      stop(paste0("Names of \"", PrintArg("Test"), "\" have to be \"quanti\", \"quali\" and \"binary\"."), call. = FALSE)
  }

  opt <- structure(
    list(LimiteDetection = LimiteDetection,
         ConfInter = ConfInter,
         ConfLevel = ConfLevel,
         Prec = Prec,
         Ordonnee = Ordonnee,
         Mode = Mode,
         Test = Test,
         ChifPval = ChifPval),
    class = "default_options_datavar"
  )

  invisible(opt)

}
