#' TabQuanti
#'
#' Description of quantitative variables
#'
#' A function to describe quantitative variables with mean, median, standard deviation, interquartile and range.
#'
#' You can also use it to cross it with a categorial variable and perform a comparison test (student or wilcoxon test). For now no tests are performed with crossing with a variable that has more than 2 classes.
#'
#' @param .Data The dataset that contains the variables.
#' @param x The quantitative variable to describe.
#' @param y The qualitative variable to perform the bivariate description. If unspecified, univariate description.
#' @param Prec Number of decimals for the statistics.
#' @param PMissing Number of decimals of percentage from whole dataset. NULL (default value) if no percent desired.
#' @param NomVariable String giving the name you want to display in the table. Automatic display if unspecified.
#' @param Mode String to indicate what statistics to display. If unspecified, by default give median, interquartile and range ("med" for
#' median, "moy" for mean, "sd" for standard deviation, "iq" for interquartile and "rg" fo range).
#' @param Test String giving the test to compare the statistic between the 2 groups if needed ("none", "ztest", "student", "wilcoxon").
#' @param ChifPval Number of decimals for the Pvalue if needed.
#' @param NomCol Vector of strings to name each column of the output. Automatic display if unspecified.
#' @param Langue "fr" for french and "eng" for english. For the display in the table.
#' @param Grapher Boolean if you want to graph the distribution in univariate case.
#' @param Simplif Not usefull, if TRUE will delete unused column 'Pvalue'.
#'
#' @export
#'
#' @seealso \code{\link{Description}}
#'
#' @examples
#' TabQuanti(.Data = mtcars, x = mpg, Prec = 1)
#' TabQuanti(.Data = mtcars, x = mpg, y = am, Prec = 1)
#' TabQuanti(.Data = mtcars, x = mpg, y = am, Prec = 1,
#'           NomVariable = "Miles per gallon",
#'           Test = "stud", ChifPval = 3)
TabQuanti <- function(.Data,
                      x,
                      y = NULL,
                      Prec = 0,
                      PMissing = NULL,
                      ChifPval = 2,
                      Mode = "mediqrg",
                      Test = "none",
                      Langue = "eng",
                      NomCol = NULL,
                      NomVariable = NULL,
                      Grapher = FALSE,
                      Simplif = TRUE) {

  # Interest variables defused so that it is possible to give unquoted arguments
  x <- rlang::enexpr(x)
  VarQuanti <- rlang::eval_tidy(x, data = .Data)
  y <- rlang::enexpr(y)

  # Verifications
  stopifnot(is.logical(Grapher), length(Grapher) == 1)
  Langue <- VerifArgs(Langue)
  Prec <- VerifArgs(Prec)
  NomVariable <- VerifArgs(NomVariable, x)
  PMissing <- VerifArgs(PMissing)
  Mode <- VerifArgs(Mode, x, Langue, Prec, PMissing)
  VarQuanti <- VerifArgs(VarQuanti, x)

  if (is.null(y)) { # Univariate description

    # Store statistics and labels
    Statistics <- purrr::map_chr(
      Mode,
      function(tab) {
        Res <- purrr::map_chr(seq_len(nrow(tab)), ~ tab$fct[[.x]](VarQuanti, tab$precision[.x]))
        return(paste(Res, collapse = ", "))
      }
    )
    Labelliseurs <- purrr::map_chr(Mode, \(tab) return(paste(tab$label, collapse = ", ")))

    # Table of results
    Tableau <- data.frame(var = NomVariable,
                          eff = Labelliseurs,
                          stats = Statistics,
                          stringsAsFactors = FALSE)
    if (Grapher) Tableau$graphes <- c(list(GGHist(VarQuanti)), rep("", nrow(Tableau) - 1))
    attr(Tableau, "crossed") <- "univariate"

    # Name of columns
    if (is.null(NomCol)) {
      if (Langue == "fr") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistiques", "Graphes") else c("Variable", "Label", "Statistiques")}
      else if (Langue == "eng") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistics", "Graphs") else c("Variable", "Label", "Statistics")}
    } else {
      if (length(NomCol) != ncol(Tableau)) stop(paste0("\"NomCol\" argument isn't of length", ncol(Tableau), "."), call. = FALSE)
      colnames(Tableau) <- NomCol
    }

    # Simplify table if no missing values
    if (Simplif && sum(is.na(VarQuanti)) == 0) {
      Tableau[1, 2] <- "N"
      Tableau[1, 3] <- gsub("^(\\d+), .*$", "\\1", Tableau[1, 3])
    }

  } else { # Multivariate description

    VarCroise <- rlang::eval_tidy(y, data = .Data)
    VarQuanti <- VarQuanti[!is.na(VarCroise)]
    VarCroise <- VarCroise[!is.na(VarCroise)]
    NClasses <- length(unique(VarCroise))

    # Verifications on statistical test
    Test <- VerifTest(Test, "quanti", NClasses, VarQuanti, y, x)
    ChifPval <- VerifArgs(ChifPval)

    # Store statistics and labels
    Statistics <- tapply(VarQuanti, VarCroise,
                         \(VarQuanti) {
                           purrr::map_chr(
                             Mode,
                             function(tab) {
                               Res <- purrr::map_chr(seq_len(nrow(tab)), ~ tab$fct[[.x]](VarQuanti, tab$precision[.x]))
                               return(paste(Res, collapse = ", "))
                             }
                           )
                         })
    Labelliseurs <- purrr::map_chr(Mode, \(tab) return(paste(tab$label, collapse = ", ")))
    if (Test != "none") Pval <- c(MakeTest(VarQuanti, VarCroise, Test, rlang::quo_name(x), rlang::quo_name(y), ChifPval), rep("", length(Labelliseurs) - 1))

    # Table of results
    Tableau <- data.frame(var = NomVariable,
                          eff = Labelliseurs,
                          stringsAsFactors = FALSE)
    Tableau <- suppressMessages(cbind(Tableau, dplyr::bind_cols(Statistics)))
    if (Test != "none") Tableau$pval <- Pval
    if (Grapher) message(Information("Graphs aren't supported in multivariate description."))
    attr(Tableau, "crossed") <- "multivariate"

    # Names of columns
    if (is.null(NomCol)) {
      if (Langue == "fr") {
        colnames(Tableau) <- c("Variable", "Label",
                               paste0(rep("Statistiques (", NClasses), rlang::quo_name(y), "=", unique(VarCroise), ")"),
                               if (Test == "none") NULL else "PValue")
      } else {
        colnames(Tableau) <- c("Variable", "Label",
                               paste0(rep("Statistics (", NClasses), rlang::quo_name(y), "=", unique(VarCroise), ")"),
                               if (Test == "none") NULL else "PValue")
      }
    } else {
      if (length(NomCol) != ncol(Tableau)) stop(paste0("\"NomCol\" argument isn't of length", ncol(Tableau), " for variable \"", rlang::quo_name(x), "\"."), call. = FALSE)
      colnames(Tableau) <- NomCol
    }

    # Simplify table if no missing values
    if (Simplif && sum(is.na(VarQuanti)) == 0) {
      Tableau[1, 2] <- "N"
      Tableau[1, 3:4] <- gsub("^(\\d+), .*$", "\\1", Tableau[1, 3:4])
    }

  }

  class(Tableau) <- c("tab_datavar", class(Tableau))
  if (Grapher) attr(Tableau, "Grapher") <- TRUE
  return(Tableau)

}
