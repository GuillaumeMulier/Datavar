#' TabQuali
#'
#' Description of categorical variables
#'
#' A function to describe categorical variables with count and percentage (+ binomial confidence interval if supplied).
#' The difference with \code{TabBinaire} is that all categories are displayed.
#'
#' You can also use it to cross it with a categorial variable and perform a comparison test (chisq test, fisher test).
#'
#' @param .Data The dataset that contains the variables.
#' @param x The categorical variable to describe. Should be the name without "".
#' @param y The qualitative variable to perform the bivariate description. Should be the name without "". If unspecified, univariate description.
#' @param Prec Number of decimals for the percentages.
#' @param PMissing Number of decimals of percentage from whole dataset. NULL (default value) if no percent desired.
#' @param NomVariable The Name of the variable you want to display. If not supplied, the name of the variable in data is used.
#' @param ConfInter Type of confidence interval (from normal, exact = Clopper-Pearson, and Jeffreys). None if no confidence interval is wanted.
#' @param ConfLevel Level of confidence for confidence intervals (by default 95%).
#' @param Test String giving the name of the comparison test performed ("none", "chisq", "fisher").
#' @param ChifPval Number of decimals for the Pvalue if test is performed.
#' @param NomCol Vector of strings to name each column of the output. Automatic display if unspecified.
#' @param Langue "fr" for french and "eng" for english. For the display in the table.
#' @param Ordonnee If FALSE, displays the classes of x in decreasing count order. If TRUE, displays in the original order.
#' @param Grapher Boolean if you want to graph the distribution in univariate case.
#' @param Simplif Not usefull, if TRUE will delete unused column 'Pvalue'.
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @seealso [Description()]
#'
#' @examples
#' TabQuali(mtcars, cyl)
#' TabQuali(mtcars, cyl, am)
#' TabQuali(mtcars, cyl, am,
#'          nomvariable = "Number of cylinders",
#'          test = "chisq", chif_pval = 3)
TabQuali <- function(.Data,
                     x,
                     y = NULL,
                     Langue = "eng",
                     Prec = 0,
                     ConfInter = c("none", "normal", "exact", "jeffreys"),
                     ConfLevel = .95,
                     PMissing = NULL,
                     Test = "none",
                     ChifPval = 2,
                     NomCol = NULL,
                     Ordonnee = FALSE,
                     Grapher = FALSE,
                     NomVariable = NULL,
                     Simplif = TRUE) {

  # Interest variables defused so that it is possible to give unquoted arguments
  x <- rlang::enexpr(x)
  VarQuali <- rlang::eval_tidy(x, data = .Data)
  y <- rlang::enexpr(y)

  # Verifications
  stopifnot(is.logical(Ordonnee), length(Ordonnee) == 1, is.logical(Grapher), length(Grapher) == 1)
  Langue <- VerifArgs(Langue)
  Prec <- VerifArgs(Prec)
  ConfInter <- VerifArgs(ConfInter)
  ConfLevel <- VerifArgs(ConfLevel)
  VarQuali <- VerifArgs(VarQuali, Ordonnee)
  NomVariable <- VerifArgs(NomVariable, x)
  PMissing <- VerifArgs(PMissing)

  if (is.null(y)) { # Univariate description

    # Store statistics
    X <- table(VarQuali, useNA = "no")
    N <- sum(X)
    M <- if (is.null(PMissing)) paste0(sum(is.na(VarQuali))) else sprintf(paste0("%i(%.", PMissing, "f%%)"), sum(is.na(VarQuali)), sum(is.na(VarQuali)) / length(VarQuali))
    Pourcent <- list(fmt = if (ConfInter == "none") paste0("%i/%i (", Prec, "%%)") else paste0("%i/%i (", Prec, "%%[", Prec, ";", Prec, "])"),
                     X,
                     N,
                     100 * X / N)
    if (ConfInter == "normal") {
      Pourcent <- append(Pourcent,
                         list(qnorm((1 - ConfLevel) / 2,
                                    mean = X / N,
                                    sd = sqrt((X / N) * (1 - X / N) / N))))
      Pourcent <- append(Pourcent,
                         list(qnorm((1 + ConfLevel) / 2,
                                    mean = X / N,
                                    sd = sqrt((X / N) * (1 - X / N) / N))))
      Pourcent[[5]] <- 100 * pmax(Pourcent[[5]], 0)
      Pourcent[[6]] <- 100 * pmin(Pourcent[[6]], 1)
    } else if (ConfInter == "exact") {
      Pourcent <- append(Pourcent, list(
        100 * qbeta((1 - ConfLevel) / 2, X, N - X + 1),
        100 * qbeta((1 + ConfLevel) / 2, X + 1, N - X)))
    } else if (ConfInter == "jeffreys") {
      Pourcent <- append(Pourcent, list(
        100 * qbeta((1 - ConfLevel) / 2, X + .5, N - X + .5),
        100 * qbeta((1 + ConfLevel) / 2, X + .5, N - X + .5)))
    }
    Pourcent <- do.call("sprintf", Pourcent)

    # Table of results
    Tableau <- data.frame(var = paste0(NomVariable, " (n, %)"),
                          eff = c(paste0("  ", levels(VarQuali)),
                                  ifelse(Langue == "fr", "    Manquants", ifelse(Langue == "eng", "    Missings", "    ..."))),
                          stats = c(Pourcent, M),
                          stringsAsFactors = FALSE)
    if (Grapher) Tableau$graphes <- c(lapply(levels(VarQuali), \(x) GGBar(as.numeric(VarQuali == x), NULL, Prec)), "")
    attr(Tableau, "crossed") <- "univariate"

    # Name of columns
    if (is.null(NomCol)) {
      if (Langue == "fr") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistiques", "Graphes") else c("Variable", "Label", "Statistiques")}
      else if (Langue == "eng") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistics", "Graphs") else c("Variable", "Label", "Statistics")}
    } else {
      if (length(NomCol) != ncol(Tableau)) stop(paste0("\"NomCol\" argument isn't of length", ncol(Tableau), "."), call. = FALSE)
      colnames(Tableau) <- NomCol
    }

  } else { # Multivariate description

    VarCroise <- rlang::eval_tidy(y, data = .Data)
    VarQuali <- VarQuali[!is.na(VarCroise)]
    VarCroise <- VarCroise[!is.na(VarCroise)]
    NClasses <- length(unique(VarCroise))

    # Verifications on statistical test
    Test <- VerifTest(Test, "quali", NClasses, VarQuali, y, x)
    ChifPval <- VerifArgs(ChifPval)

    # Statistics
    X <- as.data.frame(table(VarQuali, VarCroise, useNA = "no")) # In fact, as VarQuali is transformed as a factor, no need to track for categories as they are ordered with levels
    N <- tapply(VarQuali, VarCroise, \(x) sum(!is.na(x), na.rm = TRUE))
    M <- if (is.null(PMissing)) {
      tapply(VarQuali, VarCroise, \(x) paste0(sum(is.na(x), na.rm = TRUE)))
    } else {
      tapply(VarQuali, VarCroise, \(x) sprintf(paste0("%i(%.", PMissing, "f%%)"), sum(is.na(x)), sum(is.na(x)) / length(x)))
    }
    PourcentsCrois <- purrr::pmap_dfc(
      list(.X = split(X, X$VarCroise),
           .N = N,
           .M = M),
      \(.X, .N, .M) {
        Pourcent <- list(fmt = if (ConfInter == "none") paste0("%i/%i (", Prec, "%%)") else paste0("%i/%i (", Prec, "%%[", Prec, ";", Prec, "])"),
                         .X$Freq,
                         .N,
                         100 * .X$Freq / .N)
        if (ConfInter == "normal") {
          Pourcent <- append(Pourcent,
                             list(qnorm((1 - ConfLevel) / 2,
                                        mean = .X$Freq / .N,
                                        sd = sqrt((.X$Freq / .N) * (1 - .X$Freq / .N) / .N))))
          Pourcent <- append(Pourcent,
                             list(qnorm((1 + ConfLevel) / 2,
                                        mean = .X$Freq / .N,
                                        sd = sqrt((.X$Freq / .N) * (1 - .X$Freq / .N) / .N))))
          Pourcent[[5]] <- 100 * pmax(Pourcent[[5]], 0)
          Pourcent[[6]] <- 100 * pmin(Pourcent[[6]], 1)
        } else if (ConfInter == "exact") {
          Pourcent <- append(Pourcent, list(
            100 * qbeta((1 - ConfLevel) / 2, .X$Freq, .N - .X$Freq + 1),
            100 * qbeta((1 + ConfLevel) / 2, .X$Freq + 1, .N - .X$Freq)))
        } else if (ConfInter == "jeffreys") {
          Pourcent <- append(Pourcent, list(
            100 * qbeta((1 - ConfLevel) / 2, .X$Freq + .5, .N - .X$Freq + .5),
            100 * qbeta((1 + ConfLevel) / 2, .X$Freq + .5, .N - .X$Freq + .5)))
        }
        Pourcent <- do.call("sprintf", Pourcent)
        Pourcent <- c(Pourcent, .M)
        return(Pourcent)
      }
    )
    if (Test != "none") Pval <- c(MakeTest(VarQuali, VarCroise, Test, rlang::quo_name(x), rlang::quo_name(y), ChifPval), rep("", nlevels(VarQuali)))

    # Table of results
    Tableau <- data.frame(var = paste0(NomVariable, " (n, %)"),
                          eff = c(paste0("  ", levels(VarQuali)),
                                  ifelse(Langue == "fr", "    Manquants", ifelse(Langue == "eng", "    Missings", "    ..."))),
                          stringsAsFactors = FALSE)
    Tableau <- cbind(Tableau, as.matrix(PourcentsCrois))
    if (Test != "none") Tableau$pval <- Pval
    attr(Tableau, "crossed") <- "multivariate"
    if (Grapher) message(Information("Graphs aren't supported in multivariate description."))

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

  }

  if (Simplif && sum(is.na(VarQuali)) == 0) Tableau <- Tableau[- nrow(Tableau), ]

  class(Tableau) <- c("tab_datavar", class(Tableau))
  if (Grapher & is.null(y)) attr(Tableau, "Grapher") <- TRUE
  return(Tableau)

}

