#' TabQuali
#'
#' Description of categorical variables
#'
#' A function to describe categorical variables with count and percentage (+ binomial confidence interval if supplied).
#' The difference with \code{TabBinaire} is that all categories are displayed.
#'
#' You can also use it to cross it with a categorical variable and perform a comparison test (chisq test, fisher test).
#'
#' @param .Data The dataset that contains the variables.
#' @param x The categorical variable to describe. Should be the name without "".
#' @param y The qualitative variable to perform the bivariate description. Should be the name without "". If unspecified, univariate description.
#' @param Prec Number of decimals for the percentages.
#' @param PMissing Number of decimals of percentage from whole dataset. NULL (default value) if no percent desired.
#' @param NomVariable The Name of the variable you want to display. If not supplied, the name of the variable in data is used.
#' @param ConfInter Type of confidence interval (from normal, exact = Clopper-Pearson, and Jeffreys). None if no confidence interval is wanted.
#' @param ConfLevel Level of confidence for confidence intervals (by default 95%).
#' @param Poids Name of the column of .Data in which the weights are stored. Let NULL for unweighted analysis.
#' @param Test String giving the name of the comparison test performed ("none", "chisq", "fisher", "multinomial").
#' @param SMD Boolean to indicate if you want standardized mean differences. Of note, for weighted analysis, only SMD are available and no test.
#' @param P0 Numeric vector of length the number of categories in x between 0 and 1 giving the probability(ies) under H0. If unspecified equal probability to all groups.
#' @param ChifPval Number of decimals for the Pvalue if test is performed.
#' @param NomCol Vector of strings to name each column of the output. Automatic display if unspecified.
#' @param Langue "fr" for french and "eng" for english. For the display in the table.
#' @param Ordonnee If FALSE, displays the classes of x in decreasing count order. If TRUE, displays in the original order.
#' @param Grapher Boolean if you want to graph the distribution in univariate case.
#' @param Simplif Not usefull, if TRUE will delete unused column 'Pvalue'.
#'
#' @details
#' The standardized mean differences are computed accordingly to Yang and Dalton (2012).
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @seealso [Description()]
#'
#' @references
#' Yang, D., & Dalton, J. E. (2012, April). A unified approach to measuring the effect size between two groups using SAS. In SAS global forum (Vol. 335, pp. 1-6).
#'
#' @examples
#' TabQuali(mtcars, cyl)
#' TabQuali(mtcars, cyl, Test = "multinomial")
#' TabQuali(mtcars, cyl, am)
#' TabQuali(mtcars, cyl, am,
#'          NomVariable = "Number of cylinders",
#'          Test = "chisq", ChifPval = 3)
TabQuali <- function(.Data,
                     x,
                     y = NULL,
                     Prec = 0,
                     PMissing = NULL,
                     NomVariable = NULL,
                     ConfInter = c("none", "normal", "exact", "jeffreys"),
                     ConfLevel = .95,
                     Poids = NULL,
                     Test = "none",
                     SMD = FALSE,
                     P0 = NULL,
                     ChifPval = 2,
                     NomCol = NULL,
                     Langue = "eng",
                     Ordonnee = FALSE,
                     Grapher = FALSE,
                     Simplif = TRUE) {

  # Interest variables defused so that it is possible to give unquoted arguments
  x <- rlang::enexpr(x)
  VarQuali <- rlang::eval_tidy(x, data = .Data)
  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)

  # Verifications
  stopifnot(is.logical(Ordonnee), length(Ordonnee) == 1, is.logical(Grapher), length(Grapher) == 1)
  Langue <- VerifArgs(Langue)
  Prec <- VerifArgs(Prec, x)
  VarQuali <- VerifArgs(VarQuali, Ordonnee, x)
  Poids <- VerifArgs(Poids, x, VarQuali, .Data)
  ConfInter <- VerifArgs(ConfInter, Poids)
  ConfLevel <- VerifArgs(ConfLevel, x)
  NomVariable <- VerifArgs(NomVariable, x)
  PMissing <- VerifArgs(PMissing)
  HelperN <- if (all(Poids %in% c(0, 1))) "%i" else "%.2f" # Helper for formatting of N

  if (is.null(y)) { # Univariate description

    # Store statistics
    X <- tapply(Poids, VarQuali, sum, na.rm = TRUE)
    X[is.na(X)] <- 0
    N <- sum(X)
    M <- if (is.null(PMissing)) sprintf(HelperN, sum(as.numeric(is.na(VarQuali)) * Poids)) else sprintf(paste0(HelperN, "(%.", PMissing, "f%%)"), sum(as.numeric(is.na(VarQuali)) * Poids), sum(as.numeric(is.na(VarQuali)) * Poids) / sum(Poids))
    Pourcent <- list(fmt = if (ConfInter == "none") paste0(HelperN, "/", HelperN, " (", Prec, "%%)") else paste0(HelperN, "/", HelperN, " (", Prec, "%%[", Prec, ";", Prec, "])"),
                     X,
                     N,
                     100 * X / N)
    if (ConfInter == "normal") {
      Pourcent <- append(Pourcent,
                         list(qnorm((1 - ConfLevel) / 2,
                                    mean = X / N,
                                    sd = purrr::map_dbl(names(X), ~ sqrt(WeightedVar(VarQuali == .x, Poids, Corr = 0) / N)))))
      Pourcent <- append(Pourcent,
                         list(qnorm((1 + ConfLevel) / 2,
                                    mean = X / N,
                                    sd = purrr::map_dbl(names(X), ~ sqrt(WeightedVar(VarQuali == .x, Poids, Corr = 0) / N)))))
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
    if (Test != "none") {
      P0 <- VerifArgs(P0, VarQuali, x)
      Test <- VerifTest(Test, "quali", 1, VarQuali, y, x, Poids)
      if (Test != "none") {
        NomVariable <- paste0(NomVariable, " (n, %) [*&pi;~0~=", paste(round(P0, 2), collapse = "/"), "*]")
        Pval <- c(MakeTest(VarQuali, NULL, if (Test == "ztest") "chisq" else Test, rlang::quo_name(x), rlang::quo_name(y), ChifPval, Mu = P0), rep("", nlevels(VarQuali)))
      }
    } else {
      NomVariable <- paste0(NomVariable, " (n, %)")
    }

    # Table of results
    Tableau <- data.frame(var = NomVariable,
                          eff = c(paste0("  ", levels(VarQuali)),
                                  ifelse(Langue == "fr", "    Manquants", ifelse(Langue == "eng", "    Missings", "    ..."))),
                          stats = c(Pourcent, M),
                          stringsAsFactors = FALSE)
    if (Test != "none") Tableau$pval <- Pval
    if (Grapher) Tableau$graphes <- c(lapply(levels(VarQuali), \(x) GGBar(as.numeric(VarQuali == x), NULL, Poids, Prec)), "")
    attr(Tableau, "crossed") <- "univariate"

    # Name of columns
    if (is.null(NomCol)) {
      if (Langue == "fr") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistiques", if (Test == "none") NULL else "PValue", "Graphes") else c("Variable", "Label", "Statistiques", if (Test == "none") NULL else "PValue")}
      else if (Langue == "eng") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistics", if (Test == "none") NULL else "PValue", "Graphs") else c("Variable", "Label", "Statistics", if (Test == "none") NULL else "PValue")}
    } else {
      if (length(NomCol) != ncol(Tableau)) stop(paste0("\"", PrintArg("NomCol"), "\" argument isn't of length", ncol(Tableau), " for ", PrintVar(rlang::quo_name(x)), "."), call. = FALSE)
      colnames(Tableau) <- NomCol
    }

  } else { # Multivariate description

    VarCroise <- rlang::eval_tidy(y, data = .Data)
    VarQuali <- VarQuali[!is.na(VarCroise)]
    Poids <- Poids[!is.na(VarCroise)]
    VarCroise <- VarCroise[!is.na(VarCroise)]
    NClasses <- length(unique(VarCroise))

    # Verifications on statistical test
    Test <- VerifTest(Test, "quali", NClasses, VarQuali, y, x, Poids)
    ChifPval <- VerifArgs(ChifPval)

    # Statistics
    X <- dplyr::count(data.frame(VarQuali, VarCroise, Poids), VarQuali, VarCroise, wt = Poids, name = "Freq") |>
      tidyr::complete(VarQuali, VarCroise, fill = list(Freq = 0)) |>
      as.data.frame() |>
      dplyr::filter(!is.na(VarQuali))
    N <- tapply(Poids * as.numeric(!is.na(VarQuali)), VarCroise, sum, na.rm = TRUE)
    M <- if (is.null(PMissing)) {
      sprintf(HelperN, tapply(Poids * as.numeric(is.na(VarQuali)), VarCroise, sum, na.rm = TRUE))
    } else {
      TempNum <- tapply(as.numeric(is.na(VarQuali)) * Poids, VarCroise, sum, na.rm = TRUE)
      TempDenom <- tapply(Poids, VarCroise, sum, na.rm = TRUE)
      sprintf(paste0(HelperN, "(%.", PMissing, "f%%)"), TempNum, 100 * TempNum / TempDenom)
    }
    PourcentsCrois <- purrr::pmap_dfc(
      list(.X = split(X, X$VarCroise),
           .N = N,
           .M = M),
      \(.X, .N, .M) {
        VarTempQ <- VarQuali[VarCroise == .X$VarCroise[1]]
        VarTempP <- Poids[VarCroise == .X$VarCroise[1]]
        Pourcent <- list(fmt = if (ConfInter == "none") paste0(HelperN, "/", HelperN, " (", Prec, "%%)") else paste0(HelperN, "/", HelperN, " (", Prec, "%%[", Prec, ";", Prec, "])"),
                         .X$Freq,
                         .N,
                         100 * .X$Freq / .N)
        if (ConfInter == "normal") {
          Pourcent <- append(Pourcent,
                             list(qnorm((1 - ConfLevel) / 2,
                                        mean = .X$Freq / .N,
                                        sd = purrr::map_dbl(.X$VarQuali, ~ sqrt(WeightedVar(VarTempQ == .x, VarTempP, Corr = 0) / .N)))))
          Pourcent <- append(Pourcent,
                             list(qnorm((1 + ConfLevel) / 2,
                                        mean = .X$Freq / .N,
                                        sd = purrr::map_dbl(.X$VarQuali, ~ sqrt(WeightedVar(VarTempQ == .x, VarTempP, Corr = 0) / .N)))))
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
    if (SMD) {
      if (NClasses != 2) {
        stop(paste0("For variable \"", PrintVar(rlang::quo_name(x)), "\", there aren't 2 groups and thus pairwise SMDs aren't yet supported. Please set argument \"", PrintArg("SMD"), "\" to FALSE."), call. = FALSE)
      } else {
        LabelsCroisement <- names(table(VarCroise))
        TempSmd <- SmdProp(VarQuali[VarCroise == LabelsCroisement[1]], Poids[VarCroise == LabelsCroisement[1]],
                           VarQuali[VarCroise == LabelsCroisement[2]], Poids[VarCroise == LabelsCroisement[2]])
        DMS <- c(FormatPval(TempSmd, ChifPval), rep("", nrow(PourcentsCrois) - 1))
      }
    }

    # Table of results
    Tableau <- data.frame(var = paste0(NomVariable, " (n, %)"),
                          eff = c(paste0("  ", levels(VarQuali)),
                                  ifelse(Langue == "fr", "    Manquants", ifelse(Langue == "eng", "    Missings", "    ..."))),
                          stringsAsFactors = FALSE)
    Tableau <- cbind(Tableau, as.matrix(PourcentsCrois))
    if (Test != "none") Tableau$pval <- Pval
    if (SMD) {Tableau$smd <- DMS;attr(Tableau, "standardized_mean_difference") <- TempSmd}
    attr(Tableau, "crossed") <- "multivariate"
    if (Grapher) message(Information("Graphs aren't supported in multivariate description."))

    # Names of columns
    if (is.null(NomCol)) {
      if (Langue == "fr") {
        colnames(Tableau) <- c("Variable", "Label",
                               paste0(rep("Statistiques (", NClasses), rlang::quo_name(y), "=", unique(VarCroise), ")"),
                               if (Test == "none") NULL else "PValue",
                               if (SMD) "SMD" else NULL)
      } else {
        colnames(Tableau) <- c("Variable", "Label",
                               paste0(rep("Statistics (", NClasses), rlang::quo_name(y), "=", unique(VarCroise), ")"),
                               if (Test == "none") NULL else "PValue",
                               if (SMD) "SMD" else NULL)
      }
    } else {
      if (length(NomCol) != ncol(Tableau)) stop(paste0("\"", PrintArg("NomCol"), "\" argument isn't of length", ncol(Tableau), " for variable \"", PrintVar(rlang::quo_name(x)), "\"."), call. = FALSE)
      colnames(Tableau) <- NomCol
    }

  }

  if (Simplif && sum(is.na(VarQuali)) == 0) Tableau <- Tableau[- nrow(Tableau), ]

  class(Tableau) <- c("tab_description", class(Tableau))
  attr(Tableau, "Grapher") <- Grapher & is.null(y)
  attr(Tableau, "Comparer") <- Test != "none"
  return(Tableau)

}


#' @rdname TabQuali
#' @importFrom rlang !!
NoMessTabQuali <- function(.Data,
                           x,
                           y = NULL,
                           Prec = 0,
                           PMissing = NULL,
                           NomVariable = NULL,
                           ConfInter = c("none", "normal", "exact", "jeffreys"),
                           ConfLevel = .95,
                           Poids = NULL,
                           Test = "none",
                           SMD = FALSE,
                           P0 = NULL,
                           ChifPval = 2,
                           NomCol = NULL,
                           Langue = "eng",
                           Ordonnee = FALSE,
                           Grapher = FALSE,
                           Simplif = TRUE) {

  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)
  suppressMessages(Tableau <- TabQuali(.Data = .Data,
                                       x = !!x,
                                       y = !!y,
                                       Prec = Prec,
                                       PMissing = PMissing,
                                       NomVariable = NomVariable,
                                       ConfInter = ConfInter,
                                       ConfLevel = ConfLevel,
                                       Poids = !!Poids,
                                       Test = Test,
                                       SMD = SMD,
                                       P0 = P0,
                                       ChifPval = ChifPval,
                                       NomCol = NomCol,
                                       Langue = Langue,
                                       Ordonnee = Ordonnee,
                                       Grapher = Grapher,
                                       Simplif = Simplif))
  return(Tableau)

}


#' @rdname TabQuali
#' @importFrom rlang !!
SilentTabQuali <- function(.Data,
                           x,
                           y = NULL,
                           Prec = 0,
                           PMissing = NULL,
                           NomVariable = NULL,
                           ConfInter = c("none", "normal", "exact", "jeffreys"),
                           ConfLevel = .95,
                           Poids = NULL,
                           Test = "none",
                           SMD = FALSE,
                           P0 = NULL,
                           ChifPval = 2,
                           NomCol = NULL,
                           Langue = "eng",
                           Ordonnee = FALSE,
                           Grapher = FALSE,
                           Simplif = TRUE) {

  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)
  suppressMessages(suppressWarnings(Tableau <- TabQuali(.Data = .Data,
                                                        x = !!x,
                                                        y = !!y,
                                                        Prec = Prec,
                                                        PMissing = PMissing,
                                                        NomVariable = NomVariable,
                                                        ConfInter = ConfInter,
                                                        ConfLevel = ConfLevel,
                                                        Poids = !!Poids,
                                                        Test = Test,
                                                        SMD = SMD,
                                                        P0 = P0,
                                                        ChifPval = ChifPval,
                                                        NomCol = NomCol,
                                                        Langue = Langue,
                                                        Ordonnee = Ordonnee,
                                                        Grapher = Grapher,
                                                        Simplif = Simplif)))
  return(Tableau)

}

