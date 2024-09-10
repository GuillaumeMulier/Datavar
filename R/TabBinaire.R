#' TabBinaire
#'
#' Description of binary variables
#'
#' A function to describe binary variables with count and percentage (+ binomial confidence interval if supplied).
#' The difference with \code{TabQuali} is that \code{TabBinaire} only displays results for the wanted category supplied.
#'
#' You can also use it to cross it with a categorial variable and perform a comparison test (chisq or fisher test).
#'
#' @param .Data The dataset that contains the variables.
#' @param x The binary variable to describe.
#' @param y The qualitative variable to perform the bivariate description. If unspecified, univariate description.
#' @param Prec Number of decimals for the percentages.
#' @param PMissing Number of decimals of percentage from whole dataset. NULL (default value) if no percent desired.
#' @param NomCateg The value of the category which you want to display in the table.
#' @param NomLabel String giving the name of the class that you want to display in the table.
#' @param ConfInter Type of confidence interval (from normal, exact = Clopper-Pearson, and jeffreys = Jeffreys). None if no confidence interval is wanted.
#' @param ConfLevel Level of confidence for confidence intervals (by default 95%).
#' @param Poids Name of the column of .Data in which the weights are stored. Let NULL for unweighted analysis.
#' @param Test String giving the name of the comparison test performed ("none", "chisq", "fisher", ""binomial").
#' @param SMD Boolean to indicate if you want standardized mean differences. Of note, for weighted analysis, only SMD are available and no test.
#' @param P0 Either length 1 or length 2 numeric vector between 0 and 1 giving the probability(ies) under H0.
#' When 1 proportion is supplied, it is assigned to value NomCateg. If unspecified equal probability to all groups.
#' @param ChifPval Number of decimal for PValue.
#' @param NomCol Vector of strings to name each column of the output. Automatic display if unspecified.
#' @param Langue "fr" for french and "eng" for english. For the display in the table.
#' @param Grapher Boolean if you want to graph the distribution in univariate case.
#' @param Simplif Boolean. If TRUE (default value) the table will be simplified to remove number of missing data if there aren't any.
#'
#' @export
#'
#' @seealso [Description()]
#'
#' @examples
#' TabBinaire(mtcars, am)
#' TabBinaire(mtcars, am, Test = "binomial")
#' TabBinaire(mtcars, am, vs)
#' TabBinaire(mtcars, am, vs,
#'            NomCateg = "0", NomLabel = "Automatic transmission",
#'            Test = "fisher")
TabBinaire <- function(.Data,
                       x,
                       y = NULL,
                       Prec = 0,
                       PMissing = NULL,
                       NomCateg = NULL,
                       NomLabel = NULL,
                       ConfInter = "none",
                       ConfLevel = .95,
                       Poids = NULL,
                       Test = "none",
                       SMD = FALSE,
                       P0 = NULL,
                       ChifPval = 2,
                       NomCol = NULL,
                       Langue = "eng",
                       Grapher = FALSE,
                       Simplif = FALSE) {

  # Interest variables defused so that it is possible to give unquoted arguments
  x <- rlang::enexpr(x)
  VarBinaire <- rlang::eval_tidy(x, data = .Data)
  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)

  # Verifications
  stopifnot(is.logical(Grapher), length(Grapher) == 1)
  Langue <- VerifArgs(Langue)
  Prec <- VerifArgs(Prec, x)
  NomCateg <- VerifArgs(NomCateg, NomLabel, VarBinaire, x, Binaire = TRUE)
  VarBinaire <- VerifArgs(VarBinaire, NomCateg, x)
  NomLabel <- VerifArgs(NomLabel, VarBinaire, x)
  Poids <- VerifArgs(Poids, x, VarBinaire, .Data)
  ConfInter <- VerifArgs(ConfInter, Poids, "binary")
  ConfLevel <- VerifArgs(ConfLevel, x)
  PMissing <- VerifArgs(PMissing)
  ChifPval <- VerifArgs(ChifPval)
  HelperN <- if (all(Poids %in% c(0, 1))) "%i" else "%.2f" # Helper for formatting of N

  if (is.null(y)) { # Univariate description

    # Store statistics
    X <- sum(VarBinaire * Poids, na.rm = TRUE)
    N <- sum(as.numeric(!is.na(VarBinaire)) * Poids)
    M <- if (is.null(PMissing)) sprintf(HelperN, sum(as.numeric(is.na(VarBinaire)) * Poids)) else sprintf(paste0(HelperN, "(%.", PMissing, "f%%)"), sum(as.numeric(is.na(VarBinaire)) * Poids), 100 * sum(as.numeric(is.na(VarBinaire)) * Poids) / sum(Poids))
    Pourcent <- list(fmt = if (ConfInter == "none") paste0(HelperN, "/", HelperN, " (", Prec, "%%)") else paste0(HelperN, "/", HelperN," (", Prec, "%%[", Prec, ";", Prec, "])"),
                     X,
                     N,
                     100 * X / N)
    if (ConfInter == "normal") {
      Pourcent <- append(Pourcent,
                         lapply(c((1 - ConfLevel) / 2, (1 + ConfLevel) / 2), qnorm,
                                mean = X / N,
                                sd = sqrt(WeightedVar(VarBinaire, Poids, Corr = 0) / N)))
      Pourcent[[5]] <- 100 * max(Pourcent[[5]], 0)
      Pourcent[[6]] <- 100 * min(Pourcent[[6]], 1)
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
      P0 <- VerifArgs(P0, VarBinaire, x)
      Test <- VerifTest(Test, "binaire", 1, VarBinaire, y, x, Poids)
      if (Test != "none") {
        NomLabel <- paste0(NomLabel, " (*&pi;~0~=", paste(round(P0, 2), collapse = "/"), "*)")
        Pval <- c(MakeTest(VarBinaire, NULL, if (Test == "ztest") "chisq" else Test, rlang::quo_name(x), rlang::quo_name(y), ChifPval, Mu = P0), "")
      }
    }

    # Table of results
    Tableau <- data.frame(var = NomLabel,
                          eff = c("n, %", ifelse(Langue == "fr", "  Manquants", ifelse(Langue == "eng", "  Missings", "  ..."))),
                          stats = c(Pourcent, M),
                          stringsAsFactors = FALSE)
    if (Test != "none") Tableau$pval <- Pval
    if (Grapher) Tableau$graphes <- list(GGBar(VarBinaire, NULL, Poids, Prec), "")
    attr(Tableau, "crossed") <- "univariate"

    # Name of columns
    if (is.null(NomCol)) {
      if (Langue == "fr") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistiques", if (Test == "none") NULL else "PValue", "Graphes") else c("Variable", "Label", "Statistiques", if (Test == "none") NULL else "PValue")}
      else if (Langue == "eng") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistics", if (Test == "none") NULL else "PValue", "Graphs") else c("Variable", "Label", "Statistics", if (Test == "none") NULL else "PValue")}
    } else {
      if (length(NomCol) != ncol(Tableau)) stop(paste0("\"", PrintArg("NomCol"), "\" argument isn't of length", ncol(Tableau), "."), call. = FALSE)
      colnames(Tableau) <- NomCol
    }

  } else { # Crossed description

    VarCroise <- rlang::eval_tidy(y, data = .Data)
    VarBinaire <- VarBinaire[!is.na(VarCroise)]
    Poids <- Poids[!is.na(VarCroise)]
    VarCroise <- VarCroise[!is.na(VarCroise)]
    NClasses <- length(unique(VarCroise))

    # Verifications on statistical test
    Test <- VerifTest(Test, "binaire", NClasses, VarBinaire, y, x, Poids)

    # Statistics
    X <- tapply(VarBinaire * Poids, VarCroise, sum, na.rm = TRUE)
    N <- tapply(as.numeric(!is.na(VarBinaire)) * Poids, VarCroise, sum)
    M <- if (is.null(PMissing)) {
      tapply(as.numeric(is.na(VarBinaire)) * Poids, VarCroise, \(x) sprintf(HelperN, sum(x)))
    } else {
      TempNum <- tapply(as.numeric(is.na(VarBinaire)) * Poids, VarCroise, sum, na.rm = TRUE)
      TempDenom <- tapply(Poids, VarCroise, sum, na.rm = TRUE)
      sprintf(paste0(HelperN, "(%.", PMissing, "f%%)"), TempNum, 100 * TempNum / TempDenom)
    }
    Pourcent <- list(fmt = if (ConfInter == "none") paste0(HelperN, "/", HelperN, " (", Prec, "%%)") else paste0(HelperN, "/", HelperN, " (", Prec, "%%[", Prec, ";", Prec, "])"),
                     X,
                     N,
                     100 * X / N)
    if (ConfInter == "normal") {
      StdErr <- as.numeric(by(data.frame(VarBinaire, Poids), VarCroise,
                \(x) sqrt(WeightedVar(x$VarBinaire, x$Poids, Corr = 0) / sum(as.numeric(!is.na(x$VarBinaire)) * x$Poids))))
      Pourcent <- append(Pourcent,
                         lapply(c((1 - ConfLevel) / 2, (1 + ConfLevel) / 2), qnorm,
                                mean = X / N,
                                sd = StdErr))
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
    if (Test != "none") Pval <- c(MakeTest(VarBinaire, VarCroise, if (Test == "ztest") "chisq" else Test, rlang::quo_name(x), rlang::quo_name(y), ChifPval), "")
    if (SMD) {
      if (NClasses != 2) {
        stop(paste0("For variable \"", PrintVar(rlang::quo_name(x)), "\", there aren't 2 groups and thus pairwise SMDs aren't yet supported. Please set argument \"", PrintArg("SMD"), "\" to FALSE."), call. = FALSE)
      } else {
        LabelsCroisement <- names(table(VarCroise))
        VarTemp <- factor(VarBinaire, levels = c(0, 1))
        TempSmd <- SmdProp(VarTemp[VarCroise == LabelsCroisement[1]], Poids[VarCroise == LabelsCroisement[1]],
                                                                 VarTemp[VarCroise == LabelsCroisement[2]], Poids[VarCroise == LabelsCroisement[2]])
        DMS <- c(FormatPval(TempSmd, ChifPval), "")
      }
    }

    # Table of results
    Tableau <- data.frame(var = NomLabel,
                          eff = c("n, %", ifelse(Langue == "fr", "  Manquants", ifelse(Langue == "eng", "  Missings", "  ..."))),
                          stringsAsFactors = FALSE)
    Tableau <- cbind(Tableau, matrix(c(Pourcent, M), nrow = 2, byrow = TRUE))
    attr(Tableau, "crossed") <- "multivariate"
    if (Test != "none") Tableau$pval <- Pval
    if (Grapher) message(Information("Graphs aren't supported in multivariate description."))
    # Tableau$graphes <- list(GGBar(VarBinaire, VarCroise, Prec), "")
    # Not that informative to have graphics when multivariate description. I let it only for univariate description
    if (SMD) {Tableau$smd <- DMS;attr(Tableau, "standardized_mean_difference") <- TempSmd}

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
      if (length(NomCol) != ncol(Tableau)) stop(paste0("\"", PrintArg("NomCol"), "\" argument isn't of length", ncol(Tableau), " for variable \"", rlang::quo_name(x), "\"."), call. = FALSE)
      colnames(Tableau) <- NomCol
    }

  }

  if (Simplif && sum(is.na(VarBinaire)) == 0) Tableau <- Tableau[- nrow(Tableau), ]

  class(Tableau) <- c("tab_description", class(Tableau))
  attr(Tableau, "Grapher") <- Grapher & is.null(y)
  attr(Tableau, "Comparer") <- Test != "none"
  return(Tableau)

}


#' @rdname TabBinaire
#' @importFrom rlang !!
NoMessTabBinaire <- function(.Data,
                             x,
                             y = NULL,
                             Prec = 0,
                             PMissing = NULL,
                             NomCateg = NULL,
                             NomLabel = NULL,
                             ConfInter = c("none", "normal", "exact", "jeffreys"),
                             ConfLevel = .95,
                             Poids = NULL,
                             Test = "none",
                             SMD = FALSE,
                             P0 = NULL,
                             ChifPval = 2,
                             NomCol = NULL,
                             Langue = "eng",
                             Grapher = FALSE,
                             Simplif = FALSE) {

  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)
  suppressMessages(Tableau <- TabBinaire(.Data = .Data,
                                         x = !!x,
                                         y = !!y,
                                         Prec = Prec,
                                         PMissing = PMissing,
                                         NomCateg = NomCateg,
                                         NomLabel = NomLabel,
                                         ConfInter = ConfInter,
                                         ConfLevel = ConfLevel,
                                         Poids = !!Poids,
                                         Test = Test,
                                         SMD = SMD,
                                         P0 = P0,
                                         ChifPval = ChifPval,
                                         NomCol = NomCol,
                                         Langue = Langue,
                                         Grapher = Grapher,
                                         Simplif = Simplif))
  return(Tableau)

}


#' @rdname TabBinaire
#' @importFrom rlang !!
SilentTabBinaire <- function(.Data,
                             x,
                             y = NULL,
                             Prec = 0,
                             PMissing = NULL,
                             NomCateg = NULL,
                             NomLabel = NULL,
                             ConfInter = c("none", "normal", "exact", "jeffreys"),
                             ConfLevel = .95,
                             Poids = NULL,
                             Test = "none",
                             SMD = FALSE,
                             P0 = NULL,
                             ChifPval = 2,
                             NomCol = NULL,
                             Langue = "eng",
                             Grapher = FALSE,
                             Simplif = FALSE) {

  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)
  suppressMessages(suppressWarnings(Tableau <- TabBinaire(.Data = .Data,
                                                          x = !!x,
                                                          y = !!y,
                                                          Prec = Prec,
                                                          PMissing = PMissing,
                                                          NomCateg = NomCateg,
                                                          NomLabel = NomLabel,
                                                          ConfInter = ConfInter,
                                                          ConfLevel = ConfLevel,
                                                          Poids = !!Poids,
                                                          Test = Test,
                                                          SMD = SMD,
                                                          P0 = P0,
                                                          ChifPval = ChifPval,
                                                          NomCol = NomCol,
                                                          Langue = Langue,
                                                          Grapher = Grapher,
                                                          Simplif = Simplif)))
  return(Tableau)

}

