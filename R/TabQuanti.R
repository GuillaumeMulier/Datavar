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
#' @param Poids Name of the column of .Data in which the weights are stored. Let NULL for unweighted analysis.
#' @param Test String giving the test to compare the statistic between the 2 groups if needed ("none", "ztest", "student", "studentvar" for Welch correction, "wilcoxon", "anova", "kruskal-wallis").
#' @param SMD Boolean to indicate if you want standardized mean differences. Of note, for weighted analysis, only SMD are available and no test.
#' @param ChifPval Number of decimals for the Pvalue if needed.
#' @param NomCol Vector of strings to name each column of the output. Automatic display if unspecified.
#' @param ConfInter Type of confidence interval (from student = Student). None if no confidence interval is wanted.
#' @param ConfLevel Level of confidence for confidence intervals (by default 95%).
#' @param Langue "fr" for french and "eng" for english. For the display in the table.
#' @param Grapher Boolean if you want to graph the distribution in univariate case.
#' @param Simplif Not usefull, if TRUE will delete unused column 'Pvalue'.
#' @param Paired NULL if data are not paired/matched, else the name of the pairing variable as character (used only for bivariate analysis).
#'
#' @export
#'
#' @seealso [Description()]
#'
#' @examples
#' TabQuanti(.Data = mtcars, x = mpg, Prec = 1)
#' TabQuanti(.Data = mtcars, x = mpg, Prec = 1, Test = "signed-wilcoxon", Mu0 = 20)
#' TabQuanti(.Data = mtcars, x = mpg, y = am, Prec = 1)
#' TabQuanti(.Data = mtcars, x = mpg, y = am, Prec = 1,
#'           NomVariable = "Miles per gallon",
#'           Test = "stud", ChifPval = 3)
TabQuanti <- function(.Data,
                      x,
                      y = NULL,
                      Prec = 0,
                      PMissing = NULL,
                      NomVariable = NULL,
                      Mode = "mediqrg",
                      Poids = NULL,
                      Test = "none",
                      SMD = FALSE,
                      Mu0 = 0,
                      ChifPval = 2,
                      NomCol = NULL,
                      ConfInter = "none",
                      ConfLevel = .95,
                      Langue = "eng",
                      Grapher = FALSE,
                      Simplif = TRUE,
                      Paired = NULL) {

  # Interest variables defused so that it is possible to give unquoted arguments
  x <- rlang::enexpr(x)
  VarQuanti <- rlang::eval_tidy(x, data = .Data)
  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)

  # Verifications
  stopifnot(is.logical(Grapher), length(Grapher) == 1)
  Langue <- VerifArgs(Langue)
  Prec <- VerifArgs(Prec, x)
  NomVariable <- VerifArgs(NomVariable, x)
  PMissing <- VerifArgs(PMissing)
  VarQuanti <- VerifArgs(VarQuanti, x)
  Poids <- VerifArgs(Poids, x, VarQuanti, .Data, Paired)
  ConfInter <- VerifArgs(ConfInter, Poids, "quanti")
  ConfLevel <- VerifArgs(ConfLevel, x)
  HelperN <- if (all(Poids %in% c(0, 1))) "%i" else "%.2f" # Helper for formatting of N
  Mode <- VerifArgs(Mode, x, Langue, Prec, PMissing, HelperN)

  if (is.null(y)) { # Univariate description

    # Store statistics and labels
    Statistics <- purrr::map_chr(
      Mode,
      function(tab) {
        Res <- purrr::map_chr(seq_len(nrow(tab)), ~ tab$fct[[.x]](VarQuanti, Poids, tab$precision[.x]))
        return(paste(Res, collapse = ", "))
      }
    )
    Labelliseurs <- purrr::map_chr(Mode, \(tab) return(paste(tab$label, collapse = ", ")))
    if (Test != "none") {
      Mu0 <- VerifArgs(Mu0, VarQuanti, x)
      Test <- VerifTest(Test, "quanti", 1, VarQuanti, y, x, Poids)
      if (Test != "none") {
        NomVariable <- paste0(NomVariable, sprintf(paste0(" [*&mu;~0~=", Prec, "*]"), Mu0))
        Pval <- MakeTest(VarQuanti, NULL, Test, rlang::quo_name(x), NULL, ChifPval, Mu = Mu0)
      }
    }

    # Table of results
    Tableau <- data.frame(var = NomVariable,
                          eff = Labelliseurs,
                          stats = Statistics,
                          stringsAsFactors = FALSE)
    if (Test != "none") Tableau$pval <- c("", Pval, rep("", nrow(Tableau) - 2))
    if (Grapher) Tableau$graphes <- c(list(GGHist(VarQuanti, Poids)), rep("", nrow(Tableau) - 1))
    attr(Tableau, "crossed") <- "univariate"

    # Name of columns
    if (is.null(NomCol)) {
      if (Langue == "fr") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistiques", if (Test == "none") NULL else "PValue", "Graphes") else c("Variable", "Label", "Statistiques", if (Test == "none") NULL else "PValue")}
      else if (Langue == "eng") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistics", if (Test == "none") NULL else "PValue", "Graphs") else c("Variable", "Label", "Statistics", if (Test == "none") NULL else "PValue")}
    } else {
      if (length(NomCol) != ncol(Tableau)) stop(paste0("\"", PrintArg("NomCol"), "\" argument isn't of length", ncol(Tableau), " for ", PrintVar(rlang::quo_name(x)), "."), call. = FALSE)
      colnames(Tableau) <- NomCol
    }

    # Simplify table if no missing values
    if (Simplif && sum(is.na(VarQuanti)) == 0) {
      Tableau[1, 2] <- "N"
      Tableau[1, 3] <- gsub("^(.+), .*$", "\\1", Tableau[1, 3])
    }

  } else { # Multivariate description

    VarCroise <- rlang::eval_tidy(y, data = .Data)
    VarQuanti <- VarQuanti[!is.na(VarCroise)]
    Poids <- Poids[!is.na(VarCroise)]
    VarCroise <- VarCroise[!is.na(VarCroise)]
    NClasses <- length(unique(VarCroise))
    Paired <- VerifArgs(Paired, .Data, NClasses, x)

    # Verifications on statistical test and paired data
    if (!is.null(Paired) & NClasses != 2) stop(paste0("For variable ", PrintVar(rlang::quo_name(x)), ", there is not 2 groups so paired analysis is not possible."), call. = FALSE)
    Test <- VerifTest(Test, "quanti", NClasses, VarQuanti, y, x, Poids, !is.null(Paired))
    ChifPval <- VerifArgs(ChifPval)

    # Store statistics and labels
    Statistics <- tapply(seq_along(VarQuanti), VarCroise,
                         \(index) {
                           purrr::map_chr(
                             Mode,
                             function(tab) {
                               Res <- purrr::map_chr(seq_len(nrow(tab)), ~ tab$fct[[.x]](VarQuanti[index], Poids[index], tab$precision[.x]))
                               return(paste(Res, collapse = ", "))
                             }
                           )
                         })
    Labelliseurs <- purrr::map_chr(Mode, \(tab) return(paste(tab$label, collapse = ", ")))
    if (!is.null(Paired)) { # Add the difference of means when paired data and confidence interval is asked
      ProcessedData <- ProcessPairedQuanti(VarQuanti, VarCroise, Paired, .Data, NameX)
      Difference <- ProcessedData[[1]] - ProcessedData[[2]]
      N <- sum(!is.na(Difference))
      MoyD <- mean(Difference, na.rm = TRUE)
      SdD <- sqrt(var(Difference, na.rm = TRUE))
      if (ConfInter == "student") {
        if (ShapiroTest(Difference, NULL, rlang::quo_name(x)) < .05)
          message(Attention(paste0("For variable \"", PrintVar(rlang::quo_name(x)), "\", the Shapiro-wilk test shows some departure from normal assumption. Consider using another confidence interval.")))
        IntervalleConfiance <- MoyD + qt(c((1 - ConfLevel) / 2, (1 + ConfLevel) / 2), df = N - 1) * SdD / sqrt(N)
        Statistics[[1]] <- c(Statistics[[1]], sprintf(paste0("N=%i, ", Prec, " [", Prec, ";", Prec, "]"), N, MoyD, IntervalleConfiance[1], IntervalleConfiance[2]))
        Statistics[[2]] <- c(Statistics[[2]], "")
      } else if (ConfInter == "normal") {
        if (N < 30)
          warning(Attention(paste0("Less than 30 observations, ensure that assumptions are checked for variable \"", PrintVar(NameX), "\".")), immediate. = TRUE, call. = FALSE)
        IntervalleConfiance <- MoyD + qnorm(c((1 - ConfLevel) / 2, (1 + ConfLevel) / 2)) * SdD / sqrt(N)
        Statistics[[1]] <- c(Statistics[[1]], sprintf(paste0("N=%i, ", Prec, " [", Prec, ";", Prec, "]"), N, MoyD, IntervalleConfiance[1], IntervalleConfiance[2]))
        Statistics[[2]] <- c(Statistics[[2]], "")
      } else if (ConfInter == "bootstrap") {
        MeanBoot <- function(Donnees, Indices) mean(Donnees[Indices])
        set.seed(121221)
        on.exit(set.seed(NULL), add = TRUE)
        IntervalleConfiance <- quantile(boot::boot(Difference[!is.na(Difference)], MeanBoot, R = 5000)$t[, 1], probs = c((1 - ConfLevel) / 2, (1 + ConfLevel) / 2))
        Statistics[[1]] <- c(Statistics[[1]], sprintf(paste0("N=%i, ", Prec, " [", Prec, ";", Prec, "]"), N, MoyD, IntervalleConfiance[1], IntervalleConfiance[2]))
        Statistics[[2]] <- c(Statistics[[2]], "")
      }
      if (ConfInter != "none" & Langue == "fr") {
        Labelliseurs <- c(Labelliseurs, paste0("Différence de moyennes, CI", round(100 * ConfLevel), "%"))
      } else if (ConfInter != "none" & Langue == "eng") {
        Labelliseurs <- c(Labelliseurs, paste0("Mean difference, IC", round(100 * ConfLevel), "%"))
      }
    }
    if (Test != "none") Pval <- c(MakeTest(VarQuanti, VarCroise, Test, rlang::quo_name(x), rlang::quo_name(y), ChifPval, Apparie = !is.null(Paired), IdPairs = Paired), rep("", length(Labelliseurs) - 1))
    if (SMD) {
      if (NClasses != 2) {
        stop(paste0("For variable \"", PrintVar(rlang::quo_name(x)), "\", there aren't 2 groups and thus pairwise SMDs aren't yet supported. Please set argument \"", PrintArg("SMD"), "\" to FALSE."), call. = FALSE)
      } else {
        LabelsCroisement <- names(table(VarCroise))
        TempSmd <- SmdMoy(VarQuanti[VarCroise == LabelsCroisement[1]], Poids[VarCroise == LabelsCroisement[1]],
                          VarQuanti[VarCroise == LabelsCroisement[2]], Poids[VarCroise == LabelsCroisement[2]])
        DMS <- c("", FormatPval(TempSmd, ChifPval), rep("", length(Statistics[[1]]) - 2))
      }
    }

    # Table of results
    Tableau <- data.frame(var = NomVariable,
                          eff = Labelliseurs,
                          stringsAsFactors = FALSE)
    Tableau <- suppressMessages(cbind(Tableau, dplyr::bind_cols(Statistics)))
    if (Test != "none") Tableau$pval <- Pval
    if (Grapher & is.null(Paired)) {
      message(Information("Graphs aren't supported in multivariate description for unpaired data."))
    } else if (Grapher & !is.null(Paired)) {
      Tableau$graphes <- c(list(GGHist(Difference, rep(1, length(Difference)))), rep("", nrow(Tableau) - 1))
    }
    if (SMD) {Tableau$smd <- DMS;attr(Tableau, "standardized_mean_difference") <- TempSmd}
    attr(Tableau, "crossed") <- "multivariate"

    # Names of columns
    if (is.null(NomCol)) {
      if (Langue == "fr") {
        colnames(Tableau) <- c("Variable", "Label",
                               paste0(rep("Statistiques (", NClasses), rlang::quo_name(y), "=", unique(VarCroise), ")"),
                               if (Test == "none") NULL else "PValue",
                               if (Grapher & !is.null(Paired)) "Graphes" else NULL,
                               if (SMD) "SMD" else NULL)
      } else {
        colnames(Tableau) <- c("Variable", "Label",
                               paste0(rep("Statistics (", NClasses), rlang::quo_name(y), "=", unique(VarCroise), ")"),
                               if (Test == "none") NULL else "PValue",
                               if (Grapher & !is.null(Paired)) "Graphs" else NULL,
                               if (SMD) "SMD" else NULL)
      }
    } else {
      if (length(NomCol) != ncol(Tableau)) stop(paste0("\"", PrintArg("NomCol"), "\" argument isn't of length", ncol(Tableau), " for variable \"", PrintVar(rlang::quo_name(x)), "\"."), call. = FALSE)
      colnames(Tableau) <- NomCol
    }

    # Simplify table if no missing values
    if (Simplif && sum(is.na(VarQuanti)) == 0) {
      Tableau[1, 2] <- "N"
      Tableau[1, 3:4] <- gsub("^(.+), .*$", "\\1", Tableau[1, 3:4])
    }

  }

  class(Tableau) <- c("tab_description", class(Tableau))
  attr(Tableau, "Grapher") <- Grapher & (is.null(y) | !is.null(Paired))
  return(Tableau)

}


#' @rdname TabQuanti
#' @importFrom rlang !!
NoMessTabQuanti <- function(.Data,
                            x,
                            y = NULL,
                            Prec = 0,
                            PMissing = NULL,
                            NomVariable = NULL,
                            Mode = "mediqrg",
                            Poids = NULL,
                            Test = "none",
                            SMD = FALSE,
                            Mu0 = 0,
                            ChifPval = 2,
                            NomCol = NULL,
                            Langue = "eng",
                            Grapher = FALSE,
                            Simplif = TRUE) {

  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)
  suppressMessages(Tableau <- TabQuanti(.Data = .Data,
                                        x = !!x,
                                        y = !!y,
                                        Prec = Prec,
                                        PMissing = PMissing,
                                        NomVariable = NomVariable,
                                        Mode = Mode,
                                        Poids = !!Poids,
                                        Test = Test,
                                        SMD = SMD,
                                        Mu0 = Mu0,
                                        ChifPval = ChifPval,
                                        NomCol = NomCol,
                                        Langue = Langue,
                                        Grapher = Grapher,
                                        Simplif = Simplif))
  return(Tableau)

}


#' @rdname TabQuanti
#' @importFrom rlang !!
SilentTabQuanti <- function(.Data,
                            x,
                            y = NULL,
                            Prec = 0,
                            PMissing = NULL,
                            NomVariable = NULL,
                            Mode = "mediqrg",
                            Poids = NULL,
                            Test = "none",
                            SMD = FALSE,
                            Mu0 = 0,
                            ChifPval = 2,
                            NomCol = NULL,
                            Langue = "eng",
                            Grapher = FALSE,
                            Simplif = TRUE) {

  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)
  suppressMessages(suppressWarnings(Tableau <- TabQuanti(.Data = .Data,
                                                         x = !!x,
                                                         y = !!y,
                                                         Prec = Prec,
                                                         PMissing = PMissing,
                                                         NomVariable = NomVariable,
                                                         Mode = Mode,
                                                         Poids = !!Poids,
                                                         Test = Test,
                                                         SMD = SMD,
                                                         Mu0 = Mu0,
                                                         ChifPval = ChifPval,
                                                         NomCol = NomCol,
                                                         Langue = Langue,
                                                         Grapher = Grapher,
                                                         Simplif = Simplif)))
  return(Tableau)

}
