#' tab_binaire
#'
#' Description of binary variables
#'
#' A function to describe binary variables with count and percentage (+ binomial confidence interval if supplied).
#'
#' You can also use it to cross it with a categorial variable and perform a comparison test (chisq or fisher test).
#'
#' @param .Data The dataset that contains the variables.
#' @param x The binary variable to describe.
#' @param y The qualitative variable to perform the bivariate description. If unspecified, univariate description.
#' @param Prec Number of decimals for the percentages.
#' @param ConfInter Type of confidence interval (from normal, exact = Clopper-Pearson, and Jeffreys). None if no confidence interval is wanted.
#' @param ConfLevel Level of confidence for confidence intervals (by default 95%).
#' @param Test String giving the name of the comparison test performed.
#' @param Langue "fr" for french and "eng" for english. For the display in the table.
#' @param NomCol Vector of strings to name each column of the output. Automatic display if unspecified.
#' @param PMissing Number of decimals of percentage from whole dataset. NULL (default value) if no percent desired.
#' @param Grapher Boolean if you want to graph the distribution in univariate case.
#' @param ChifPval Number of decimal for PValue.
#' @param NomLabel String giving the name of the class that you want to display in the table.
#' @param NomCateg The value of the category which you want to display in the table.
#'
#' @export
#'
#' @seealso \code{\link{descr}}
#'
#' @examples
#' TabBinaire(mtcars, am)
#' TabBinaire(mtcars, am, vs)
#' TabBinaire(mtcars, am, vs,
#'            NomCateg = "0", NomLabel = "Automatic transmission",
#'            Test = "fisher")
TabBinaire <- function(.Data,
                       x,
                       y = NULL,
                       Prec = 0,
                       ConfInter = c("none", "normal", "exact", "jeffreys"),
                       ConfLevel = .95,
                       PMissing = NULL,
                       Grapher = FALSE,
                       ChifPval = 2,
                       Test = "none",
                       Langue = "eng",
                       NomCol = NULL,
                       NomCateg = NULL,
                       NomLabel = NULL) {

  # Interest variables defused so that it is possible to give unquoted arguments
  x <- rlang::enexpr(x)
  VarBinaire <- rlang::eval_tidy(x, data = .Data)
  y <- rlang::enexpr(y)

  # Verifications
  Langue <- VerifArgs(Langue)
  Prec <- VerifArgs(Prec)
  NomCateg <- VerifArgs(NomCateg, NomLabel, VarBinaire, x)
  VarBinaire <- VerifArgs(VarBinaire, NomCateg, x)
  NomLabel <- VerifArgs(NomLabel, VarBinaire, x)
  ConfInter <- VerifArgs(ConfInter)
  ConfLevel <- VerifArgs(ConfLevel)

  if (is.null(y)) { # Univariate description

    # Store statistics
    X <- sum(VarBinaire)
    N <- sum(!is.na(VarBinaire))
    M <- if (is.null(PMissing)) paste0(sum(is.na(VarBinaire))) else sprintf(paste0("%i(%.", PMissing, "f%%)"), sum(is.na(VarBinaire)), sum(is.na(VarBinaire)) / length(VarBinaire))
    Pourcent <- list(fmt = if (ConfInter == "none") paste0("%i/%i (", Prec, "%%)") else paste0("%i/%i (", Prec, "%%[", Prec, ";", Prec, "])"),
                     X,
                     N,
                     100 * X / N)
    if (ConfInter == "normal") {
      Pourcent <- append(Pourcent,
                         lapply(c((1 - ConfLevel) / 2, (1 + ConfLevel) / 2), qnorm,
                                mean = X / N,
                                sd = sqrt((X / N) * (1 - X / N) / N)))
      Pourcent[[5]] <- 100 * max(Pourcent[[4]], 0)
      Pourcent[[6]] <- 100 * min(Pourcent[[5]], 1)
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
    Tableau <- data.frame(var = NomLabel,
                          eff = c("n, %", ifelse(Langue == "fr", "Manquants", ifelse(Langue == "eng", "Missings", "..."))),
                          stats = c(Pourcent, M),
                          stringsAsFactors = FALSE)
    if (Grapher) Tableau$graphes <- list(GGBar(VarBinaire, NULL, Prec), "")

    # Name of columns
    if (is.null(NomCol)) {
      if (Langue == "fr") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistiques", "Graphes") else c("Variable", "Label", "Statistiques")}
      else if (Langue == "eng") {colnames(Tableau) <- if (Grapher) c("Variable", "Label", "Statistics", "Graphs") else c("Variable", "Label", "Statistics")}
    } else {
      if (length(NomCol) != ncol(Tableau)) stop(paste0("\"NomCol\" argument isn't of length", ncol(Tableau), "."), call. = FALSE)
      colnames(Tableau) <- NomCol
    }

  } else { # Crossed description

    VarCroise <- rlang::eval_tidy(y, data = .Data)
    VarBinaire <- VarBinaire[!is.na(VarCroise)]
    VarCroise <- VarCroise[!is.na(VarCroise)]
    NClasses <- length(unique(VarCroise))

    # Verifications on statistical test
    Test <- VerifTest(Test, "binaire", NClasses, VarBinaire, y, x)
    ChifPval <- VerifArgs(ChifPval)

    # Statistics
    X <- tapply(VarBinaire, VarCroise, sum, na.rm = TRUE)
    N <- tapply(VarBinaire, VarCroise, \(x) sum(!is.na(x), na.rm = TRUE))
    M <- if (is.null(PMissing)) {
      tapply(VarBinaire, VarCroise, \(x) paste0(sum(is.na(x), na.rm = TRUE)))
    } else {
      tapply(VarBinaire, VarCroise, \(x) sprintf(paste0("%i(%.", PMissing, "f%%)"), sum(is.na(x)), sum(is.na(x)) / length(x)))
    }
    Pourcent <- list(fmt = if (ConfInter == "none") paste0("%i/%i (", Prec, "%%)") else paste0("%i/%i (", Prec, "%%[", Prec, ";", Prec, "])"),
                     X,
                     N,
                     100 * X / N)
    if (ConfInter == "normal") {
      Pourcent <- append(Pourcent,
                         lapply(c((1 - ConfLevel) / 2, (1 + ConfLevel) / 2), qnorm,
                                mean = X / N,
                                sd = sqrt((X / N) * (1 - X / N) / N)))
      Pourcent[[4]] <- 100 * pmax(Pourcent[[4]], 0)
      Pourcent[[5]] <- 100 * pmin(Pourcent[[5]], 1)
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

    # Table of results
    Tableau <- data.frame(var = NomLabel,
                          eff = c("n, %", ifelse(Langue == "fr", "Manquants", ifelse(Langue == "eng", "Missings", "..."))),
                          stringsAsFactors = FALSE)
    Tableau <- cbind(Tableau, matrix(c(Pourcent, M), nrow = 2, byrow = TRUE))
    if (Test != "none") Tableau$pval <- Pval
    if (Grapher) Tableau$graphes <- list(GGBar(VarBinaire, VarCroise, Prec), "")

    # Names of columns
    if (is.null(NomCol)) {
      if (Langue == "fr") {
        colnames(Tableau) <- if (Grapher) {
          c("Variable", "Label",
            paste0(rep("Statistiques (", NClasses), rlang::quo_name(y), "=", unique(VarCroise), ")"),
            if (Test == "none") NULL else "PValue",
            paste0("Graphes_", seq_len(NClasses)))
        } else {
          c("Variable", "Label",
            paste0(rep("Statistiques (", NClasses), rlang::quo_name(y), "=", unique(VarCroise), ")"),
            if (Test == "none") NULL else "PValue")
        }
      } else {
        colnames(Tableau) <- if (Grapher) {
          c("Variable", "Label",
            paste0(rep("Statistics (", NClasses), rlang::quo_name(y), "=", unique(VarCroise), ")"),
            if (Test == "none") NULL else "PValue",
            paste0("Graphs_", seq_len(NClasses)))
        } else {
          c("Variable", "Label",
            paste0(rep("Statistics (", NClasses), rlang::quo_name(y), "=", unique(VarCroise), ")"),
            if (Test == "none") NULL else "PValue")
        }
      }
    } else {
      if (length(NomCol) != ncol(Tableau)) stop(paste0("\"NomCol\" argument isn't of length", ncol(Tableau), " for variable \"", rlang::quo_name(x), "\"."), call. = FALSE)
      colnames(Tableau) <- NomCol
    }

  }

  if (sum(is.na(VarBinaire)) == 0) Tableau <- Tableau[- nrow(Tableau), ]

  return(Tableau)

}




Tableau %>%
  as_grouped_data(groups = "Variable") %>%
  mutate(across(Label, ~ ifelse(row_number() == 1, Variable, .x))) %>%
  select(-1) %>%
  flextable() %>%
  mk_par(j = c(4), value = as_paragraph(gg_chunk(value = ., height = .5, width = 1)),
         use_dot = TRUE) %>%
  merge_at(i = 1, j = 1:4) %>%
  padding(padding.top = 1, padding.bottom = 1)




