#' Inverse of %in%
'%nin%' <- function(x, y) {match(x, y, nomatch = 0L) <= 0L}


#' Formatting PValue
FormatPval <- function(PVal, S) if (PVal < 10 ** (-S)) paste0("<0.", paste(rep("0", S - 1), collapse = ""), "1") else sprintf(paste0("%.", S, "f"), PVal)


#' Get the 1st quartile
Q1 <- function(x, w = rep(1, length(x))) as.numeric(WeightedQuantile(x, w, q = 0.25))


#' Get the 3rd quartile
Q3 <- function(x, w = rep(1, length(x))) as.numeric(WeightedQuantile(x, w, q = 0.75))


#' Get IQR
IQR <- function(x, w = rep(1, length(x)), Prec) sprintf(paste0("(", Prec, "-", Prec, ")"), Q1(x, w), Q3(x, w))


#' Get range
RangeVar <- function(x, w, Prec, na.rm = TRUE) sprintf(paste0("[", Prec, "-", Prec, "]"),
                                                    range(x, na.rm = na.rm)[1], range(x, na.rm = na.rm)[2])


#' Get mean
MeanVar <- function(x, w, Prec, na.rm = TRUE) sprintf(Prec, WeightedMean(x, w, na.rm = na.rm))


#' Get standard deviation
SdVar <- function(x, w, Prec, na.rm = TRUE) sprintf(Prec, sqrt(WeightedVar(x, w, na.rm = na.rm, Corr = 1)))


#' Get median
MedianVar <- function(x, w, Prec) sprintf(Prec, WeightedQuantile(x, w, q = .5))


#' Get N
GetN <- function(x, w, Prec, na.rm = TRUE) sprintf(Prec, sum(as.numeric(!is.na(x)) * w, na.rm = na.rm))


#' Get Missing
GetM <- function(x, w, Prec, na.rm = TRUE) if (grepl("\\(", Prec)) sprintf(Prec, sum(as.numeric(is.na(x)) * w, na.rm = na.rm), 100 * sum(as.numeric(is.na(x)) * w, na.rm = na.rm) / length(x)) else sprintf(Prec, sum(as.numeric(is.na(x)) * w, na.rm = na.rm))


#' Weighted Mean
WeightedMean <- function(x, w, na.rm = TRUE) sum(x * w, na.rm = na.rm) / sum(w[!is.na(x)])


#' Weighted variance
#' @references https://www.gnu.org/software/gsl/doc/html/statistics.html#weighted-samples
#' https://www.analyticalgroup.com/download/weighted_mean.pdf
WeightedVar <- function(x, w, na.rm = TRUE, Corr = 1) {
  if (na.rm) {w <- w[!is.na(x)];x <- x[!is.na(x)]}
  M <- WeightedMean(x, w, na.rm = na.rm)
  Denom <- sum(w, na.rm = na.rm)
  return(sum(w * (x - M) ** 2, na.rm = na.rm) / (Denom - Corr))
}


#' Weighted quantiles
#' @references https://en.wikipedia.org/wiki/Weighted_median
WeightedQuantile <- function(x, w = rep(1, length(x)), q = .5) {

  if (any(w != 1)) {
    # Sort values and weights
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
    OrdreX <- order(x)
    w <- w[order(x)]
    x <- x[order(x)]
    sw <- sum(w)
    n <- length(x) - 1

    Resultat <- rcpp_QuantileSearch(x, w, q, sw, n)
  } else {
    Resultat <- quantile(x, probs = q, na.rm = TRUE)
  }

  return(Resultat)

}

#' Newcombe square and add confidence interval
#' @references Fagerland MW, Lydersen S, Laake P. Recommended tests and confidence intervals for paired binomial proportions. Stat Med. 2014 Jul 20;33(16):2850-75. doi: 10.1002/sim.6148.
MoverWilsonCI <- function(Tab, Alpha = .05) {
  if (any(dim(Tab) != c(2, 2))) {
    stop("Il y a plus de 2 modalités")
  }
  N <- as.numeric(sum(Tab))
  Delta <- (Tab[1, 2] - Tab[2, 1]) / N
  N1p <- as.numeric(Tab[1, 1] + Tab[1, 2])
  N2p <- as.numeric(Tab[2, 1] + Tab[2, 2])
  Np1 <- as.numeric(Tab[1, 1] + Tab[2, 1])
  Np2 <- as.numeric(Tab[1, 2] + Tab[2, 2])
  ValCrit <- qnorm(1 - Alpha / 2)
  WilsonP1p <- (2 * N1p + ValCrit ** 2 + c(-1, 1) * ValCrit * sqrt(ValCrit ** 2 + 4 * N1p * (1 - N1p / N))) / (2 * (N + ValCrit ** 2))
  WilsonPp1 <- (2 * Np1 + ValCrit ** 2 + c(-1, 1) * ValCrit * sqrt(ValCrit ** 2 + 4 * Np1 * (1 - Np1 / N))) / (2 * (N + ValCrit ** 2))
  A <- Tab[1, 1] * Tab[2, 2] - Tab[1, 2] * Tab[2, 1]
  if (A < 0) {
    Phi <- A / sqrt(N1p * N2p * Np1 * Np2)
  } else if (A <= (N / 2)) {
    Phi <- 0
  } else {
    Phi <- (A - N / 2) / sqrt(N1p * N2p * Np1 * Np2)
  }
  Lower <- Delta - sqrt((N1p / N - WilsonP1p[1]) ** 2 + (WilsonPp1[2] - Np1 / N) ** 2 - 2 * Phi * (N1p / N - WilsonP1p[1]) * (WilsonPp1[2] - Np1 / N))
  Upper <- Delta + sqrt((Np1 / N - WilsonPp1[1]) ** 2 + (WilsonP1p[2] - N1p / N) ** 2 - 2 * Phi * (Np1 / N - WilsonPp1[1]) * (WilsonP1p[2] - N1p / N))
  return(c("N" = N, "delta" = -Delta, "lower" = -Upper, "upper" = -Lower))
}


#' Print variable in message
PrintVar <- function(x) cli::combine_ansi_styles(cli::style_underline, cli::bg_br_red, cli::col_black)(x)
PrintArg <- function(x) cli::combine_ansi_styles(cli::style_italic, cli::bg_br_cyan, cli::col_black)(x)


#' Borders and formatting for flextable
LargeBorder <- officer::fp_border(color = "#474747", style = "solid", width = 2.5)
SlimBorder <- officer::fp_border(color = "#474747", style = "solid", width = 1.25)


#' Information message
BgInfo <- cli::make_ansi_style("#ddc03d", bg = TRUE)
ColInfo <- cli::make_ansi_style("#1346e7")
ColInfoTxt <- cli::col_br_blue
Information <- function(x) paste0(cli::style_underline(BgInfo(ColInfo("Info:"))), " ", ColInfoTxt(x))


#' Warning message
BgWarn <- cli::make_ansi_style("#7fccc9", bg = TRUE)
ColWarn <- cli::make_ansi_style("#8d2885")
ColWarnTxt <- cli::make_ansi_style("#d56354")
Attention <- function(x) paste0(cli::style_underline(BgWarn(ColWarn("Careful:"))), " ", ColWarnTxt(x))


#' Warning for deprecated function
Deprecated <- function() warning("This function is deprecated and is here to let old scripts run. Consider using functions Description(), ... instead.", call. = FALSE, immediate. = TRUE)


# ------------------------------------------------ #
# Then, older functions for old version of package #
# ------------------------------------------------ #


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
  if (!is.numeric(x$prec) || !is.numeric(x$chif_pval)) stop("Columns \"prec\" and \"chif_pval\" should be numerics.", call. = FALSE)

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


