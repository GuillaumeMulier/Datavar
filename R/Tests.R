MakeTest <- function(X, Y, Test, NameX, NameY, S, Mu = 0) {

  if (Test == "chisq") {
    if (any(Y != Y[1])) { # Multivariate
      suppressWarnings(Expected <- chisq.test(table(X, Y))$expected)
      if (all(Expected >= 5)) {
        suppressWarnings(Pval <- FormatPval(chisq.test(table(X, Y), correct = FALSE)$p.value, S))
      } else if (all(Expected >= 3)) {
        suppressWarnings(Pval <- FormatPval(chisq.test(table(X, Y), correct = TRUE)$p.value, S))
      } else {
        warning(Attention(paste0("Chisq test between ", PrintVar(NameX), " and ", PrintVar(NameY), " is outside the assumptions of the test. Consider regrouping categories or using a Fisher test instead.")), immediate. = TRUE, call. = FALSE)
        suppressWarnings(Pval <- FormatPval(chisq.test(table(X, Y), correct = TRUE)$p.value, S))
      }

    } else { # Univariate
      suppressWarnings(Expected <- chisq.test(table(X))$expected)
      if (length(Mu) != length(Expected))
        stop(paste0("For one-sample test on variable\"", PrintVar(NameX), "\", the argument \"", PrintArg("Mu"), "\" should be of the same length than the number of categories in variable (or number - 1)."), call. = FALSE)
      if (all(Expected >= 5)) {
        suppressWarnings(Pval <- FormatPval(chisq.test(table(X), p = Mu, correct = FALSE)$p.value, S))
      } else if (all(Expected >= 3)) {
        suppressWarnings(Pval <- FormatPval(chisq.test(table(X), p = Mu, correct = TRUE)$p.value, S))
      } else {
        warning(Attention(paste0("One-sample Chisq test between ", PrintVar(NameX), " is outside the assumptions of the test. Consider regrouping categories or using a binomial/multinomial exact test instead.")), immediate. = TRUE, call. = FALSE)
        suppressWarnings(Pval <- FormatPval(chisq.test(table(X), p = Mu, correct = TRUE)$p.value, S))
      }

    }

  } else if (Test == "binomial") { # Only univariate
    if (length(Mu) != length(unique(X[!is.na(X)])))
      stop(paste0("For one-sample test on variable\"", PrintVar(NameX), "\", the argument \"", PrintArg("Mu"), "\" should be of the same length than the number of categories in variable (or number - 1)."), call. = FALSE)
    Pval <- FormatPval(binom.test(x = table(X)[2], n = sum(table(X)), p = Mu[2])$p.value, S)

  } else if (Test == "multinomial") { # Only univariate
    if (length(Mu) != length(unique(X[!is.na(X)])))
      stop(paste0("For one-sample test on variable\"", PrintVar(NameX), "\", the argument \"", PrintArg("Mu"), "\" should be of the same length than the number of categories in variable (or number - 1)."), call. = FALSE)
    Possibilites <- expand.grid(lapply(table(X), \(x) seq_len(sum(table(X)) + 1) - 1))
    Possibilites <- Possibilites[apply(Possibilites, 1, sum) == sum(table(X)), ]
    Possibilites$proba <- apply(Possibilites, 1, \(x) dmultinom(x, size = sum(table(X)), prob = Mu))
    ProbaObs <- dmultinom(table(X), size = sum(table(X)), prob = Mu)
    Pval <- FormatPval(sum(Possibilites$proba[Possibilites$proba <= ProbaObs]), S)

  } else if (Test == "fisher") { # Only multivariate
    Pval <- FormatPval(fisher.test(table(X, Y))$p.value, S)

  } else if (Test == "ztest") {
    if (any(Y != Y[1])) { # Multivariate
      if (any(as.numeric(table(Y)) < 30))
        warning(Attention(paste0("At least 1 group with less than 30 observations, ensure that assumptions are checked for variable \"", PrintVar(NameX), "\".")), immediate. = TRUE, call. = FALSE)
      suppressWarnings(Pval <- FormatPval(ZTest(X, Y)$p.value, S))

    } else { # Univariate
      suppressWarnings(Pval <- FormatPval(ZTest(X, Mu = Mu)$p.value, S))

    }

  } else if (Test == "student") {

    if (any(Y != Y[1])) { # Multivariate
      if (ShapiroTest(X, Y, NameX) < .05)
        message(Information(paste0("For variable \"", PrintVar(NameX), "\", the Shapiro-wilk test shows some departure from normal assumption. Student's T-test is robust to that departure and you may check with QQplot for example that it is not that bad to perform the test.")))
      if (var.test(X ~ Y)$p.value < .05) {
        Longueurs <- as.numeric(table(Y))
        if (max(Longueurs[2] / Longueurs[1], Longueurs[1] / Longueurs[2]) > 1.5) {
          warning(paste0("Non equal variances of \"", PrintVar(NameX), "\" in levels of \"", PrintVar(NameY), "\" with different sizes of samples. Consider Welch correction with \"", PrintArg("Test = studentvar"), "\"."), call. = FALSE, immediate. = TRUE)
        } else {
          message(Information(paste0("Non equal variances for variable\"", PrintVar(NameX), "\" in levels of \"", PrintVar(NameY), "\" with about the same number of subjects. T-test is robust in that condition.")))
        }
      }
      Pval <- FormatPval(t.test(X ~ Y, var.equal = TRUE)$p.value, S)

    } else { # Univariate
      if (ShapiroTest(X, NULL, NameX) < .05)
        message(Information(paste0("For variable \"", PrintVar(NameX), "\", the Shapiro-wilk test shows some departure from normal assumption. Student's T-test is robust to that departure and you may check with QQplot for example that it is not that bad to perform the test.")))
      Pval <- FormatPval(t.test(X, mu = Mu)$p.value, S)

    }

  } else if (Test == "studentvar") {
    if (ShapiroTest(X, Y, NameX) < .05)
      message(Information(paste0("For variable \"", PrintVar(NameX), "\", the Shapiro-wilk test shows some departure from normal assumption. Student's T-test is robust to that departure and you may check with QQplot for example that it is not that bad to perform the test.")))
    if (var.test(X ~ Y)$p.value >= .05)
      message(Information(paste0("For variable \"", PrintVar(NameX), "\", the F-test doesn't find significant difference between variances so classic T-test could be performed.")))
    Pval <- FormatPval(t.test(X ~ Y, var.equal = FALSE)$p.value, S)

  } else if (Test == "wilcoxon") {
    Pval <- FormatPval(wilcox.test(X ~ Y)$p.value, S)

  } else if (Test == "signed-wilcoxon") {
    if (any(Y != Y[1])) { # Multivariate
      stop("")
    } else { # Univariate
      Pval <- FormatPval(wilcox.test(X, mu = Mu)$p.value, S)

    }

  } else if (Test == "anova") {
    if (ShapiroTest(X, Y, NameX) < .05)
      message(Information(paste0("For variable \"", PrintVar(NameX), "\", the Shapiro-wilk test shows some departure from normal assumption. ANOVA is robust to that departure (unless there are very skewed distributions and very different across groups) and you may check with QQplot for example that it is not that bad to perform the test. Else consider using Kruskal-Wallis test.")))
    if (car::leveneTest(X, as.factor(Y))[1, "Pr(>F)"] < .05)
      message(Information(paste0("For variable \"", PrintVar(NameX), "\", Levene's test find non-equality of variances across groups. ANOVA is robust to this assumption if groups are of comparable size, else consider using Kruskal-Wallis test. Sample sizes are ", paste(table(Y), collapse = "/"), ".")))
    Pval <- FormatPval(summary(aov(X ~ as.factor(Y)))[[1]][1, "Pr(>F)"], S)

  } else if (Test == "kruskal-wallis") {
    Pval <- FormatPval(kruskal.test(X ~ Y)$p.value, S)

  } else {
    stop("Safety stop.")

  }

  return(Pval)
}


#' Z-test
#'
#' Perform one- and two-samples Z-tests.
#'
#' @param VarQuanti Numeric vector of data.
#' @param VarY Either numeric vector of paired data with VarQuanti or binary variable indicating the groups to compare.
#' @param Paired TRUE for paired Z-test.
#' @param Alternative Character string for alternative hypothesis ("two.sided", "less", "greater").
#' @param Mu A numeric indicating the value to which we want to compare the mean in one-sample test or difference of means in two-sample test.
#' @param ConfLevel Confidence level of the interval of the difference.
#'
#' @return
#' A list of class "htest" containing the following components :
#' \describe{
#'   \item{statistic}{The value of Z-statistic.}
#'   \item{p.value}{PValue of the test.}
#'   \item{conf.int}{Confidence interval of the difference of means.}
#'   \item{estimate}{Estimated mean or difference in means.}
#'   \item{null.value}{The value to which we want to compare the mean in one-sample test or difference of means in two-sample test.}
#'   \item{stderr}{Standard error of the difference in means.}
#'   \item{alternative}{Alternative hypothesis.}
#'   \item{method}{Type of Z-test.}
#'   \item{data.name}{Name(s) of the data.}
#' }
#'
#' @export
#'
#' @examples
#' ZTest(mtcars$mpg, mtcars$vs)
ZTest <- function(VarQuanti, VarY = NULL, Paired = FALSE,
                  Alternative = c("two.sided", "less", "greater"),
                  Mu = 0, ConfLevel = .95) {

  # Verifications
  Alternative <- match.arg(Alternative)
  if (!missing(Mu) && (length(Mu) != 1 || is.na(Mu)))
    stop("Argument 'Mu' must be a single number.", call. = FALSE)
  if (!missing(ConfLevel) && (length(ConfLevel) != 1 || !is.finite(ConfLevel) || ConfLevel < 0 || ConfLevel > 1))
    stop("Argument 'ConfLevel' must be a single number between 0 and 1.", call. = FALSE)
  if (!is.null(VarY) & length(unique(VarY[!is.na(VarY)])) != 2)
    stop("'VarY' must have 2 categories to perform Z-test.", call. = FALSE)

  # Reconstruct the different quantities used in formula
  if (!is.null(VarY)) {
    # 2 variables specified
    dname <- paste(deparse(substitute(VarQuanti)), "and", deparse(substitute(VarY)))
    if (Paired) {
      GardX <- GardY <- complete.cases(VarQuanti, VarY)
    } else {
      GardX <- !is.na(VarQuanti) & !is.na(VarY)
    }
    VarY <- VarY[GardX]
  } else {
    # Only 1 variable
    dname <- deparse(substitute(VarQuanti))
    if (Paired)
      stop("'VarY' is missing for paired test")
    GardX <- !is.na(VarQuanti)
  }
  VarQuanti <- VarQuanti[GardX]
  if (Paired) {
    VarQuanti <- VarQuanti - VarY
    VarY <- NULL
  }
  if (!is.null(VarY)) {
    N <- tapply(VarQuanti, VarY, length)
    M <- tapply(VarQuanti, VarY, mean)
    S2 <- tapply(VarQuanti, VarY, var)
  } else {
    N <- length(VarQuanti)
    M <- mean(VarQuanti)
    S2 <- var(VarQuanti)
  }

  # Compute test statistics
  if (is.null(VarY)) {
    if (N < 30)
      warning(paste0("Less than 30 observations, ensure that assumptions are checked for variable \"", PrintVar(dname), "\"."), call. = FALSE, immediate. = TRUE)
    S <- sqrt(S2 / N)
    if (S < 10 * .Machine$double.eps * abs(M))
      stop("Data are essentially constant.", call. = FALSE)
    ZStat <- (M - Mu) / S
    Methode <- if (Paired) "Paired Z-test" else "One sample Z-test"
    Estimation <- setNames(M, if (Paired) "mean difference" else "mean of x")
  } else {
    if (any(N < 30))
      warning(paste0("At least 1 group with less than 30 observations, ensure that assumptions are checked for variable \"", PrintVar(gsub("^(.*) and .*$", "\\1", dname)), "\"."), call. = FALSE, immediate. = TRUE)
    Methode <- "Two sample Z-test"
    S <- sqrt(sum(S2 / N))
    Estimation <- M
    names(Estimation) <- paste0("mean of ", names(Estimation))
    if (any(S2 < 10 * .Machine$double.eps * max(abs(M))))
      stop("Data are essentially constant.")
    ZStat <- (-diff(M) - Mu) / S
  }

  # PValue and confidence intervals with normal distribution
  if (Alternative == "less") {
    PVal <- pnorm(ZStat)
    InterConf <- c(-Inf, ZStat + qnorm(ConfLevel))
  } else if (Alternative == "greater") {
    PVal <- pnorm(ZStat, lower.tail = FALSE)
    InterConf <- c(ZStat - qnorm(ConfLevel), Inf)
  } else {
    PVal <- 2 * pnorm(-abs(ZStat))
    InterConf <- ZStat + qnorm(c((1 - ConfLevel) / 2, (1 + ConfLevel) / 2))
  }
  InterConf <- Mu + InterConf * S
  names(ZStat) <- "Z"
  names(Mu) <- if (Paired) "mean difference" else if (!is.null(VarY)) "difference in means" else "mean"
  attr(InterConf, "conf.level") <- ConfLevel
  rval <- list(statistic = ZStat, p.value = PVal, parameter = NULL,
               conf.int = InterConf, estimate = Estimation, null.value = Mu,
               stderr = S, alternative = Alternative, method = Methode,
               data.name = dname)
  class(rval) <- "htest"

  return(rval)

}


#' Wrapper function because of the error of shapiro.test with more than 5000 observations.
#'
#' @param x Variable to test normality.
#' @param y Independant variable if there is one, else let NULL
#' @param NameX Name of variable.
#'
#' @return Pvalue
ShapiroTest <- function(x, y = NULL, NameX) {

  if (is.null(y)) {
    PetitP <- tryCatch(shapiro.test(x)$p.value,
                       error = function(e) {
                         message(Information(paste0("For variable \"", PrintVar(NameX), "\", there is more than 5000 or less than 3 rows / all identical values. The Shapiro test cannot be performed, and you can inspect graphically with QQplot your data. In case of very large sample size, consider using a Z-test.")))
                         return(1)
                       })

  } else {
    PetitP <- min(tapply(x, y, ShapiroTest, NameX = NameX))
  }

  return(PetitP)

}

