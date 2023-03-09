 MakeTest <- function(X, Y, Test, NameX, NameY, S) {
   if (Test == "chisq") {
     suppressWarnings(Expected <- chisq.test(table(X, Y))$expected)
     if (all(Expected >= 5)) {
       suppressWarnings(Pval <- FormatPval(chisq.test(table(X, Y), correct = FALSE)$p.value, S))
     } else if (all(Expected >= 3)) {
       suppressWarnings(Pval <- FormatPval(chisq.test(table(X, Y), correct = TRUE)$p.value, S))
     } else {
       warning(paste0("Chisq test between ", NameX, " and ", NameY, " is outside the assumptions of the test.\nConsider regrouping categories or using a Fisher test instead.", call. = FALSE))
       suppressWarnings(Pval <- FormatPval(chisq.test(table(X, Y), correct = TRUE)$p.value, S))
     }
   } else if (Test == "fisher") {
     Pval <- FormatPval(fisher.test(table(X, Y))$p.value, S)
   }
   return(Pval)
 }





 ZTest <- function(VarQuanti, VarY = NULL, Paired = FALSE,
                    Alternative = c("two.sided", "less", "greater"),
                    Mu = 0, ConfLevel = .95) {

   # Verifications
   Alternative <- match.arg(Alternative)
   if (!missing(Mu) && (length(Mu) != 1 || is.na(Mu)))
     stop("Argument 'Mu' must be a single number.", call. = FALSE)
   if (!missing(ConfLevel) && (length(ConfLevel) != 1 || !is.finite(ConfLevel) || ConfLevel < 0 || ConfLevel > 1))
     stop("Argument 'ConfLevel' must be a single number between 0 and 1.", call. = FALSE)
   if (length(unique(VarY[!is.na(VarY)])) != 2)
     stop("'VarY' must have 2 categories to perform Z-test.", call. = FALSE)

   # Reconstruct the different quantities used in formula
   if (!is.null(VarY)) {
     # 2 variables specified
     dname <- paste(deparse(substitute(VarQuanti)), "and", deparse(substitute(VarY)))
     if (Paired) {
       GardX <- GardY <- complete.cases(VarQuanti, VarY)
     } else {
       GardX <- !is.na(VarQuanti)
       GardY <- !is.na(VarY)
     }
     VarY <- VarY[GardY]
   } else {
     # Only 1 variable
     dname <- deparse(substitute(VarQuanti))
     if (Paired)
       stop("'VarY' is missing for paired test")
     GardX <- !is.na(GardX)
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
       warning("Less than 30 observations, ensure that assumptions are checked.", call. = FALSE, immediate. = TRUE)
     S <- sqrt(S2 / N)
     if (S < 10 * .Machine$double.eps * abs(M))
       stop("Data are essentially constant.", call. = FALSE)
     ZStat <- (M - Mu) / S
     Methode <- if (paired) "Paired Z-test" else "One sample Z-test"
     Estimation <- setNames(M, if (Paired) "mean difference" else "mean of x")
   } else {
     if (any(N < 30))
       warning("At least 1 group with less than 30 observations, ensure that assumptions are checked.", call. = FALSE, immediate. = TRUE)
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

