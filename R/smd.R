#' SMD proportions
#'
#' @param Vec1,Poids1 Values and weights of categorical variable in group 1
#' @param Vec2,Poids2 Values and weights of categorical variable in group 2
#'
#' @return SMD for categorical variable between the 2 groups
SmdProp <- function(Vec1, Poids1, Vec2, Poids2) {
  T1 <- tapply(Poids1, Vec1, sum, na.rm = TRUE) / sum(Poids1, na.rm = TRUE)
  T2 <- tapply(Poids2, Vec2, sum, na.rm = TRUE) / sum(Poids2, na.rm = TRUE)
  T1[is.na(T1)] <- 0
  T2[is.na(T2)] <- 0
  S <- (outer(T1[-1], T1[-1], "*") + outer(T2[-1], T2[-1], "*")) / 2
  S[matrix(c(rep(seq_len(nrow(S)), 2)), ncol = 2)] <- ((outer(T1[-1], 1 - T1[-1], "*") + outer(T2[-1], 1 - T2[-1], "*")) / 2)[matrix(c(rep(seq_len(nrow(S)), 2)), ncol = 2)]
  return(as.numeric(sqrt((T1[-1] - T2[-1]) %*% solve(S) %*% (T1[-1] - T2[-1]))))
}


#' SMD means
#'
#' @param Vec1,Poids1 Values and weights of quantitative variable in group 1
#' @param Vec2,Poids2 Values and weights of quantitative variable in group 2
#'
#' @return SMD for quantitative variable between 2 groups
SmdMoy <- function(Vec1, Poids1, Vec2, Poids2) {
  return(abs(WeightedMean(Vec1, Poids1) - WeightedMean(Vec2, Poids2)) / sqrt((WeightedVar(Vec1, Poids1) + WeightedVar(Vec2, Poids2)) / 2))
}


#' Extract SMDs
#'
#' @param TabDescription tab_description object created by function `Description()`.
#'
#' @return
#' Data.frame of SMDs for all variable in TabDescription object.
#'
#' @export
#'
#' @examples
#' TabDesc <- Description(mtcars, y = am, .Datavar = DatavarMtcars, SMD = TRUE)
#' ExtraireSMDs(TabDesc)
ExtraireSMDs <- function(TabDescription) {
  if (!inherits(TabDescription, "tab_description"))
    stop("Provide a tab_description object created with function Description().", call. = FALSE)
  return(attr(TabDescription, "smds"))
}
