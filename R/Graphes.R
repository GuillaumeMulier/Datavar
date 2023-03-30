#' Bar chart for categorical variables
#'
#' @param VarCroise Crossing variable.
#' @param Prec Number of digits for percentages (not displayed now by choice).
#' @param VarBinaire Variable to plot.
#'
#' @return SIngle bar plot.
#'
GGBar <- function(VarBinaire, VarCroise = NULL, Prec) {
  # The percent in annotation is quite unadapted and I didn't find a way to add it satisfactorily
  if (is.null(VarCroise)) {
    .Data <- data.frame(varbin = VarBinaire)
    Graphe <- ggplot2::ggplot(.Data, ggplot2::aes(x = 1, fill = factor(varbin))) +
      ggplot2::geom_bar(position = "fill", width = .1, color = "black", show.legend = FALSE) +
      # ggplot2::annotate(geom = "text", y = -.2, x = 1, label = sprintf(paste0(Prec, "%%"), 100 * mean(.Data[["varbin"]], na.rm = TRUE)), size = 5) +
      ggplot2::coord_flip(xlim = c(.85, 1.15)) +
      ggplot2::scale_fill_discrete(type = c("#FFFFFF", "#777777")) +
      ggplot2::theme_void()
  } else {
    .Data <- data.frame(varbin = VarBinaire, varcrois = as.numeric(VarCroise == unique(VarCroise[1])))
    Graphe <- ggplot2::ggplot(.Data, ggplot2::aes(x = varcrois, fill = factor(varbin))) +
      ggplot2::geom_bar(position = "fill", width = .3, color = "black", show.legend = FALSE) +
      # ggplot2::annotate(geom = "text", y = -.2, x = 0:1,
                        # label = sprintf(paste0(Prec, "%%"), 100 * tapply(.Data[["varbin"]], .Data[["varcrois"]], \(x) mean(x, na.rm = TRUE))), size = 5) +
      ggplot2::coord_flip(xlim = c(-.2, 1.2)) +
      ggplot2::scale_fill_discrete(type = c("#FFFFFF", "#777777")) +
      ggplot2::theme_void()
  }

  return(Graphe)
}



#' Histogramm for continuous variables
#'
#' @param VarQuanti Continuous variables.
#' @param Bins Number of bins
#'
#' @return Histogramm plot.
#'
GGHist <- function(VarQuanti, Bins = 10) {
    Graphe <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = VarQuanti)) +
      ggplot2::geom_histogram(color = "white", fill = "#333333", bins = Bins, size = 2) +
      ggplot2::theme_void()
  return(Graphe)
}


#' Blank plot for rendering
#'
#' @return Blank plot.
#'
GGBlank <- function() {
  Graphe <- ggplot2::ggplot(data = NULL) +
    ggplot2::annotate("point", x = 1:5, y = 1:5, color = "transparent") +
    ggplot2::theme_void()
  return(Graphe)
}

