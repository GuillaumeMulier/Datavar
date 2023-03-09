#' Title
#'
#' @param .Data
#' @param VarBinaire
#' @param NomCateg
#' @param y
#'
#' @return
#'
#' @examples
GGBar <- function(VarBinaire, VarCroise = NULL, Prec) {
  # The percent in annotation is quite unadapted and I didn't find a way to add it satisfactorily
  if (is.null(VarCroise)) {
    .Data <- data.frame(varbin = VarBinaire)
    Graphe <- ggplot2::ggplot(.Data, ggplot2::aes(x = 1, fill = factor(varbin))) +
      ggplot2::geom_bar(position = "fill", width = .1, color = "black", show.legend = FALSE) +
      # ggplot2::annotate(geom = "text", y = -.2, x = 1, label = sprintf(paste0(Prec, "%%"), 100 * mean(.Data[["varbin"]], na.rm = TRUE)), size = 5) +
      ggplot2::coord_flip(xlim = c(.8, 1.2)) +
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
