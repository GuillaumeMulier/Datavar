
#' Print description table in console
#'
#' @param x The table of description
#'
#' @export
#'
#' @examples
#' TabBinaire(mtcars, am, Grapher = TRUE)
print.tab_description <- function(x, ...) {

  if (attr(x, "Grapher")) {
    if (attr(x, "crossed") == "univariate") {
      colonne <- ncol(x)
      x[[colonne]] <- NULL
      x2 <- as.data.frame(x)
    }
  } else {
    x2 <- as.data.frame(x)
  }
  x2 <- split(x2, x2[, 1])
  purrr::walk(names(x2),
              \(var) {
                var_modif <- gsub("\\&|\\*|;|~", "", var)
                cat(PrintVar(paste0(var_modif, " :\n")))
                print.data.frame(x2[[var]][, -1], row.names = FALSE)
                cat(cli::col_br_red("----------------------------------------"), "\n")
              })

  invisible(x)

}



#' Flextable personal theme
#'
#' @param tab Flextable objet.
#' @param BaseSize Size of police.
#'
#' @return A flextable object.
#' @export
#'
ThemePerso <- function(tab, BaseSize = 10) {

  if (!inherits(tab, "flextable"))
    stop("Function \"ThemePerso()\" should be used on flextable object.", call. = FALSE)

  # Borders
  tab <- flextable::border_remove(tab)
  tab <- flextable::border(tab, part = "header", border = LargeBorder)
  tab <- flextable::border_outer(tab, part = "body", border = LargeBorder)
  tab <- flextable::border_outer(tab, part = "foot", border = LargeBorder)

  # Font
  if ("Lato" %in% sysfonts::font_families())
    tab <- flextable::font(tab, part = "all", fontname = "Lato")
  tab <- flextable::fontsize(tab, part = "header", size = BaseSize * 1.3)
  tab <- flextable::fontsize(tab, part = "body", size = BaseSize * 1.1)
  tab <- flextable::fontsize(tab, part = "footer", size = BaseSize)
  tab <- flextable::bold(tab, part = "header")
  tab <- flextable::color(tab, part = "header", color = "white")

  # Alignment
  Types <- sapply(tab$body$dataset, class)
  TypesNum <- names(Types)[Types %in% c("numeric", "integer")]
  TypesChar <- names(Types)[!Types %in% c("numeric", "integer")]
  tab <- flextable::align(tab, part = "header", align = "center")
  if (length(TypesNum)) tab <- flextable::align(tab, j = TypesNum, part = "body", align = "right")
  if (length(TypesChar)) tab <- flextable::align(tab, j = TypesChar, part = "body", align = "left")

  # Background
  tab <- flextable::bg(tab, part = "all", bg = "transparent")
  tab <- flextable::bg(tab, part = "header", bg = "#474747")
  tab <- flextable::bg(tab, i = seq(1, flextable::nrow_part(tab, part = "body"), 2), part = "body", bg = "#D5D5D5")

  # Padding
  tab <- flextable::padding(tab, part = "header", padding.top = 4, padding.bottom = 4, padding.right = 2, padding.left = 2)
  tab <- flextable::padding(tab, part = "body", padding.top = 2, padding.bottom = 2, padding.right = 2, padding.left = 2)
  tab <- flextable::padding(tab, part = "foot", padding.top = 3, padding.bottom = 1, padding.right = 2, padding.left = 2)

  return(tab)

}

#' Flextable theme for description table
#'
#' @param tab Flextable object.
#' @param BaseSize Size of police.
#' @param ColNum Index of numerical columns.
#' @param Separations Index of columns we want separator.
#'
#' @return A flextable object (first column and headers can be written in markdown like synthax).
#' @export
#'
ThemeDescription <- function(tab, BaseSize = 10, ColNum = NULL, Separations = NULL) {

  tab <- ThemePerso(tab, BaseSize)
  tab <- flextable::border(tab, j = 1, border.right = SlimBorder)
  tab <- ftExtra::colformat_md(tab, j = 1, part = "body")
  tab <- ftExtra::colformat_md(tab, part = "header")
  if (!is.null(ColNum)) tab <- flextable::align(tab, part = "body", j = ColNum, align = "right")
  if (!is.null(Separations)) tab <- flextable::border(tab, part = "body", j = Separations, border.right = SlimBorder)
  tab <- flextable::fix_border_issues(tab)

  return(tab)

}


#' Display results of description
#'
#' Create a flextable object to display the result of a tab_description object.
#'
#' @param TabDescription  tab_description object.
#' @param Widths The widths of the displayed columns.
#' @param BaseSize Size of police.
#'
#' @return A flextable object.
#' @export
#'
#' @examples
#' # Create the datavar object
#' DatavarVoitures <- CreateDatavar(mtcars)
#' # The description of the whole mtcars dataset
#' TabDescr <- Description(mtcars, .Datavar = DatavarVoitures, Langue = "eng", Grapher = TRUE)
#' # Display as flextable
#' FlexTabDescr(TabDescr)
FlexTabDescr <- function(TabDescription, Widths = NULL, BaseSize = 10) {

  stopifnot(is.null(Widths) | length(Widths) == (ncol(TabDescription) - 1))
  if (!inherits(TabDescription, "tab_description") & !inherits(TabDescription, "tab_datavar"))
    stop("You should supply to the function an object of type tab_description or tab_datavar.", call. = FALSE)
  if (is.null(Widths)) {
    if (attr(TabDescription, "crossed") == "univariate") {
      Widths <- if (attr(TabDescription, "Grapher")) c(2.5, 1.5, if (attr(TabDescription, "Comparer")) .5 else NULL, 1.2) else c(2.5, 1.5, if (attr(TabDescription, "Comparer")) .5 else NULL)
    } else {
      if (attr(TabDescription, "difference_moy_stand")) {
        Widths <- if (attr(TabDescription, "Comparer")) c(2.5, rep(1.5, ncol(TabDescription) - 4), .5, .5) else c(2.5, rep(1.5, ncol(TabDescription) - 3), .5)
      } else {
        Widths <- if (attr(TabDescription, "Comparer")) c(2.5, rep(1.5, ncol(TabDescription) - 3), .5) else c(2.5, rep(1.5, ncol(TabDescription) - 2))
      }
    }
  }
  TabLabels <- attr(TabDescription, "tab_lab")

  ResDescription <- TabDescription |>
    flextable::as_grouped_data(groups = colnames(TabDescription)[1]) |>
    tidyr::fill(1, .direction = "down")
  LignesFusion <- which(is.na(ResDescription[, 2]))
  ResDescription[LignesFusion, 2] <- ResDescription[LignesFusion, 1]
  ResDescription <- ResDescription[, -1]

  if (attr(TabDescription, "crossed") == "univariate") {
    if (attr(TabDescription, "Grapher")) {
      # Hacky way, but if I don't supply all Null lines with blank ggplots, there is a print in console and if I don't put ggplots in them, [NULL] in the output
      # I still get a warning so I decided to suppress it for now
      LignesNULL <- purrr::map_lgl(ResDescription[, if (attr(TabDescription, "Comparer")) 4 else 3], ~ length(.x) < 2)
      LignesGG <- which(purrr::map_int(ResDescription[, if (attr(TabDescription, "Comparer")) 4 else 3], length) > 1)
      ListeImg <- as.list(rep(getOption("blanc_datavar"), sum(LignesNULL)))
      suppressWarnings(ResDescription[LignesNULL, if (attr(TabDescription, "Comparer")) 4 else 3] <- ListeImg)
    }
    FlexDescription <- flextable::flextable(ResDescription)
    if (attr(TabDescription, "Grapher")) {
      FlexDescription <- flextable::compose(FlexDescription, i = LignesNULL, j = if (attr(TabDescription, "Comparer")) 4 else 3, value = flextable::as_paragraph(flextable::as_image(src = ., height = 1, width = 1)), use_dot = TRUE)
      if (any(TabLabels$type == "quanti")) {
        TabLabels$debut[TabLabels$type == "quanti"] <- purrr::map_dbl(TabLabels$label[TabLabels$type == "quanti"], ~ 1 + which(ResDescription[, 1] == .x)[1])
        TabLabels$fin[TabLabels$type == "quanti"] <- purrr::map_dbl(which(TabLabels$type == "quanti"), ~ if (.x == nrow(TabLabels)) {nrow(ResDescription)} else {which(ResDescription[, 1] == TabLabels$label[.x + 1]) - 1})
        FlexDescription <- purrr::reduce2(TabLabels$debut[TabLabels$type == "quanti"], TabLabels$fin[TabLabels$type == "quanti"],
                                          \(data, d, f) flextable::merge_at(data, i = seq(d, f), j = if (attr(TabDescription, "Comparer")) 4 else 3), .init = FlexDescription)
      }
      FlexDescription <- flextable::compose(FlexDescription, i = LignesGG[LignesGG %in% TabLabels$debut], j = if (attr(TabDescription, "Comparer")) 4 else 3, value = flextable::as_paragraph(flextable::gg_chunk(value = ., height = 1, width = 1)), use_dot = TRUE)
      FlexDescription <- flextable::compose(FlexDescription, i = LignesGG[!LignesGG %in% TabLabels$debut], j = if (attr(TabDescription, "Comparer")) 4 else 3, value = flextable::as_paragraph(flextable::gg_chunk(value = ., height = .25, width = 1)), use_dot = TRUE)
    }
    FlexDescription <- purrr::reduce(LignesFusion, \(data, x) flextable::merge_at(data, i = x, j = seq_len(flextable::ncol_keys(FlexDescription))), .init = FlexDescription)
    FlexDescription <- purrr::reduce(LignesFusion, \(data, x) flextable::fontsize(data, i = x, j = 1, size = BaseSize * 1.2) |> flextable::bold(i = x, j = 1), .init = FlexDescription)
    FlexDescription <- purrr::reduce2(seq_along(Widths), Widths, \(data, x, larg) flextable::width(data, j = x, width = larg), .init = FlexDescription)
    FlexDescription <- flextable::add_footer_lines(FlexDescription, values = attr(TabDescription, "footnote"))
    FlexDescription <- ThemeDescription(FlexDescription,
                                        BaseSize = BaseSize,
                                        Separations = seq(2, flextable::ncol_keys(FlexDescription) - 1),
                                        ColNum = seq(2, flextable::ncol_keys(FlexDescription)))
  } else {
    if (attr(TabDescription, "Comparer") && attr(TabDescription, "tests_atypiques")) {
      TabModifP <- attr(TabDescription, "modif_p")
      TabModifP <- purrr::map_dfr(seq_len(nrow(TabModifP)), ~ data.frame(lab = TabModifP$vars[[.x]], symbole = TabModifP$symbole[.x]) |> dplyr::mutate(lig = 1 + match(lab, FlexDescription[[1]])))
      ResDescription[[ncol(ResDescription)]][TabModifP$lig] <- paste0(ResDescription[[ncol(ResDescription)]][TabModifP$lig], TabModifP$symbole)
    }
    FlexDescription <- flextable::flextable(ResDescription)
    FlexDescription <- purrr::reduce(LignesFusion, \(data, x) flextable::merge_at(data, i = x, j = seq_len(flextable::ncol_keys(FlexDescription))), .init = FlexDescription)
    FlexDescription <- purrr::reduce(LignesFusion, \(data, x) flextable::fontsize(data, i = x, j = 1, size = BaseSize * 1.2) |> flextable::bold(i = x, j = 1), .init = FlexDescription)
    FlexDescription <- purrr::reduce2(seq_along(Widths), Widths, \(data, x, larg) flextable::width(data, j = x, width = larg), .init = FlexDescription)
    if (attr(TabDescription, "Comparer"))
      FlexDescription <- flextable::add_footer_lines(FlexDescription, values = attr(TabDescription, "footnote"))
    FlexDescription <- ThemeDescription(FlexDescription,
                                        BaseSize = BaseSize,
                                        Separations = seq(2, flextable::ncol_keys(FlexDescription) - 1),
                                        ColNum = seq(2, flextable::ncol_keys(FlexDescription)))
  }

  return(FlexDescription)

}


