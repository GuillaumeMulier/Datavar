
#' Print description table in console
#'
#' @param x The table of description
#'
#' @export
#'
#' @examples
#' TabBinaire(mtcars, am, Grapher = TRUE)
print.tab_datavar <- function(x) {

  if (inherits(x, "Grapher")) {
    if (attr(x, "crossed") == "univariate") {
      print.data.frame(x[, -4])
    } else {
      print.data.frame(x[, -c(4, 5)])
    }
  } else {
    print.data.frame(x)
  }

  invisible(x)

}



ThemePerso <- function(tab) {

  if (!inherits(tab, "flextable"))
    stop("Function \"ThemePerso()\" should be used on flextable object.", call. = FALSE)



}


















# Tableau %>%
#   as_grouped_data(groups = "var") %>%
#   mutate(across(eff, ~ ifelse(row_number() == 1, var, .x))) %>%
#   select(-1) %>%
#   flextable() %>%
#   mk_par(j = c(3), value = as_paragraph(gg_chunk(value = ., height = 1, width = 1)),
#          use_dot = TRUE) %>%
#   merge_at(i = 1, j = 1:2) %>%
#   merge_at(i = 2:4, j = 3) %>%
#   width(j = 2, width = 2) %>%
#   padding(padding.top = 1, padding.bottom = 1) %>%
#   fix_border_issues()




