#' VarDescr
#'
#' @param .Data Data.frame of the data.
#' @param LigneDatavar The line in datavar of the variable to describe.
#' @param y The variable used to cross.
#' @param NomCol Vector of the names of the columns.
#' @param Langue Language.
#' @param PMissing Number of decimals of percentage from whole dataset. NULL (default value) if no percent desired.
#' @param Grapher Boolean if you want to graph the distribution in univariate case.
#' @param Simplif Boolean. If TRUE (default value) the table will be simplified to remove number of missing data if there aren't any.
#'
#' @importFrom rlang !!
VarDescr <- function(.Data,
                     LigneDatavar,
                     y,
                     NomCol,
                     PMissing = NULL,
                     Simplif = TRUE,
                     Langue,
                     Grapher = FALSE) {

  x <- rlang::parse_expr(LigneDatavar[["var"]])

  if (LigneDatavar[["type"]] == "quanti") {
    Tableau <- TabQuanti(
      .Data = .Data, x = !!x, y = !!y,
      Prec = if (!is.na(LigneDatavar[["prec"]])) as.numeric(LigneDatavar[["prec"]]) else 0,
      PMissing = PMissing, NomCol = NomCol, Langue = Langue, Grapher = Grapher, Simplif = Simplif,
      NomVariable = if (!is.na(LigneDatavar[["nomvariable"]])) as.character(LigneDatavar[["nomvariable"]]) else NULL,
      Mode = if (!is.na(LigneDatavar[["mode"]])) as.character(LigneDatavar[["mode"]]) else "mediqrg",
      Test = if (!is.na(LigneDatavar[["test"]])) as.character(LigneDatavar[["test"]]) else "none",
      ChifPval = if (!is.na(LigneDatavar[["chif_pval"]])) as.numeric(LigneDatavar[["chif_pval"]]) else 2
    )
  } else if (LigneDatavar[["type"]] == "binary") {
    Tableau <- TabBinaire(
      .Data = .Data, x = !!x, y = !!y,
      Prec = if (!is.na(LigneDatavar[["prec"]])) as.numeric(LigneDatavar[["prec"]]) else 0,
      PMissing = PMissing, NomCol = NomCol, Langue = Langue, Grapher = Grapher, Simplif = Simplif,
      NomCateg = if (!is.na(LigneDatavar[["nomcateg"]])) as.character(LigneDatavar[["nomcateg"]]) else NULL,
      NomLabel = if (!is.na(LigneDatavar[["label"]])) as.character(LigneDatavar[["label"]]) else NULL,
      Test = if (!is.na(LigneDatavar[["test"]])) as.character(LigneDatavar[["test"]]) else "none",
      ChifPval = if (!is.na(LigneDatavar[["chif_pval"]])) as.numeric(LigneDatavar[["chif_pval"]]) else 2,
      ConfInter = if (!is.na(LigneDatavar[["conf_inter"]])) as.character(LigneDatavar[["conf_inter"]]) else "none",
      ConfLevel = if (!is.na(LigneDatavar[["conf_level"]])) as.numeric(LigneDatavar[["conf_level"]]) else .95
    )
  } else if (LigneDatavar[["type"]] == "quali") {
    Tableau <- TabQuali(
      .Data = .Data, x = !!x, y = !!y,
      Prec = if (!is.na(LigneDatavar[["prec"]])) as.numeric(LigneDatavar[["prec"]]) else 0,
      PMissing = PMissing, NomCol = NomCol, Langue = Langue, Grapher = Grapher, Simplif = Simplif,
      NomVariable = if (!is.na(LigneDatavar[["nomvariable"]])) as.character(LigneDatavar[["nomvariable"]]) else NULL,
      Ordonnee = if (!is.na(LigneDatavar[["ordonnee"]])) as.logical(LigneDatavar[["ordonnee"]]) else TRUE,
      Test = if (!is.na(LigneDatavar[["test"]])) as.character(LigneDatavar[["test"]]) else "none",
      ChifPval = if (!is.na(LigneDatavar[["chif_pval"]])) as.numeric(LigneDatavar[["chif_pval"]]) else 2,
      ConfInter = if (!is.na(LigneDatavar[["conf_inter"]])) as.character(LigneDatavar[["conf_inter"]]) else "none",
      ConfLevel = if (!is.na(LigneDatavar[["conf_level"]])) as.numeric(LigneDatavar[["conf_level"]]) else .95
    )
  }

  return(Tableau)

}


#' Description
#'
#' Description of a data.frame
#'
#' A function using the other individual description functions (\code{TabQuanti}, \code{TabQuali} and \code{TabBinaire}) to describe the variables of a data.frame.
#'
#' You can also use it to cross it with a categorial variable and perform a comparison test. For now no tests are performed with crossing with a variable that has more than 2 classes.
#'
#' NoMessDescription removes the information messages and SilentDescription removes also warnings.
#'
#' @param .Data The dataset to describe.
#' @param y The crossing variable. NULL if you only want univariate description.
#' @param .Datavar The datavar.
#' @param .Listevar A vector of the name of the columns you want to describe. By default, it is the whole set of variables present in the datavar.
#' @param PMissing Number of decimals of percentage from whole dataset. NULL (default value) if no percent desired.
#' @param NomCol The vector of the names you want to give to the columns of the output. NULL for automatic naming.
#' @param Langue "fr" for French and "eng" for English.
#' @param Grapher Boolean if you want to graph the distribution in univariate case.
#' @param Simplif Not usefull, if TRUE will delete unused column 'Pvalue'.
#' @param Comparer TRUE won't alterate the datavar, but FALSE will ensure that no test will be performed even if supplied in the datavar. In case of crossed description. Of note, if no test is supplied in the datavar, even with Comparer = TRUE, there will be no comparison made.
#'
#' @export
#'
#' @seealso [TabQuali()], [TabQuanti()], [TabBinaire()]
#'
#' @examples
#' # Create the datavar object
#' DatavarVoitures <- CreateDatavar(mtcars)
#' # The description of the whole mtcars dataset
#' Description(mtcars, .Datavar = DatavarVoitures, Langue = "eng")
#' # Only some variables
#' Description(mtcars, .Datavar = DatavarVoitures, .Listevar = c("mpg", "am", "cyl"), Langue = "eng")
#' # Crossed (messages for Wilcoxon's test because of ex aequos)
#' Description(mtcars, .Datavar = DatavarVoitures, y = am, Langue = "eng", Comparer = FALSE)
#' Description(mtcars, .Datavar = DatavarVoitures, y = am, Langue = "eng", Comparer = TRUE)
Description <- function(.Data,
                        y = NULL,
                        .Datavar,
                        .Listevar = .Datavar[[1]],
                        PMissing = NULL,
                        NomCol = NULL,
                        Langue = "eng",
                        Grapher = FALSE,
                        Simplif = TRUE,
                        Comparer = TRUE) {

  y <- rlang::enexpr(y)
  if (!is.null(y)) .Listevar <- .Listevar[.Listevar != rlang::quo_name(y)]
  StockMeta <- rlang::new_environment(parent = rlang::current_env())
  StockMeta$tests <- if (Comparer) data.frame(var = character(0), test = character(0), type = character(0)) else NULL
  StockMeta$labels <- data.frame(var = character(0), label = character(0), type = character(0))

  # Verifications
  stopifnot(is.logical(Comparer), length(Comparer) == 1, is.logical(Grapher), length(Grapher) == 1)
  if (!Comparer) .Datavar$test <- "none"
  .Datavar <- VerifArgs(.Datavar, .Listevar)
  .Datavar$test <- purrr::map_chr(.Datavar$test, ~ match.arg(.x, c("none", "student", "ztest", "wilcoxon", "kruskal", "fisher", "chisq", "mcnemar")))

  Tableau <- purrr::map_dfr(
    .x = .Listevar,
    .f = \(var) {
      Ligne <-  .Datavar[.Datavar$var == var, ]
      Tab <- VarDescr(.Data = .Data,
                      LigneDatavar = Ligne,
                      y = y,
                      NomCol = NomCol,
                      Langue = Langue,
                      PMissing = PMissing,
                      Grapher = Grapher,
                      Simplif = Simplif)
      if (Comparer) StockMeta$tests <- rbind(StockMeta$tests, data.frame(var = Tab[1, 1], test = Ligne[["test"]], type = Ligne[["type"]]))
      StockMeta$labels <- rbind(StockMeta$labels, data.frame(var = var, label = Tab[1, 1], type = Ligne[["type"]]))
      return(Tab)
    }
  )

  # Track labels of variables
  attr(Tableau, "tab_lab") <-  StockMeta$labels

  # Retrieve metadata to present results
  # Add them as attributes to store them and use them with flextable to create footnotes automatically on which tests are made
  row.names(Tableau) <- NULL
  if (Comparer & !is.null(y)) {
    if (Langue == "fr") {
      DicoVariables <- c("quanti" = "quantitatives", "quali" = "catégorielles", "binary" = "binaires")
      DicoTests <- c("student" = "test T de Student", "ztest" = "test Z", "wilcoxon" = "test de Wilcoxon-Mann-Whitney",
                     "kruskal" = "test de Kruskal-Wallis", "fisher" = "test exact de Fisher", "chisq" = "test du Khi-2", "mcnemar" = "test de McNemar")
    } else {
      DicoVariables <- c("quanti" = "quantitative", "quali" = "categorical", "binary" = "binary")
      DicoTests <- c("student" = "Student's T-test", "ztest" = "Z-test", "wilcoxon" = "Wilcoxon-Mann-Whitney's test",
                     "kruskal" = "Kruskal-Wallis' test", "fisher" = "exact Fisher's test", "chisq" = "Khi-2 test", "mcnemar" = "McNemar test")
    }
    Testings <- StockMeta$tests |>
      dplyr::filter(test != "none") |>
      dplyr::mutate(test = ifelse(test == "ztest" & type %in% c("quali", "binary"), "chisq", test)) |>
      dplyr::count(type, test) |>
      dplyr::group_by(type) |>
      dplyr::mutate(nb_dist = dplyr::n_distinct(test)) |>
      dplyr::ungroup() |>
      dplyr::arrange(type, -n)
    attr(Tableau, "tests_atypiques") <- FALSE
    if (all(Testings$nb_dist == 1)) {
      attr(Tableau, "footnote") <- if (Langue == "fr") {
        paste(DicoTests[Testings$test], "pour les variables", DicoVariables[Testings$type], collapse = " / ")
      } else {
        paste(DicoTests[Testings$test], "for", DicoVariables[Testings$type], "variables", collapse = " / ")
      }
    } else {
      Symboles <- c("binary" = "*", "quali" = "¤", "quanti" = "§")
      attr(Tableau, "tests_atypiques") <- TRUE
      Testings <- split(Testings, Testings$type)
      attr(Tableau, "footnote") <- purrr::map(Testings, \(tab) {
        if (nrow(tab) == 1) {
          return(if (Langue == "fr") paste(DicoTests[tab$test], "pour les variables", DicoVariables[tab$type]) else paste(DicoTests[tab$test], "for", DicoVariables[tab$type], "variables"))
        } else {
          deb_char <- if (Langue == "fr") paste(DicoTests[tab$test[1]], "pour les variables", DicoVariables[tab$type[1]]) else paste(DicoTests[tab$test[1]], "for", DicoVariables[tab$type[1]], "variables")
          fin_char <- if (Langue == "fr") {
            paste(strrep(Symboles[tab$type[1]], seq_len(nrow(tab) - 1)), " : ", DicoTests[tab$test[-1]], collapse = ", ", sep = "")
          } else {
            paste(strrep(Symboles[tab$type[1]], seq_len(nrow(tab) - 1)), ": ", DicoTests[tab$test[-1]], collapse = ", ", sep = "")
          }
          return(paste0(deb_char, " (except ", fin_char, ")"))
        }
      }) |>
        paste(collapse = " / ")
      attr(Tableau, "modif_p") <- purrr::map_dfr(Testings, \(tab) {
        if (nrow(tab) == 1) {
          return(NULL)
        } else {
          tab <- tab[-1, ]
          tab$symbole <- strrep(Symboles[tab$type[1]], seq_len(nrow(tab)))
          tab$vars <- purrr::map2(tab$type, tab$test, ~ StockMeta$tests$var[StockMeta$tests$test == .y & StockMeta$tests$type == .x])
          return(tab)
        }
      })
    }
  }

  class(Tableau) <- c("tab_description", "data.frame")
  attr(Tableau, "Grapher") <- Grapher & is.null(y)
  attr(Tableau, "Comparer") <- Comparer & !is.null(y) & any(.Datavar$test != "none")
  return(Tableau)

}



#' @rdname Description
#' @importFrom rlang !!
NoMessDescription <- function(.Data,
                              y = NULL,
                              .Datavar,
                              .Listevar = .Datavar[[1]],
                              PMissing = NULL,
                              NomCol = NULL,
                              Langue = "eng",
                              Grapher = FALSE,
                              Simplif = TRUE,
                              Comparer = TRUE) {

  y <- rlang::enexpr(y)
  suppressMessages(Tableau <- Description(.Data = .Data,
                                          y = !!y,
                                          .Datavar = .Datavar,
                                          .Listevar = .Listevar,
                                          PMissing = PMissing,
                                          NomCol = NomCol,
                                          Langue = Langue,
                                          Grapher = Grapher,
                                          Simplif = Simplif,
                                          Comparer = Comparer))
  return(Tableau)

}


#' @rdname Description
#' @importFrom rlang !!
SilentDescription <- function(.Data,
                              y = NULL,
                              .Datavar,
                              .Listevar = .Datavar[[1]],
                              PMissing = NULL,
                              NomCol = NULL,
                              Langue = "eng",
                              Grapher = FALSE,
                              Simplif = TRUE,
                              Comparer = TRUE) {

  y <- rlang::enexpr(y)
  suppressMessages(suppressWarnings(Tableau <- Description(.Data = .Data,
                                                           y = !!y,
                                                           .Datavar = .Datavar,
                                                           .Listevar = .Listevar,
                                                           PMissing = PMissing,
                                                           NomCol = NomCol,
                                                           Langue = Langue,
                                                           Grapher = Grapher,
                                                           Simplif = Simplif,
                                                           Comparer = Comparer)))
  return(Tableau)

}

