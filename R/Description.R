#' VarDescr
#'
#' @param .Data Data.frame of the data.
#' @param LigneDatavar The line in datavar of the variable to describe.
#' @param y The variable used to cross.
#' @param NomCol Vector of the names of the columns.
#' @param Langue Language.
#' @param PMissing Number of decimals of percentage from whole dataset. NULL (default value) if no percent desired.
#' @param Poids Name of the column of .Data in which the weights are stored. Let NULL for unweighted analysis.
#' @param SMD Boolean to indicate if you want standardized mean differences. Of note, for weighted analysis, only SMD are available and no test.
#' @param Grapher Boolean if you want to graph the distribution in univariate case.
#' @param Simplif Boolean. If TRUE (default value) the table will be simplified to remove number of missing data if there aren't any.
#' @param Paired NULL if data are not paired/matched, else the name of the pairing variable as character (used only for bivariate analysis).
#'
#' @importFrom rlang !!
VarDescr <- function(.Data,
                     LigneDatavar,
                     y,
                     NomCol,
                     PMissing = NULL,
                     Poids = NULL,
                     SMD = FALSE,
                     Simplif = TRUE,
                     Langue,
                     Grapher = FALSE,
                     Paired = NULL) {

  x <- rlang::parse_expr(LigneDatavar[["var"]])

  if (LigneDatavar[["type"]] == "quanti") {
    Tableau <- TabQuanti(
      .Data = .Data, x = !!x, y = !!y,
      Prec = if (!is.na(LigneDatavar[["prec"]])) as.numeric(LigneDatavar[["prec"]]) else 0,
      PMissing = PMissing, NomCol = NomCol, Langue = Langue, Grapher = Grapher, Simplif = Simplif,
      Poids = !!Poids, SMD = SMD,
      NomVariable = if (!is.na(LigneDatavar[["nomvariable"]])) as.character(LigneDatavar[["nomvariable"]]) else NULL,
      Mode = if (!is.na(LigneDatavar[["mode"]])) as.character(LigneDatavar[["mode"]]) else "mediqrg",
      Test = if (!is.na(LigneDatavar[["test"]])) as.character(LigneDatavar[["test"]]) else "none",
      Mu0 = if (!is.na(LigneDatavar[["mu0"]])) as.numeric(LigneDatavar[["mu0"]]) else NULL,
      ChifPval = if (!is.na(LigneDatavar[["chif_pval"]])) as.numeric(LigneDatavar[["chif_pval"]]) else 2,
      ConfInter = if (!is.na(LigneDatavar[["conf_inter"]])) as.character(LigneDatavar[["conf_inter"]]) else "none",
      ConfLevel = if (!is.na(LigneDatavar[["conf_level"]])) as.numeric(LigneDatavar[["conf_level"]]) else .95,
      Paired = Paired
    )
  } else if (LigneDatavar[["type"]] == "binary") {
    Tableau <- TabBinaire(
      .Data = .Data, x = !!x, y = !!y,
      Prec = if (!is.na(LigneDatavar[["prec"]])) as.numeric(LigneDatavar[["prec"]]) else 0,
      PMissing = PMissing, NomCol = NomCol, Langue = Langue, Grapher = Grapher, Simplif = Simplif,
      NomCateg = if (!is.na(LigneDatavar[["nomcateg"]])) as.character(LigneDatavar[["nomcateg"]]) else NULL,
      NomLabel = if (!is.na(LigneDatavar[["label"]])) as.character(LigneDatavar[["label"]]) else NULL,
      Test = if (!is.na(LigneDatavar[["test"]])) as.character(LigneDatavar[["test"]]) else "none",
      Poids = !!Poids, SMD = SMD,
      P0 = if (!is.na(LigneDatavar[["mu0"]])) as.numeric(as.numeric(strsplit(LigneDatavar[["mu0"]], ";")[[1]])) else NULL,
      ChifPval = if (!is.na(LigneDatavar[["chif_pval"]])) as.numeric(LigneDatavar[["chif_pval"]]) else 2,
      ConfInter = if (!is.na(LigneDatavar[["conf_inter"]])) as.character(LigneDatavar[["conf_inter"]]) else "none",
      ConfLevel = if (!is.na(LigneDatavar[["conf_level"]])) as.numeric(LigneDatavar[["conf_level"]]) else .95,
      Paired = Paired
    )
  } else if (LigneDatavar[["type"]] == "quali") {
    Tableau <- TabQuali(
      .Data = .Data, x = !!x, y = !!y,
      Prec = if (!is.na(LigneDatavar[["prec"]])) as.numeric(LigneDatavar[["prec"]]) else 0,
      PMissing = PMissing, NomCol = NomCol, Langue = Langue, Grapher = Grapher, Simplif = Simplif,
      NomVariable = if (!is.na(LigneDatavar[["nomvariable"]])) as.character(LigneDatavar[["nomvariable"]]) else NULL,
      Ordonnee = if (!is.na(LigneDatavar[["ordonnee"]])) as.logical(LigneDatavar[["ordonnee"]]) else TRUE,
      Test = if (!is.na(LigneDatavar[["test"]])) as.character(LigneDatavar[["test"]]) else "none",
      Poids = !!Poids, SMD = SMD,
      P0 = if (!is.na(LigneDatavar[["mu0"]])) as.numeric(as.numeric(strsplit(LigneDatavar[["mu0"]], ";")[[1]])) else NULL,
      ChifPval = if (!is.na(LigneDatavar[["chif_pval"]])) as.numeric(LigneDatavar[["chif_pval"]]) else 2,
      ConfInter = if (!is.na(LigneDatavar[["conf_inter"]])) as.character(LigneDatavar[["conf_inter"]]) else "none",
      ConfLevel = if (!is.na(LigneDatavar[["conf_level"]])) as.numeric(LigneDatavar[["conf_level"]]) else .95,
      Paired = Paired
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
#' @param Poids Name of the column of .Data in which the weights are stored. Let NULL for unweighted analysis.
#' @param SMD Boolean to indicate if you want standardized mean differences. Of note, for weighted analysis, only SMD are available and no test.
#' @param NomCol The vector of the names you want to give to the columns of the output. NULL for automatic naming.
#' @param Langue "fr" for French and "eng" for English.
#' @param Grapher Boolean if you want to graph the distribution in univariate case.
#' @param Simplif Not usefull, if TRUE will delete unused column 'Pvalue'.
#' @param Comparer TRUE won't alterate the datavar, but FALSE will ensure that no test will be performed even if supplied in the datavar. In case of crossed description. Of note, if no test is supplied in the datavar, even with Comparer = TRUE, there will be no comparison made.
#' @param Paired NULL if data are not paired/matched, else the name of the pairing variable as character (used only for bivariate analysis).
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
                        Poids = NULL,
                        SMD = FALSE,
                        NomCol = NULL,
                        Langue = "eng",
                        Grapher = FALSE,
                        Simplif = TRUE,
                        Comparer = FALSE,
                        Paired = TRUE) {

  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)
  if (!is.null(y)) .Listevar <- .Listevar[.Listevar != rlang::quo_name(y)]
  StockMeta <- rlang::new_environment(parent = rlang::current_env())
  StockMeta$tests <- if (Comparer) data.frame(var = character(0), test = character(0), type = character(0)) else NULL
  StockMeta$labels <- data.frame(var = character(0), label = character(0), type = character(0))
  StockMeta$smds <- if (SMD & !is.null(y)) data.frame(var = character(0), label = character(0), type = character(0), smd = numeric(0)) else NULL

  # Verifications
  stopifnot(is.logical(Comparer), length(Comparer) == 1, is.logical(Grapher), length(Grapher) == 1)
  if (!Comparer) .Datavar$test <- "none"
  .Datavar <- VerifArgs(.Datavar, .Listevar)
  .Datavar$test <- purrr::map_chr(.Datavar$test, ~ match.arg(.x, c("none", "student", "studentvar", "ztest", "wilcoxon", "kruskal-wallis", "signed-wilcoxon",
                                                                   "anova", "fisher", "chisq", "binomial", "multinomial", "mcnemar")))

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
                      Poids = Poids,
                      SMD = SMD,
                      Grapher = Grapher,
                      Simplif = Simplif,
                      Paired = Paired)
      if (Comparer) StockMeta$tests <- rbind(StockMeta$tests, data.frame(var = Tab[1, 1], test = Ligne[["test"]], type = Ligne[["type"]]))
      StockMeta$labels <- rbind(StockMeta$labels, data.frame(var = var, label = Tab[1, 1], type = Ligne[["type"]]))
      if (SMD & !is.null(y)) StockMeta$smds <- rbind(StockMeta$smds, data.frame(var = var, label = Tab[1, 1], type = Ligne[["type"]], smd = attr(Tab, "standardized_mean_difference")))
      return(Tab)
    }
  )

  # Track labels of variables
  attr(Tableau, "tab_lab") <-  StockMeta$labels
  if (SMD & !is.null(y)) attr(Tableau, "smds") <- StockMeta$smds

  # Retrieve metadata to present results
  # Add them as attributes to store them and use them with flextable to create footnotes automatically on which tests are made
  row.names(Tableau) <- NULL
  if (Comparer) {
    if (Langue == "fr") {
      DicoVariables <- c("quanti" = "quantitatives", "quali" = "catégorielles", "binary" = "binaires")
      DicoTests <- c("student" = "test T de Student", "studentvar" = "test T de Student avec correction de Welch", "ztest" = "test Z", "wilcoxon" = "test de Wilcoxon-Mann-Whitney",
                     "signed-wilcoxon" = "test des rangs signés de Wilcoxon", "binomial" = "test binomial exact", "multinomial" = "test multinomial exact",
                     "kruskal" = "test de Kruskal-Wallis", "fisher" = "test exact de Fisher", "chisq" = "test du Khi-2", "mcnemar" = "test de McNemar",
                     "anova" = "ANOVA")
    } else {
      DicoVariables <- c("quanti" = "quantitative", "quali" = "categorical", "binary" = "binary")
      DicoTests <- c("student" = "Student's T-test", "studentvar" = "Student's T-test with Welch's correction", "ztest" = "Z-test", "wilcoxon" = "Wilcoxon-Mann-Whitney's test",
                     "signed-wilcoxon" = "signed ranks' Wilcoxon test", "binomial" = "exact binomial test", "multinomial" = "exact multinomial test",
                     "kruskal" = "Kruskal-Wallis' test", "fisher" = "exact Fisher's test", "chisq" = "Khi-2 test", "mcnemar" = "McNemar test",
                     "anova" = "ANOVA")
    }
    if (is.null(y)) StockMeta$tests$test[StockMeta$tests$test == "studentvar"] <- "student"
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
  attr(Tableau, "difference_moy_stand") <- SMD & !is.null(y)
  attr(Tableau, "Comparer") <- Comparer & any(.Datavar$test[.Datavar$var %in% .Listevar] != "none") & is.null(Poids)
  attr(Tableau, "standardized_mean_difference") <- NULL # Inherited attribute to remove
  return(Tableau)

}



#' @rdname Description
#' @export
#' @importFrom rlang !!
NoMessDescription <- function(.Data,
                              y = NULL,
                              .Datavar,
                              .Listevar = .Datavar[[1]],
                              PMissing = NULL,
                              Poids = NULL,
                              SMD = FALSE,
                              NomCol = NULL,
                              Langue = "eng",
                              Grapher = FALSE,
                              Simplif = TRUE,
                              Comparer = TRUE) {

  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)
  suppressMessages(Tableau <- Description(.Data = .Data,
                                          y = !!y,
                                          .Datavar = .Datavar,
                                          .Listevar = .Listevar,
                                          PMissing = PMissing,
                                          Poids = !!Poids,
                                          SMD = SMD,
                                          NomCol = NomCol,
                                          Langue = Langue,
                                          Grapher = Grapher,
                                          Simplif = Simplif,
                                          Comparer = Comparer))
  return(Tableau)

}


#' @rdname Description
#' @export
#' @importFrom rlang !!
SilentDescription <- function(.Data,
                              y = NULL,
                              .Datavar,
                              .Listevar = .Datavar[[1]],
                              PMissing = NULL,
                              Poids = NULL,
                              SMD = FALSE,
                              NomCol = NULL,
                              Langue = "eng",
                              Grapher = FALSE,
                              Simplif = TRUE,
                              Comparer = FALSE) {

  y <- rlang::enexpr(y)
  Poids <- rlang::enexpr(Poids)
  suppressMessages(suppressWarnings(Tableau <- Description(.Data = .Data,
                                                           y = !!y,
                                                           .Datavar = .Datavar,
                                                           .Listevar = .Listevar,
                                                           PMissing = PMissing,
                                                           Poids = !!Poids,
                                                           SMD = SMD,
                                                           NomCol = NomCol,
                                                           Langue = Langue,
                                                           Grapher = Grapher,
                                                           Simplif = Simplif,
                                                           Comparer = Comparer)))
  return(Tableau)

}

