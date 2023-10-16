
#' Inverse of %in%
'%nin%' <- function(x, y) {match(x, y, nomatch = 0) <= 0}


#' Check the validity of supplied arguments for the functions
#'
#' @param ... The tested argument with helping arguments if needed. The first argument will guide the function
#'
#' @return The value of the argument
VerifArgs <- function(...) {

  arguments <- rlang::enquos(..., .named = TRUE)
  nom_var <- names(arguments)
  # Actually, even if there is ... in argument, I'll just use it for 1 argument at a time but I don't know which one in advance
  Fct <- switch(nom_var[1],
                Langue = function(Langue){
                  if (Langue %in% c("fr", "Fr", "français", "Français", "francais", "Francais", "french", "French")) {
                    Langue <- "fr"
                  } else if (Langue %in% c("eng", "Ang", "ang", "Eng", "english", "English", "anglais", "Anglais")) {
                    Langue <- "eng"
                  } else {
                    stop(paste0("Unrecognized language: ", Langue, "\nChoose betwwen 'fr', 'eng' in argument \"", PrintArg("Langue"), "\"."), call. = FALSE)
                  }
                  return(Langue)
                },
                Prec = function(Prec, x) {
                  if (length(Prec) != 1 || !is.numeric(Prec) || Prec %% 1 != 0 || Prec < 0)
                    stop("\"", PrintArg("Prec"), "\" should be a whole positive integer for variable \"", PrintVar(rlang::quo_name(x)), "\".", call. = FALSE)
                  return(paste0("%.", Prec, "f"))
                },
                ChifPval = function(ChifPval) {
                  if (length(ChifPval) != 1 || !is.numeric(ChifPval) || ChifPval %% 1 != 0 || ChifPval < 0)
                    stop("\"", PrintArg("ChifPval"), "\" should be a whole positive number.", call. = FALSE)
                  return(ChifPval)
                },
                PMissing = function(PMissing) {
                  if (!is.null(PMissing) && (length(PMissing) != 1 || !is.numeric(PMissing) || PMissing %% 1 != 0 || PMissing < 0))
                    stop("\"", PrintArg("PMissing"), "\" should be a whole positive number or NULL.", call. = FALSE)
                  return(PMissing)
                },
                NomCateg = function(NomCateg, NomLabel, VarBinaire, x, Binaire = FALSE) {
                  if (!is.null(NomCateg)) {
                    if (NomCateg == "") {
                      NomCateg <- NULL
                    } else if (NomCateg %nin% names(table(VarBinaire, useNA = "no"))) {
                      if (!Binaire) {
                        stop(paste0("Class \"", NomCateg, "\" isn't in variable \"", PrintVar(rlang::quo_name(x)),
                                    "\".\nPossible classes: ", paste(names(table(VarBinaire, useNA = "no")), collapse = " / "), "."),
                             call. = FALSE)
                      } else {
                        if (length(names(table(VarBinaire, useNA = "no"))) != 1) {
                          stop(paste0("Class \"", NomCateg, "\" isn't in variable \"", PrintVar(rlang::quo_name(x)),
                                      "\".\nPossible classes: ", paste(names(table(VarBinaire, useNA = "no")), collapse = " / "), "."),
                               call. = FALSE)
                        }
                      }
                    }
                  } else {
                    NomCateg <- names(table(VarBinaire, useNA = "no"))[length(names(table(VarBinaire, useNA = "no")))]
                    message(paste0("\"", PrintArg("NomCateg"), "\" of variable \"", PrintVar(rlang::quo_name(x)), "\" isn't specified, thus it is taken to be ",
                                   NomCateg, ".Check if the name of the label (", if (is.null(NomLabel)) "NULL" else NomLabel,
                                   ") corresponds to it."))
                  }
                  return(NomCateg)
                },
                NomLabel = function(NomLabel, VarBinaire, x) {
                  if (is.null(NomLabel)) {
                    NomLabel <- paste0(rlang::quo_name(x), "=", names(table(VarBinaire, useNA = "no"))[length(names(table(VarBinaire, useNA = "no")))])
                    message(paste0("Unspecified label for variable \"", PrintVar(rlang::quo_name(x)), "\". Default label will be \"",
                                   NomLabel, "\"."))
                  }
                  return(NomLabel)
                },
                NomVariable = function(NomVariable, x) {
                  if (is.null(NomVariable)) {
                    return(paste0(rlang::quo_name(x)))
                  } else {
                    if (!is.character(NomVariable) || length(NomVariable) != 1)
                      stop(paste0("Variable \"", PrintVar(rlang::quo_name(x)), "\"'s ", PrintArg("NomVariable"), " should be a string designing the name you want to display."), call. = FALSE)
                    return(NomVariable)
                  }
                },
                VarBinaire = function(VarBinaire, NomCateg, x) {
                  if (sum(!is.na(VarBinaire)) == 0)
                    stop(paste0("Variable \"", PrintVar(rlang::as_label(x)), "\" has 0 non missing values."), call. = FALSE)
                  if (length(unique(VarBinaire[!is.na(VarBinaire)])) > 2) {
                    stop(paste0("The variable to describe \"", PrintVar(rlang::quo_name(x)), "\" isn't a binary variable and has more than 2 classes."), call. = FALSE)
                  } else if (length(unique(VarBinaire[!is.na(VarBinaire)])) == 1) {
                    warning(paste0("The variable to describe \"", PrintVar(rlang::quo_name(x)), "\" only have 1 class."))
                  }
                  if (is.factor(VarBinaire))
                    VarBinaire <- as.character(VarBinaire)
                  return(as.numeric(VarBinaire == NomCateg))
                },
                VarQuali = function(VarQuali, Ordonnee, x) {
                  if (sum(!is.na(VarQuali)) == 0)
                    stop(paste0("Variable \"", PrintVar(rlang::quo_name(x)), "\" has 0 non missing values."), call. = FALSE)
                  if (length(unique(VarQuali[!is.na(VarQuali)])) == 1)
                    message(Information(paste0(" The variable to describe \"", PrintVar(rlang::quo_name(x)), "\" only have 1 class.")))
                  if (Ordonnee) {
                    if (!is.factor(VarQuali))
                      VarQuali <- factor(VarQuali, levels = sort(unique(VarQuali[!is.na(VarQuali)])))
                  } else {
                    VarQuali <- factor(as.character(VarQuali), levels = names(sort(table(VarQuali), decreasing = TRUE)))
                  }
                  return(VarQuali)
                },
                ConfInter = function(ConfInter, Poids) {
                  ConfInter <- match.arg(ConfInter, c("none", "normal", "exact", "jeffreys"))
                  if (all(Poids %in% c(0, 1))) {
                    return(ConfInter)
                  } else {
                    if (ConfInter == "none") {
                      return("none")
                    } else {
                      if (ConfInter != "normal")
                        message(Information("With weights, only normal confidence interval is supported."))
                      return("normal")
                    }
                  }
                },
                ConfLevel = function(ConfLevel, x) {
                  if (length(ConfLevel) != 1 || !is.numeric(ConfLevel) || ConfLevel <= 0 || ConfLevel >= 1)
                    stop(paste0("\"", PrintArg("ConfLevel"), "\" for variable \"", PrintVar(rlang::quo_name(x)), "\" must be a unique number between 0 and 1."), call. = FALSE)
                  return(ConfLevel)
                },
                Poids = function(Poids, x, Variable, .Data) {
                  if (is.null(Poids)) {
                    return(rep(1, length(Variable)))
                  } else {
                    Poids <- rlang::eval_tidy(Poids, data = .Data)
                    if (any(is.na(Poids)))
                      message(Information("Missing weights aren't supported and artificially put to 0."))
                    Poids[is.na(Poids)] <- 0
                    if (!is.numeric(Poids) || length(Poids) != length(Variable) || any(Poids < 0))
                      stop(paste0("For variable \"", PrintVar(rlang::quo_name(x)), "\", the argument \"", PrintArg("Poids"), "\" should either be NULL if you don't want to weight or the column of a numeric positive vector of the same length as the variable to describe."), call. = FALSE)
                    return(Poids)
                  }
                },
                P0 = function(P0, VarQuali, x) {
                  if (length(unique(VarQuali[!is.na(VarQuali)])) == 2) {
                    if (is.null(P0)) {
                      P0 <- c(.5, .5)
                    } else if (length(P0) == 1) {
                      P0 <- c(1 - P0, P0)
                    } else if (length(P0) == 2) {
                      if (round(abs(1 - sum(P0)), 15) != 0)
                        stop(paste0("For \"", PrintVar(rlang::quo_name(x)), "\", the sum of probabilities in \"", PrintArg("P0"), "\" should sum to 1."), call. = FALSE)
                    }
                  } else {
                    if (is.null(P0)) {
                      P0 <- rep(1 / nlevels(VarQuali), nlevels(VarQuali))
                    }
                  }
                  if (any(P0 < 0 | P0 > 1))
                    stop(paste0("For \"", PrintVar(rlang::quo_name(x)), "\", \"", PrintArg("P0"), "\" should contain probabilities between 0 and 1."), call. = FALSE)
                  return(P0)
                },
                Mu0 = function(Mu0, VarQuanti, x) {
                  if (length(Mu0) != 1 || !is.numeric(Mu0))
                    stop(paste0("For \"", PrintVar(rlang::quo_name(x)), "\", \"", PrintArg("Mu0"), "\" should contain the theoretical mean you want to. It should be a numeric vector of length 1."), call. = FALSE)
                  if (sum(Mu0 > range(VarQuanti, na.rm = TRUE)) != 1)
                    message(Information(paste0(" For variable \"", PrintVar(rlang::quo_name(x)), "\" you supplied a \"", PrintArg("Mu0"), "\" that isn't in the range of the variable. Maybe it is an error ?")))
                  return(Mu0)
                },
                Mode = function(Mode, x, Langue, Prec, PMissing, HelperN) {
                  Groupes <- dplyr::tibble(
                    token = c("n", "miss", "moy", "sd", "med", "iq", "rg"),
                    label = c("N",
                              if (is.null(PMissing)) {
                                if (Langue == "fr") "N~Manq~" else "N~Miss~"
                              } else {
                                if (Langue == "fr") "N~Manq~(%)" else "N~Miss~(%)"
                              },
                              if (Langue == "fr") "Moyenne" else "Mean",
                              if (Langue == "fr") "(Ec-type)" else "(Sd)",
                              if (Langue == "fr") "Médiane" else "Median",
                              "(Q1-Q3)", "[min-max]"),
                    precision = c(HelperN, if (is.null(PMissing)) HelperN else paste0(HelperN, "(%.", PMissing, "f%%)"), rep(Prec, 5)),
                    fct = list(GetN, GetM, MeanVar, SdVar, MedianVar, IQR, RangeVar),
                    groupe = c(1, 1, 2, 2, 3, 3, 4)
                  )
                  if (!grepl("med|moy|iq|sd|rg", Mode))
                    stop(paste0("Unrecognized statistical presentation for variable \"", PrintVar(rlang::quo_name(x)), "\". Please type at least 1 of the following strings: \"med\", \"moy\", \"sd\", \"iq\", \"rg\"."), call. = FALSE)
                  Groupes <- Groupes[c(TRUE, TRUE, purrr::map_lgl(c("moy", "sd", "med", "iq", "rg"), ~ grepl(.x, Mode))), ]
                  Groupes <- split(Groupes, Groupes$groupe)
                  return(Groupes)
                },
                VarQuanti = function(VarQuanti, x) {
                  if (sum(!is.na(VarQuanti)) == 0)
                    stop(paste0("Variable ", PrintVar(rlang::quo_name(x)), " has 0 non missing values."), call. = FALSE)
                  if (!is.numeric(VarQuanti))
                    stop(paste0("The variable to describe \"", PrintVar(rlang::quo_name(x)), "\" isn't of numeric type. Consider transforming it to numeric type to describe it as quantitative variable or describe it as qualitative variable."), call. = FALSE)
                  return(VarQuanti)
                },
                .Datavar = function(.Datavar, ListeVar) {
                  if (any(.Datavar$type[.Datavar$var %in% ListeVar] %nin% c("quanti", "quali", "binary")))
                    stop("Second column of \"", PrintArg(".Datavar"), "\" should only be comprised of \"quanti\", \"quali\" and \"binary\".")
                  return(.Datavar)
                })

  return(Fct(...))

}


#' Check the statistical test
#'
#' @param x String naming the test.
#' @param type_var Type of variable.
#'
VerifTest <- function(Test, TypeVar, NClasses, Variable, y, x, Poids) {

  Test <- match.arg(Test, c("none", "student", "studentvar", "ztest", "wilcoxon", "kruskal-wallis", "signed-wilcoxon", "anova",
                            "fisher", "chisq", "binomial", "multinomial", "mcnemar"))
  if (!all(Poids %in% c(0, 1)) & Test != "none") {
    message(Information(paste0("Variable \"", PrintVar(rlang::quo_name(x)), "\": no test is supported with weighted population.")))
    Test <- "none"
  }

  if (NClasses == 1) {
    if (TypeVar == "binaire") {
      if (Test %nin% c("ztest", "binomial", "chisq", "none"))
        stop(paste0("Test unadapted to a binary variable: ", PrintVar(x), ". Choose one of none, ztest, binomial or chisq."), call. = FALSE)
    } else if (TypeVar == "quali") {
      if (Test %nin% c("ztest", "multinomial", "chisq", "none"))
        stop(paste0("Test unadapted to a qualitative variable: ", PrintVar(x), ". Choose one of none, ztest, multinomial or chisq."), call. = FALSE)
    } else if (TypeVar == "quanti") {
      if (Test %nin% c("ztest", "student", "studentvar", "signed-wilcoxon", "none"))
        stop(paste0("Test unadapted to a quantitative variable: ", PrintVar(x), ". Choose one of none, ztest, student or studentvar."), call. = FALSE)
      if (Test == "studentvar") Test <- "student"
    }
  } else if (NClasses == 2) {
    if (TypeVar == "binaire") {
      if (Test %nin% c("ztest", "mcnemar", "fisher", "chisq", "none"))
        stop(paste0("Test unadapted to a binary variable: ", PrintVar(x), ". Choose one of none, ztest, fisher or chisq."), call. = FALSE)
    } else if (TypeVar == "quali") {
      if (Test %nin% c("fisher", "chisq", "none"))
        stop(paste0("Test unadapted to a categorical variable: ", PrintVar(x), ". Choose one of none, fisher or chisq."), call. = FALSE)
    } else if (TypeVar == "quanti") {
      if (Test %nin% c("ztest", "student", "studentvar", "wilcoxon", "none"))
        stop(paste0("Test unadapted to a quantitative variable: ", PrintVar(x), ". Choose one of none, ztest, student or studentvar."), call. = FALSE)
    }
  } else if (NClasses > 2) {
    if (TypeVar == "binaire") {
      if (Test %nin% c("fisher", "chisq", "none"))
        stop(paste0("Test unadapted to a binary variable: ", PrintVar(x), ". Choose one of none, fisher or chisq."), call. = FALSE)
    } else if (TypeVar == "quali") {
      if (Test %nin% c("fisher", "chisq", "none"))
        stop(paste0("Test unadapted to a categorical variable: ", PrintVar(x), ". Choose one of none, fisher or chisq."), call. = FALSE)
    } else if (TypeVar == "quanti") {
      if (Test %nin% c("anova", "kruskal-wallis", "none"))
        stop(paste0("Test unadapted to a quantitative variable: ", PrintVar(x), ". Choose one of none, anova or kruskal-wallis."), call. = FALSE)
    }
  }

  return (Test)
}
