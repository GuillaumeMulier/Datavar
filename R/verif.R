
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
                    stop(paste0("Unrecognized language: ", Langue, "\nChoose betwwen: 'fr', 'eng'."), call. = FALSE)
                  }
                  return(Langue)
                },
                Prec = function(Prec) {
                  if (length(Prec) != 1 || !is.numeric(Prec) || Prec %% 1 != 0 || Prec < 0)
                    stop("\"Prec\" should be a whole positive number.", call. = FALSE)
                  return(paste0("%.", Prec, "f"))
                },
                ChifPval = function(ChifPval) {
                  if (length(ChifPval) != 1 || !is.numeric(ChifPval) || ChifPval %% 1 != 0 || ChifPval < 0)
                    stop("\"ChifPval\" should be a whole positive number.", call. = FALSE)
                  return(ChifPval)
                },
                PMissing = function(PMissing) {
                  if (!is.null(PMissing) && (length(PMissing) != 1 || !is.numeric(PMissing) || PMissing %% 1 != 0 || PMissing < 0))
                    stop("\"PMissing\" should be a whole positive number or NULL.", call. = FALSE)
                  return(ChifPval)
                },
                NomCateg = function(NomCateg, NomLabel, VarBinaire, x) {
                  if (!is.null(NomCateg)) {
                    if (NomCateg == "") {
                      NomCateg <- NULL
                    } else if (NomCateg %nin% names(table(VarBinaire, useNA = "no"))) {
                      stop(paste0("Class \"", NomCateg, "\" isn't in variable \"", rlang::quo_name(x),
                                  "\".\nPossible classes: ", paste(names(table(VarBinaire, useNA = "no")), collapse = " / "), "."),
                           call. = FALSE)
                    }
                  } else {
                    NomCateg <- names(table(VarBinaire, useNA = "no"))[length(names(table(VarBinaire, useNA = "no")))]
                    message(paste0("\"NomCateg\" of variable \"", rlang::quo_name(x), "\" isn't specified, thus it is taken to be ",
                                   NomCateg, ".\nCheck if the name of the label (", if (is.null(NomLabel)) "NULL" else NomLabel,
                                   ") corresponds to it."))
                  }
                  return(NomCateg)
                },
                NomLabel = function(NomLabel, VarBinaire, x) {
                  if (is.null(NomLabel)) {
                    NomLabel <- paste0(rlang::quo_name(x), "=", names(table(VarBinaire, useNA = "no"))[length(names(table(VarBinaire, useNA = "no")))])
                    warning(
                      paste0("Unspecified label for variable \"", rlang::quo_name(x), "\".\nDefault label will be \"",
                             NomLabel, "\"."),
                      call. = FALSE
                    )
                  }
                  return(NomLabel)
                },
                NomVariable = function(NomVariable, x) {
                  if (is.null(NomVariable)) {
                    return(paste0(rlang::quo_name(x)))
                  } else {
                    if (!is.character(NomVariable) || length(NomVariable) != 1)
                      stop(paste0("Variable \"", PrintVar(rlang::quo_name(x)), "\"'s NomVariable should be a string designing the name you want to display."), call. = FALSE)
                    return(NomVariable)
                  }
                },
                VarBinaire = function(VarBinaire, NomCateg, x) {
                  if (sum(!is.na(VarBinaire)) == 0)
                    stop(paste0("Variable \"", rlang::as_label(x), "\" has 0 non missing values."), call. = FALSE)
                  if (length(unique(VarBinaire[!is.na(VarBinaire)])) > 2) {
                    stop(paste0("The variable to describe \"", rlang::quo_name(x), "\" isn't a binary variable and has more than 2 classes."), call. = FALSE)
                  } else if (length(unique(VarBinaire[!is.na(VarBinaire)])) == 1) {
                    warning(paste0("The variable to describe \"", rlang::quo_name(x), "\" only have 1 class."))
                  }
                  if (is.factor(VarBinaire))
                    VarBinaire <- as.character(VarBinaire)
                  return(as.numeric(VarBinaire == NomCateg))
                },
                VarQuali = function(VarQuali, Ordonnee) {
                  if (sum(!is.na(VarQuali)) == 0)
                    stop(paste0("Variable \"", rlang::quo_name(x), "\" has 0 non missing values."), call. = FALSE)
                  if (length(unique(VarQuali[!is.na(VarQuali)])) == 1)
                    message(paste0(Information(), " The variable to describe \"", PrintVar(rlang::quo_name(x)), "\" only have 1 class."))
                  if (Ordonnee) {
                    if (!is.factor(VarQuali))
                      VarQuali <- factor(VarQuali, levels = sort(unique(VarQuali[!is.na(VarQuali)])))
                  } else {
                    VarQuali <- factor(as.character(VarQuali), levels = names(sort(table(VarQuali))))
                  }
                  return(VarQuali)
                },
                ConfInter = function(ConfInter) {
                  return(match.arg(ConfInter, c("none", "normal", "exact", "jeffreys")))
                },
                ConfLevel = function(ConfLevel, x) {
                  if (length(ConfLevel) != 1 || !is.numeric(ConfLevel) || ConfLevel <= 0 || ConfLevel >= 1)
                    stop(paste0("\"ConfLevel\" for variable \"", rlang::quo_name(x), "\" must be a unique number between 0 and 1."), call. = FALSE)
                  return(ConfLevel)
                })

  return(Fct(...))

}


#' Check the statistical test
#'
#' @param x String naming the test.
#' @param type_var Type of variable.
#'
VerifTest <- function(Test, TypeVar, NClasses, Variable, y, x) {

  Test <- match.arg(Test, c("none", "student", "ztest", "wilcoxon", "kruskal", "fisher", "chisq", "mcnemar"))

  if (NClasses == 1) {
    stop(paste0("There must be at least 2 classes to perform a crossed description relative to variable \"", rlang::quo_name(y), "\"."), call. = FALSE)
  } else if (NClasses == 2) {
    if (TypeVar == "binaire") {
      if (Test %nin% c("ztest", "mcnemar", "fisher", "chisq", "none"))
        stop(paste0("Test unadapted to a binary variable: ", x), call. = FALSE)
    } else if (TypeVar == "quali") {
      if (Test %nin% c("fisher", "chisq", "none"))
        stop(paste0("Test unadapted to a categorical variable: ", x), call. = FALSE)
    }
  }
  if (Test != "none" & length(unique(Variable[!is.na(Variable)])) == 1) {
    stop(paste0("Variable \"", rlang::quo_name(x), "\" with only 1 class, no feasable test.\nChange to test = \"none\"."), call. = FALSE)
  }
  # if (type_var == "quanti") {
  #   if (x %nin% c("student", "wilcoxon", "none")) stop(paste0("Test unadapted to a quantitative variable: ", x), call. = FALSE)
  # } else if (type_var %in% c("binaire")) {
  #   if (x %nin% c("fisher", "chisq", "none")) stop(paste0("Test unadapted to a binary variable: ", x), call. = FALSE)
  # } else if (type_var %in% c("quali")) {
  #   if (x %nin% c("chisq", "none")) stop(paste0("Test unadapted to a categorial variable: ", x), call. = FALSE)
  # }

  return (Test)
}
