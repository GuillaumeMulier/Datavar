#' RenameDF
#'
#' Rename the columns of a data.frame
#'
#' Tool to rename the columns of a data.frame with what is in the column var of the datavar.
#' Useful to correct complicated names or standardize names.
#'
#' @param .Data The dataset.
#' @param .Datavar The datavar.
#'
#' @return The dataset with changed names of columns
#' @export
#'
#' @examples
#' DatavarMtcars$var <- toupper(DatavarMtcars$var)
#' RenameDF(mtcars, DatavarMtcars)
RenameDF <- function(.Data, .Datavar) {

  # Identify columns to replace and create named vector of names to replace
  NamesToReplace <- names(.Data) %in% .Datavar$col_name
  NamesReplacing <- .Datavar$col_name %in% names(.Data)
  NamesReplacing <- setNames(.Datavar$var[NamesReplacing], .Datavar$col_name[NamesReplacing])

  # Replacing the names in the .Data
  names(.Data)[NamesToReplace] <- as.character(NamesReplacing[names(.Data)[NamesToReplace]])

  return(.Data)

}
