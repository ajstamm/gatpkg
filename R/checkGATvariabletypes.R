#' Check GAT Variable Types
#'
#' This function is used to create a list of either numeric or character
#' variables in the data frame.
#'
#' @param mapdata A data frame, intended to be read from a shapefile DBF.
#' @param type Variable type. Options include "number", which returns a vector
#'             of names for all numeric variables, and "character", which
#'             returns a vector of names for all character variables.
#'
#' @examples
#' # identify numeric variables
#' my_numericvars <-
#'   checkGATvariabletypes(
#'     mapdata = hftown,
#'     type = "number"
#'   )
#'
#' @export

checkGATvariabletypes <- function(mapdata, type = "number") {
  # once read in, get variable names and classes
  # note "count" option gives a vector instead of a logical
  items <- names(mapdata) # need to limit this to numeric variables
  types <- sapply(mapdata, class) # get data classes for all columns
  if (type == "number") {
    vars <- items[grepl("integer|numeric", types)]
    msg = "Sorry, Your shapefile has no numeric fields and won't work in GAT."
  } else if (type == "character") {
    vars <- items[grepl("factor|character", types)]
    msg = "Sorry, Your shapefile has no character fields and won't work in GAT."
  }
  if (length(vars) < 1) {
    tcltk::tkmessageBox(title = "File data problem", message = msg,
                        type = "ok", icon = "error")
    vars <- c()
  }
  return(vars)
}
