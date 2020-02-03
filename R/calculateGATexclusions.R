#' Calculate GAT Exclusions
#'
#' @description
#' This function cleans up the code to flag areas based on exclusion
#' criteria so that they will not be used in the merge.
#'
#' I will need to add an example later.
#'
#' @param d    The dataset containing the flag and exclusion variables.
#' @param var  The variable to use when calculating exclusions.
#' @param math The condition to use when calculating exclusions. Recognized
#'             conditions include {"equals", "less than", "greater than"}.
#' @param val  The value to compare to `var` based on the condition selected.
#'
#' @export

calculateGATexclusions <- function(d, var, math, val) {
  if (math == "equals") {
    d$GATflag <- ifelse(d[, var] == val, 1, d$GATflag)
  } else if (math == "less than") {
    d$GATflag <- ifelse(d[, var] < val, 1, d$GATflag)
  } else if (math == "greater than") {
    d$GATflag <- ifelse(d[, var] > val, 1, d$GATflag)
  }
  return(d)
}

