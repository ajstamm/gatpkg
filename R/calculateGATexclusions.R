#' Calculate GAT Exclusions
#'
#' @description
#' This function cleans up the code to flag areas based on exclusion
#' criteria so that they will not be used in the merge.
#'
#'
#' @param d    The dataset containing the flag and exclusion variables.
#' @param var  The variable to use when calculating exclusions.
#' @param math The condition to use when calculating exclusions. Recognized
#'             conditions include {"equals", "less than", "greater than"}.
#' @param val  The value to compare to `var` based on the condition selected.
#'
#' @examples
#'
#' calculateGATexclusions(d = hftown, var = "TOTAL_POP",
#'                        math = "less than", val = 1000)
#'
#' @export

calculateGATexclusions <- function(d, var, math, val) {
  if (!"GATflag" %in% names(d)) {
    d$GATflag <- 0 # for non-default uses of this function
  }
  d$GATflag <- ifelse(d$GATflag == 1, 0, d$GATflag)

  if (math == "equals") {
    d$GATflag <- ifelse(d$GATflag == 0 & data.frame(d)[, var] == val, 1, d$GATflag)
  } else if (math == "less than") {
    d$GATflag <- ifelse(d$GATflag == 0 & data.frame(d)[, var] < val, 1, d$GATflag)
  } else if (math == "greater than") {
    d$GATflag <- ifelse(d$GATflag == 0 & data.frame(d)[, var] > val, 1, d$GATflag)
  }
  return(d)
}

