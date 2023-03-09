#' Calculate GAT Flag
#'
#' @description
#' This function cleans up the code to update the flag variable based on
#' exclusion criteria.
#'
#'
#' @param d       The dataset containing the flag and exclusion variables.
#' @param exclist The vector of settings to use, including variable names
#'                {"var1", "var2", "var3"}, the criteria {"math1", "math2",
#'                "math3"}, and the numeric values {"val1", "val2", "val3"}.
#'
#' @examples
#' exclist <- list(
#'   var1 = "TOTAL_POP",
#'   math1 = "less than",
#'   val1 = 500,
#'   var2 = "NONE", # if not "NONE", define math2 & val2
#'   var3 = "NONE"  # if not "NONE", define math3 & val3
#' )
#'
#' calculateGATflag(exclist = exclist, d = hftown)
#'
#' @export

calculateGATflag <- function(exclist, d) {
  if (!"GATflag" %in% names(d)) {
    d$GATflag <- 0 # for non-default uses of this function
  }

  if (exclist$var1 != "NONE") {
    d <- calculateGATexclusions(d, exclist$var1, exclist$math1, exclist$val1)
  }
  if (exclist$var2 != "NONE") {
    d <- calculateGATexclusions(d, exclist$var2, exclist$math2, exclist$val2)
  }
  if (exclist$var3 != "NONE") {
    d <- calculateGATexclusions(d, exclist$var3, exclist$math3, exclist$val3)
  }
  return(d$GATflag)
}

