#' Check GAT Shapefile Size
#'
#' @description
#' This function is used to determine whether the shapefile is too large for
#' GAT and the user's computer to handle it. This function makes a very
#' conservative estimate that may be refined later.
#'
#' This function returns a number, 0 or 100. If it returns 0, the check passed
#' and you can use the file. If it returns 100, the check failed and you will
#' need to reduce the size or choose a new shapefile.
#'
#' @details
#' Note from Gwen: "I am not sure of the relationship between the size of the
#' R objects produced by file import and the file size. We are assumming 1:1
#' here, but this may not be correct. Size appears to increase at least 10x
#' going from disk to memory."
#'
#' @param userfile A filepath to a shapfile, including filename, without the
#'                 extension.
#' @examples
#'
#' \dontrun{
#' # locate the example shapefile in the package
#' myfile <- paste0(find.package("gatpkg"), "/extdata/hftown")
#'
#' # check the memory size of the shapefile
#' # user only receives a message if there is a problem
#' checkGATshapefilesize(
#'   myfile
#' )
#' }
#'
#' @export

checkGATshapefilesize <- function(userfile) {
  # check the file size compared to memory limit
  mysize <- 10 * file.info(paste0(userfile, ".shp"))[ , "size"] / 1048576 + 10 *
    file.info(paste0(userfile, ".dbf"))[ , "size"] / 1048576
  # size / 1048576 converts bytes to megabytes(Mb)
  # not sure if shape part or dbf will be large - try combining both
  mymem <- memory.limit(size = NA)
  checkfile <- 0
  if (mysize > mymem) {
    msg <- "Warning: your file may be too large for GAT. Do you wish to try it anyway?"
    x <- tcltk::tkmessageBox(title = "Possible file size problem", message = msg,
                             type = "yesno", icon = "question")
    confirm <- tcltk::tclvalue(x)
    if (confirm != "yes") {
      tcltk::tkmessageBox(title = "Filesize too large",
                          message = "Please select a new shapefile.",
                          type = "ok", icon = "info")
      checkfile <- 100
    }
  }
  return(checkfile)
}
