#' Locate GAT Shapefile
#'
#' @description
#' This function opens a window that asks the user to select a shapefile for
#' aggregation. The file selection window looks like this.
#'
#' \if{html}{\figure{locateGATshapefile.png}{options: width="700px"
#'                   alt="Figure: Screenshot of dialog to select your shapefile"}}
# \if{latex}{\figure{locateshapefile.pdf}{options: width=7in}} # for reference
#'
#' Select your folder and file and click \code{Open}. Clicking \code{Cancel}
#' on this window will trigger a flag to cancel GAT.
#'
#' @details
#' This function returns a list with the following elements.
#'
#' \itemize{\bold{userin: }
#'   The full name and path for the shapefile, without the extension.
#' }
#' \itemize{\bold{filein: }
#'   The name of the shapefile, without the extension.
#' }
#' \itemize{\bold{pathin: }
#'   The full path for the shapefile.
#' }
#'
#' The window may be hidden behind other windows. If so, minimize or close the
#' other windows or click on it to bring it to the front.
#'
#' @param type   String to identify what type of file is being requested.
#'               Accepted values are "aggregation" and "population".
#' @param myfile String denoting default file name and location to open.
#'
#' @examples
#'
#' \donttest{
#' # navigate to and select a shapefile
#' locateGATshapefile()
#'
#' # provide a default location to start
#' locateGATshapefile(
#'   type = "population",
#'   myfile = getwd()
#' )
#'
#' }
#'
#' @export

locateGATshapefile <- function(type = "aggregation", myfile = "") {
  fil <- cbind("Shapefiles", "*.shp") # creates 1x2 matrix
  checkfile <- 100
  if (type == "aggregation") {
    mycaption <- "Select the shapefile to aggregate"
  } else if (type == "population") {
    mycaption <- "Select the population shapefile"
  }
  while (checkfile != 0){
    userfile <- choose.files(filters = fil, caption = mycaption, default = myfile)
    if (length(userfile) > 0) {
      userfile <- gsub("\\\\", "/", userfile) # add "if Windows" tag
      # remove extension if present
      periodloc <- max(unlist(gregexpr(".", userfile, fixed = TRUE)))
                   # will be -1 if no match, otherwise location(s) of matches
      if(periodloc > 0) {
        userfile <- substr(userfile, 1, periodloc[1] - 1)
      }
      checkfile <- file.access(paste0(userfile, ".shp"), mode = 4) # -1 for bad, 0 for OK
      if (checkfile != 0) { # file not found
        msg <- "Sorry, GAT could not find your shapefile. Please select a new shapefile."
        tcltk::tkmessageBox(title = "File error", message = msg,
                            type = "ok", icon = "error")
      }
    } else {
      myfiles <- list(userin = "cancel", filein = "", pathin = "")
      checkfile <- 0
    }
  }
  if (length(userfile) > 0) {
    # make sure file is not too large
    checkfile <- checkGATshapefilesize(userfile)
    # find location of last slash, divides path and file name
    slashloc = max(unlist(gregexpr("/", userfile, fixed = TRUE)))

    # find input file name and path
    filein = substr(userfile, slashloc + 1, nchar(userfile))
    pathin = substr(userfile, 1, slashloc - 1)

    myfiles <- list(userin = userfile, filein = filein, pathin = pathin)
  }
  return(myfiles)
}
