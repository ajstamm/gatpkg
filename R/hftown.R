#' Hamilton & Fulton counties town-level spatial polygon data frame
#'
#' This object was subsetted from a US census shapefile of towns in Hamilton
#' and Fulton Counties in New York State.
#'
#' @format A SpatialPolygonsDataFrame object that includes the following
#'   11 variables:
#'
#' \describe{
#' \item{\code{NAME_SHORT}}{Town name}
#' \item{\code{ID}}{Census county and town FIPS designation}
#' \item{\code{COUNTY}}{Census county FIPS designation}
#' \item{\code{AREALAND}}{Land area}
#' \item{\code{AREAWATR}}{Water area}
#' \item{\code{TOTAL_POP}}{Census population for 2010}
#' \item{\code{W_TOT}}{Census white population for 2010}
#' \item{\code{B_TOT}}{Census black population for 2010}
#' \item{\code{HISP}}{Census Hispanic population for 2010}
#' \item{\code{LATITUDE}}{Latitude}
#' \item{\code{LONGITUDE}}{Longitude}
#' }
#'
#' @export
"hftown"
