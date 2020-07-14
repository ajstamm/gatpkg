#' Hamilton & Fulton counties town-level spatial polygon data frame
#'
#' This object was subsetted from a US census shapefile of towns in Hamilton
#' and Fulton Counties in New York State.
#'
#' @format A SpatialPolygonsDataFrame object that includes the following
#'   7 variables:
#'
#' \describe{
#' \item{\code{TOWN}}{Town name}
#' \item{\code{ID}}{Census county and town FIPS designation}
#' \item{\code{COUNTY}}{Census county FIPS designation}
#' \item{\code{AREALAND}}{Land area}
#' \item{\code{AREAWATR}}{Water area}
#' \item{\code{TOTAL_POP}}{Census population for 2010}
#' \item{\code{MY_FLAG}}{Arbitrary flag variable to illustrate exclusions}
#' }
#'
#' @export
"hftown"
