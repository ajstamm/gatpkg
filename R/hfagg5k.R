#' Hamilton & Fulton counties aggregated spatial polygon data frame
#'
#' @description
#' This object was aggregated from a US census shapefile of towns in Hamilton
#' and Fulton Counties in New York State. The settings used were:
#'
#'  * aggregate total population to at least 5,000
#'  * maintain county borders
#'  * aggregate by similar ratio of land to water
#'  * do not exclude any areas
#'
#' @format A SpatialPolygonsDataFrame object that includes the following
#'   11 variables:
#'
#' \describe{
#' \item{\code{ID}}{Census county and town FIPS designation}
#' \item{\code{TOWN}}{Town name}
#' \item{\code{COUNTY}}{Census county FIPS designation}
#' \item{\code{AREALAND}}{Land area}
#' \item{\code{AREAWATR}}{Water area}
#' \item{\code{TOTAL_POP}}{Census population for 2010}
#' \item{\code{MY_FLAG}}{Arbitrary flag variable to illustrate exclusions}
#' \item{\code{GATflag}}{Placeholder for excluded areas, if any}
#' \item{\code{GATx}}{Recalculated longitude}
#' \item{\code{GATy}}{Recalculated latitude}
#' \item{\code{GATnumIDs}}{number of towns aggregated to the area}
#' }
#'
#' @export
"hfagg5k"

