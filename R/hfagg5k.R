#' Hamilton & Fulton counties aggregated spatial polygon data frame
#'
#' This object was aggregated from a US census shapefile of towns in Hamilton
#' and Fulton Counties in New York State. The settings used were aggregate
#' total population to at least 5,000 and maintain county borders.
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
#' \item{\code{flag}}{Placeholder for excluded areas, if any}
#' \item{\code{GATx}}{Recalculated longitude}
#' \item{\code{GATy}}{Recalculated latitude}
#' \item{\code{num_IDs}}{number of town aggregated to the area}
#' }
#'
#' @export
"hfagg5k"

