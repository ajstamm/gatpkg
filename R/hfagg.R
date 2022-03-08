#' Hamilton & Fulton counties aggregated spatial polygon data frame
#'
#' @description
#' These objects were created by aggregating a US census shapefile of towns
#' in Hamilton and Fulton Counties in New York State. The settings used were:
#'
#' * hfagg610k (aggregated object) and hfcw610k (crosswalk)
#'     * aggregate hftown total population to at least 6,000 and at most 10,000
#'     * maintain/require county borders
#'     * aggregate by closest geographic centroid
#'     * do not exclude any areas
#' * hfagg615k (aggregated object) and hfcw615k (crosswalk)
#'     * aggregate hfagg610k total population to at least 6,000 and at most
#'       15,000
#'     * request county borders, but do not require them
#'     * aggregate by closest geographic centroid
#'     * do not exclude any areas
#'
#' @format SpatialPolygonsDataFrame objects with the following structures.
#'
#' Aggregated objects:
#'
#' \describe{
#' \item{\code{ID}}{Census county and town FIPS designation}
#' \item{\code{TOWN}}{Town name}
#' \item{\code{COUNTY}}{Census county FIPS designation}
#' \item{\code{TOTAL_POP}}{Census population for 2010}
#' \item{\code{GATflag}}{Placeholder for excluded areas, if any}
#' \item{\code{GATx}}{Recalculated longitude}
#' \item{\code{GATy}}{Recalculated latitude}
#' \item{\code{GATnumIDs}}{number of towns aggregated to the area}
#' }
#'
#' Crosswalk objects:
#'
#' \describe{
#' \item{\code{ID}}{Census county and town FIPS designation}
#' \item{\code{TOWN}}{Town name}
#' \item{\code{COUNTY}}{Census county FIPS designation}
#' \item{\code{TOTAL_POP}}{Census population for 2010}
#' \item{\code{GATflag}}{Placeholder for excluded areas, if any}
#' \item{\code{GATid}}{GAT-generated ID of assigned aggregated area}
#' }
#'
#' @export
"hfagg610k"
"hfagg615k"
"hfcw610k"
"hfcw615k"
