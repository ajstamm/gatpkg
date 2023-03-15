#' Hamilton & Fulton counties aggregated to 10k population
#'
#' @description
#' This object was created by aggregating a US census shapefile of towns
#' in Hamilton and Fulton Counties in New York State. The settings used were:
#'
#' * aggregate hftown total population to at least 6,000 and at most 10,000
#' * maintain/require county borders
#' * aggregate by closest geographic centroid
#' * do not exclude any areas
#'
#' @format Simple features object with the following structure.
#'
#' \describe{
#' \item{\code{ID}}{Census county and town FIPS designation}
#' \item{\code{TOWN}}{Town name}
#' \item{\code{COUNTY}}{Census county FIPS designation}
#' \item{\code{TOTAL_POP}}{Census population for 2010}
#' \item{\code{GATflag}}{Flag values for excluded and problematic areas, if any}
#' \item{\code{GATx}}{Recalculated longitude}
#' \item{\code{GATy}}{Recalculated latitude}
#' \item{\code{GATnumIDs}}{number of towns aggregated to the area}
#' }
#'
#' @export
"hfagg610k"

#' Hamilton & Fulton counties aggregated to 15k population
#'
#' @description
#' This object was created by aggregating a US census shapefile of towns
#' in Hamilton and Fulton Counties in New York State. The settings used were:
#'
#' * aggregate hfagg610k total population to at least 6,000 and at most
#'   15,000
#' * request county borders, but do not require them
#' * aggregate by closest geographic centroid
#' * do not exclude any areas
#'
#' @format Simple features object with the following structure.
#'
#' \describe{
#' \item{\code{ID}}{Census county and town FIPS designation}
#' \item{\code{TOWN}}{Town name}
#' \item{\code{COUNTY}}{Census county FIPS designation}
#' \item{\code{TOTAL_POP}}{Census population for 2010}
#' \item{\code{GATflag}}{Flag values for excluded and problematic areas, if any}
#' \item{\code{GATx}}{Recalculated longitude}
#' \item{\code{GATy}}{Recalculated latitude}
#' \item{\code{GATnumIDs}}{number of towns aggregated to the area}
#' }
#'
#' @export
"hfagg615k"

#' Hamilton & Fulton counties aggregation to 10k population crosswalk
#'
#' @description
#' This object was created by aggregating a US census shapefile of towns
#' in Hamilton and Fulton Counties in New York State. The settings used were:
#'
#' * aggregate hftown total population to at least 6,000 and at most 10,000
#' * maintain/require county borders
#' * aggregate by closest geographic centroid
#' * do not exclude any areas
#'
#' @format Simple features object with the following structure.
#'
#' \describe{
#' \item{\code{ID}}{Census county and town FIPS designation}
#' \item{\code{TOWN}}{Town name}
#' \item{\code{COUNTY}}{Census county FIPS designation}
#' \item{\code{TOTAL_POP}}{Census population for 2010}
#' \item{\code{GATflag}}{Flag values for excluded areas, if any}
#' \item{\code{GATid}}{GAT-generated ID of assigned aggregated area}
#' }
#'
#' @export
"hfcw610k"

#' Hamilton & Fulton counties aggregation to 15k population crosswalk
#'
#' @description
#' This object was created by aggregating a US census shapefile of towns
#' in Hamilton and Fulton Counties in New York State. The settings used were:
#'
#' * aggregate hfagg610k total population to at least 6,000 and at most 15,000
#' * request county borders, but do not require them
#' * aggregate by closest geographic centroid
#' * do not exclude any areas
#'
#' @format Simple features object with the following structure.
#'
#' \describe{
#' \item{\code{ID}}{Census county and town FIPS designation}
#' \item{\code{TOWN}}{Town name}
#' \item{\code{COUNTY}}{Census county FIPS designation}
#' \item{\code{TOTAL_POP}}{Census population for 2010}
#' \item{\code{GATflag}}{Flag values for excluded areas, if any}
#' \item{\code{GATid}}{GAT-generated ID of assigned aggregated area}
#' }
#'
#' @export
"hfcw615k"
