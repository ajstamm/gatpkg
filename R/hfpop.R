#' Hamilton & Fulton block-level population spatial polygon data frame
#'
#' This object was created from a census block file and a town file. The
#' blocks were intersected by town borders and the populations were split
#' proportionally among the resulting areas, then the geographic centroids
#' were calculated for these areas.
#'
#' @format A SpatialPolygonsDataFrame object that includes the following
#'   4 variables:
#'
#' \describe{
#' \item{\code{Pop_tot}}{Full 2010 census population for the block}
#' \item{\code{pop}}{Proportional population for the block segment}
#' \item{\code{x}}{The geographic centroid longitude}
#' \item{\code{y}}{The geographic centroid latitude}
#' }
#'
#' @export
"hfpop"

