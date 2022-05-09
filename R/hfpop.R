#' Hamilton & Fulton counties block group-level population spatial layer
#'
#' This object was created from a census block group file and a town file. The
#' block groups were intersected by town borders and the populations were split
#' proportionally among the resulting areas, then the geographic centroids
#' were calculated for these areas.
#'
#' @format A simple feature (sf) object that includes the following
#'   5 variables:
#'
#' \describe{
#' \item{\code{Pop}}{Full 2010 census population for the block}
#' \item{\code{pop}}{Proportional population for the block group segment}
#' \item{\code{geometry}}{The polygon nodes, bounding box, and projection}
#' \item{\code{x}}{The geographic centroid longitude}
#' \item{\code{y}}{The geographic centroid latitude}
#' }
#'
#' @export
"hfpop"

