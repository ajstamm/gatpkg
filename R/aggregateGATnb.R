#' Aggregate GAT neighbors
#'
#' @description
#' This function is a clone of spdep::aggregate.nb with one
#' minor modification - it does not remove areas without neighbors,
#' because in GAT, not having neighbors matters.
#'
#' @param nb          Neighbors object.
#' @param ids         Vector of IDs corresponding to the neighbor object's
#'                    "region.id" attribute.
#' @param remove.self Boolean denoting whether to remove original areas.
#'
#' @examples
#' nb <- spdep::poly2nb(hftown, queen = FALSE,
#'                      row.names = rownames(hftown@data))
#' # neighbors of areas being aggregated
#' nb[[12]]; nb[[13]]; nb[[20]]
#' ids <- c(1:12, 12, 14:19, 12, 21)
#' # neighbors of resulting area
#' aggregateGATnb(nb, ids)[[12]]
#'
#' @export

aggregateGATnb <- function (nb, ids, remove.self = TRUE) {
  stopifnot(length(nb) == length(ids))
  in_reg.ids <- attr(nb, "region.id")
  mtch <- tapply(in_reg.ids, ids, function(i) c(i))
  out_reg.ids <- names(mtch)
  nb_short <- vector(mode = "list", length = length(mtch))
  for (i in seq(along = mtch)) {
    nb_short[[i]] <- 0L
    imtch <- match(mtch[[i]], in_reg.ids)
    res <- unlist(nb[imtch])
    nb_short[[i]] <- as.integer(sort(unique(match(ids[res],
                                                  out_reg.ids))))
    if (remove.self && i %in% nb_short[[i]]) {
      nb_short[[i]] <- nb_short[[i]][-(match(i, nb_short[[i]]))]
      if (length(nb_short[[i]]) < 1L)
        nb_short[[i]] <- 0L
    }
  }
  nb_short[sapply(nb_short, length) == 0L] <- 0L
  attr(nb_short, "region.id") <- out_reg.ids
  class(nb_short) <- "nb"
  # remove areas without neighbors
  # commented out as doing this messes up GAT
  #if (any(spdep::card(nb_short) == 0L) & any(spdep::card(nb_short) > 0L)) {
  #  nb_short <- subset(nb_short, spdep::card(nb_short) > 0L)
  #}
  nb_short <- spdep::sym.attr.nb(nb_short)
  return(nb_short)
}
