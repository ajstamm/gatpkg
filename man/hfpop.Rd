% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hfpop.R
\docType{data}
\name{hfpop}
\alias{hfpop}
\title{Hamilton & Fulton block-level population spatial polygon data frame}
\format{
A simple feature (sf) object that includes the following
5 variables:

\describe{
\item{\code{Pop_tot}}{Full 2010 census population for the block}
\item{\code{pop}}{Proportional population for the block segment}
\item{\code{geometry}}{The polygon nodes, bounding box, and projection}
\item{\code{x}}{The geographic centroid longitude}
\item{\code{y}}{The geographic centroid latitude}
}
}
\usage{
hfpop
}
\description{
This object was created from a census block file and a town file. The
blocks were intersected by town borders and the populations were split
proportionally among the resulting areas, then the geographic centroids
were calculated for these areas.
}
\keyword{datasets}
