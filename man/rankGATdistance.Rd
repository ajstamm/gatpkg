% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rankGATdistance.R
\name{rankGATdistance}
\alias{rankGATdistance}
\title{Rank GAT Distance}
\usage{
rankGATdistance(area, nbdata, first, gatvars, mergevars)
}
\arguments{
\item{area}{A spatial polygons data frame.}

\item{nbdata}{A data frame of (polygon) observations.}

\item{first}{A data frame with one (polygon) observation.}

\item{gatvars}{A list of objects created by the GAT tool. It contains the
strings myidvar, aggregator1, aggregator2, and boundary,
which are all variables in the area, and the numbers
minvalue1 and minvalue2. Both aggregator1 and aggregator2
must be numeric. The identifier, myidvar, must contain
unique values.}

\item{mergevars}{A list of string objects needed to aggregate the areas in
the GAT tool. It contains mergeopt1, mergeopt2, similar1,
and similar2. The valid options for mergeopt1 and
mergeopt2 are "closest", "least", and "similar". If
"similar" is selected, similar1 and similar2 must be
numeric variables in the area and similar2 cannot equal
zero.}
}
\description{
This function orders the distance to neighboring polygons' centroids from
nearest to farthest and returns a vector of integers.
}
\examples{
# add GAT variables
sf::st_agr(hftown) <- "constant"
centroids <- sf::st_coordinates(sf::st_geometry(sf::st_centroid(hftown)))
colnames(centroids) <- c("GATx", "GATy")
my_data <- data.frame(hftown, centroids)
my_data <- sf::st_as_sf(my_data, coords = c("GATx", "GATy"),
                        crs = sf::st_crs(hftown))
first <- my_data[which(grepl("37374", my_data$ID)), ] # only one observation

# hard coded for simplicity; use sfdep::st_contiguity to get these obs
nbdata <- my_data[which(grepl("43412|02572|40794|79059", my_data$ID)), ]
          # only adjacent neighbors to first

gatvars <- list(
  myidvar = "ID",             # character variable of unique values
  aggregator1 = "TOTAL_POP",  # numeric variable
  aggregator2 = "TOTAL_POP",  # numeric variable
  minvalue1 = 5000, minvalue2 = 5000,
  boundary = "COUNTY"         # character variable of non-unique values
)

mergevars <- list(
  mergeopt1 = "similar",    # can be similar, closest, or least
  mergeopt2 = "closest",    # if "similar" fails
  similar1 = "B_TOT",       # numeric variable
  similar2 = "W_TOT"        # numeric variable without any zeros
)
# rank the distances
my_rank_list <- rankGATdistance(area = hftown, nbdata = nbdata,
                                first = first, gatvars = gatvars,
                                mergevars = mergevars)

}
