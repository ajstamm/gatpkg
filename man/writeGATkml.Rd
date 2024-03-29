% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeGATkml.R
\name{writeGATkml}
\alias{writeGATkml}
\title{Write GAT KML File}
\usage{
writeGATkml(myshp, filename, filepath, myidvar = "GEOID10")
}
\arguments{
\item{myshp}{A spatial layer.}

\item{filename}{The desired name for the KML file.}

\item{filepath}{The desired location for the KML file.}

\item{myidvar}{A variable of unique values for the spatial layer.}
}
\description{
This function writes a KML file of the spatial layer.
}
\examples{
# creates the kml file in the project root directory
if (interactive()) {
writeGATkml(myshp = hftown, filename = "my_kml_example",
            filepath = getwd(), myidvar = "TOWN")
}
}
