% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transects.R
\name{get_pts_and_transects}
\alias{get_pts_and_transects}
\title{Get points on line}
\usage{
get_pts_and_transects(linestring, distance, length)
}
\arguments{
\item{linestring}{A data.frame of sf_LINESTRING}

\item{distance}{A \code{numeric} indicating distance between transects}

\item{length}{A \code{numeric} indicating the length of the transect}
}
\description{
set-up function for getting the points on the linestring
where the transects cross
}
