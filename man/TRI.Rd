% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tpi.bp3.R
\name{TRI}
\alias{TRI}
\title{Function calculate topographic roughness index with a window}
\usage{
TRI(input, window_size)
}
\arguments{
\item{input}{The elevation grid that will be used during modelling. It may also be larger in order to avoid edge effects if necessary.}

\item{window_size}{Must be an odd number}
}
\value{
SpatRaster
}
\description{
Function calculate topographic roughness index with a window
}
