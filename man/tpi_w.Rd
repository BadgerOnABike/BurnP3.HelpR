% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tpi.bp3.R
\name{tpi_w}
\alias{tpi_w}
\title{Function to calculate topographic position index with a window}
\usage{
tpi_w(input, window_size = 5)
}
\arguments{
\item{input}{The elevation grid that will be used during modelling. It may also be larger in order to avoid edge effects if necessary.}

\item{window_size}{Must be an odd number}
}
\value{
SpatRaster
}
\description{
Function to calculate topographic position index with a window
}
