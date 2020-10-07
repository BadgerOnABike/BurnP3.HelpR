#' Burn-P3 Input/Output Assistance Package
#'
#' @description The \code{BurnP3} package is a set of functions for streamlining the generations of inputs for burn probability modelling. As the package evolves it will also include a number of tools to aid in the combination and analysis of outputs.
#'
#' Burn-P3 is a modelling platform that can be found at \href{www.firegrowthmodel.ca}{www.firegrowthmodel.ca}. It is a monte carlo simulator that leverages the Prometheus The Canadian Fire Growth Simulation Model to burn numerous fires based on a series of stochastic inputs to derive a set of products ranging from burn probability to probabilistic secondary fbp outputs, see the cffdrs package function \link[cffdrs]{fbp}.
#'
#' Typical workflow will be described below if working from scratch however the package is fully modular and individual components can be used in isolation.
#'
#' @details
#' ## 0. Establish Area of Interest and Question
#'
#' While the package will be unable to do most of this work for you, we will describe the inputs you need ready to use this tool effectively.
#'
#' The key inputs needed prior to use of this package:
#' * Area of Interest
#' * Base raster for your area
#' * Dataframe of Weather Information
#'
#'
#'
#' @docType package
#'
#' @author Brett Moore \email{Brett.Moore@@canada.ca}
#'
#' @name BurnP3-package
#' @aliases BurnP3 BurnP3-package
#'
#'
# Removed DMwR from import
NULL
