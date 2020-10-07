#' Burn-P3 Input/Output Assistance Package
#'
#' @description
#' \tab The \code{BurnP3} package is a set of functions for streamlining the generations of inputs for burn probability modelling. As the package evolves it will also include a number of tools to aid in the combination and analysis of outputs.
#'
#' \tab Burn-P3 is a modelling platform that can be found at \href{www.firegrowthmodel.ca}{www.firegrowthmodel.ca}. It is a monte carlo simulator that leverages the Prometheus The Canadian Fire Growth Simulation Model to burn numerous fires based on a series of stochastic inputs to derive a set of products ranging from burn probability to probabilistic secondary fbp outputs, see the \link[cffdrs]{cffdrs} package function \link[cffdrs]{fbp}.
#'
#' \tab Typical workflow will be described below if working from scratch however the package is fully modular and individual components can be used in isolation.
#'
#' @details
#' # 0. Establish Area of Interest and Question
#'
#' \tab While the package will be unable to do most of this work for you, we will describe the inputs you need ready to use this tool effectively.
#'
#' The key inputs needed prior to use of this package:
#' * Area of Interest - polygon
#' * Base raster for your area - raster
#' * Data frame of Weather Information - data.frame
#'
#' *_*Note*_: There is an \link[aoi] function that can assist in generating a polygon for use within the model, however if the Canadian National Parks are not the area of interest a polygon of the area is required.
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
"_PACKAGE"
