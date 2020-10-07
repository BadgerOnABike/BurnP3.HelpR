#' Burn-P3 Input/Output Assistance Package
#'
#' @description
#' The \code{BurnP3} package is a set of functions for streamlining the generations of inputs for burn probability modelling. As the package evolves it will also include a number of tools to aid in the combination and analysis of outputs.
#'
#' Burn-P3 is a modelling platform that can be found at \href{www.firegrowthmodel.ca}{www.firegrowthmodel.ca}. It is a monte carlo simulator that leverages the Prometheus The Canadian Fire Growth Simulation Model to burn numerous fires based on a series of stochastic inputs to derive a set of products ranging from burn probability to probabilistic secondary fbp outputs, see the \link[cffdrs]{cffdrs} package function \link[cffdrs]{fbp}.
#'
#' Typical workflow will be described below if working from scratch however the package is fully modular and individual components can be used in isolation.
#'
#' @details
#' # 0. Establish Area of Interest and Question
#'
#' While the package will be unable to do most of this work for you, we will describe the inputs you need ready to use this tool effectively.
#'
#' The key inputs needed prior to use of this package:
#' * Area of Interest - polygon
#' * Base raster for your area - raster
#' * Data frame of Weather Information - data.frame
#'
#' *__Note__: There is an \link{aoi} function that can assist in generating a polygon for use within the model, however if the Canadian National Parks are not the area of interest a polygon of the area is required.
#'
#' # 1. Establish Directory
#'
#' The \link{bp3_dir_gen} function is set up to generate a work space for Burn-P3 under the current generation windows build (2020-03-31). This will flesh out the basic requirements for any Burn-P3 project. This should be run every time as it will not overwrite or regenerate the directory but does create directory objects for later use. Specifically bp3_base and base_dir. The bp3_base is the directory where the project has been instantiated, base_dir is the working directory.
#'
#'  # 2. Fuel and Elevation File Generation
#'
#'  Fuel and elevation data are key to the operation of any fire growth model. In this case the elevation is automatically generated within Canada from the 30m - CDEM.
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
