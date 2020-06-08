#' Function to call TPI, TRI and flowdir
#'
#' A set of three functions wrapped in a single call to generate Topographic Position Index, Topographic Ruggedness Index and a flow layer. While \code{terrain} can perform all of these actions, some have been exported to apply a window factor for manipulable use.
#'
#' @details Explanatory variables that are often used during the ignition gridding process are TPI, TRI and flow. These parameters give us an idea the influence of the landscape on ignition potential. Additional information about the calculation and interpretation of topographic position index can be found at: \link{http://www.jennessent.com/downloads/tpi-poster-tnc_18x22.pdf} and in \code{\link[raster]{terrain}}
#'
#' @param input The elevation grid that will be used during modelling. It may also be larger in order to avoid edge effects if necessary.
#' @param window_size Must be an odd number
#'
#' @return RasterStack
#' @export
#'
#' @import raster
#'
#' @examples
#'
#' ##Load in example data
#' elev <- raster(system.file("extdata/elev.tif",package="BurnP3"))
#' tpi.out <- tpi.bp3(elevation_grid = elev, window_size = 5)
#' plot(tpi.out)

tpi.bp3 <- function(input, window_size = 5){

  if( grepl("RasterLayer", class(reference_grid)) ){ grast <- reference_grid }
  if( grepl("character", class(reference_grid)) ){ grast <- raster(reference_grid) }
  if( !grepl("RasterLayer|character", class(reference_grid)) ){ message("Reference Grid must be the directory of the raster or a raster object.") }

  tpi <- .tpi_w(input=elev,window_size)
  tri <- .TRI(input=elev, window_size)
  flow <- terrain(x=elev,opt="flowdir")
  out.r <- stack(tpi,tri,flow)
  names(out.r) <- c("tpi","tri","flowdir")
  return(out.r)
}

#' Function to actually calculate topographic position index with a window
#' @param input The elevation grid that will be used during modelling. It may also be larger in order to avoid edge effects if necessary.
#' @param window_size Must be an odd number
tpi_w <- function(input, window_size=5) {
  m <- matrix(1/(window_size^2-1), nc=window_size, nr=window_size)
  m[ceiling(0.5 * length(m))] <- 0
  f <- focal(x, m)
  x - f
}
#' Function calculate topographic roughness index with a window
#' @param input The elevation grid that will be used during modelling. It may also be larger in order to avoid edge effects if necessary.
#' @param window_size Must be an odd number
TRI <- function(input,window_size){

  f <- matrix(1, nc=window_size, nr=window_size)
  f[ceiling(0.5 * length(f))] <- 0

  focal(x, f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)
}


