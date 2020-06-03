#' Kernal Density Calculator
#'
#' Generate kernal density layers from a list of layers and write the rasters out
#'
#'
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.Can be either the location of the raster or a raster object.
#' @param layer A point layer for which you are interested in the density.
#' @param output_location The location the output density rasters will be placed.
#' @param output_name The name of the files to be output. Typically these will be a vector of the names of your list of layers.
#'
#' @details This function utilizes the kernal density estimation algorithm from spatialEco to create raster layers for use in ignition grid calculation.
#'
#' @return RasterLayer
#'
#' @seealso \code{\link[BurnP3]{spatdat_ign_layer}}
#' @seealso \code{\link[spatialEco]{sp.kde}}
#'
#' @import raster
#' @import rgdal
#' @import spatialEco
#'
#' @examples
#'
#' ## Load example data
#' ref_grid <- raster(system.file("extdata/fuel.tif",package = "BurnP3"))
#' fires <- readOGR(system.file("extdata/fires.shp",package = "BurnP3"))
#' temp_dir <- tempdir()
#' density_test <- density_ign(reference_grid = ref_grid,
#'                             layer = fires,
#'                             output_location = temp_dir,
#'                             output_name = "density_test")
#'
#' plot(density_test)
#'
#' unlink(temp_dir)
density_ign <- function(reference_grid,layer,output_location,output_name){

  if( grepl("RasterLayer", class(reference_grid)) ){ grast <- reference_grid }
  if( grepl("character", class(reference_grid)) ){ grast <- raster(reference_grid) }
  if( !grepl("RasterLayer|character", class(reference_grid)) ){ message("Reference Grid must be the directory of the raster or a raster object.") }

  density.r <- sp.kde(crop(layer,grast),bw = 1000,newdata = grast)
  writeRaster(mask(density.r, grast),
              paste0(output_location,"/",output_name,"_density.tif"),
              format="GTiff",
              overwrite =T)
}
