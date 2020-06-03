#' Euclidean Distance Calculator
#'
#' Generate kernal density layers from a list of layers and write the rasters out
#'
#' @author Brett Moore, \email{Brett.Moore@@canada.ca}
#'
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.
#' @param rasters_list    The location the output density rasters will be placed.
#' @param output_location The name of the files to be output. Typically these will be a vector of the names of your list of layers.
#' @param output_name
#'
#' @details This function generates a euclidean distance grid with the \code{distance} function.
#'
#' @return RasterLayer
#' @export
#'
#' @example
#' ## Load example data
#' ref_grid <- raster(system.file("extdata/fuel.tif",package = "BurnP3"))
#' roads <- readOGR(system.file("extdata/roads.shp",package = "BurnP3"))
#' temp_dir <- tempdir()
#' density_test <- density_ign(reference_grid = ref_grid,
#'                             layer = fires,
#'                             output_location = temp_dir,
#'                             output_name = "density_test")
#'
#' plot(density_test)
#'
#' unlink(temp_dir)
distance_ign <- function(reference_grid, rasters_list,output_location,output_name){

  if( grepl("RasterLayer", class(reference_grid)) ){ grast <- reference_grid }
  if( grepl("character", class(reference_grid)) ){ grast <- raster(reference_grid) }
  if( !grepl("RasterLayer|character", class(reference_grid)) ){ message("Reference Grid must be the directory of the raster or a raster object.") }

  distance.r <- distance(rasters_list)
  writeRaster(mask(distance.r,grast),
              paste0(output_location,"/",output_name,"_distance.tif"),
              format="GTiff",
              overwrite =T)

}
