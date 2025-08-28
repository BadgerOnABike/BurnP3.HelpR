#' Euclidean Distance Calculator
#'
#' Generate euclidean distance layers from a list of spatrasters and write the spatrasters out
#'
#'
#' @param reference_grid This is a reference spatraster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.Can be either the location of the raster or a raster object.
#' @param rasters_list    The location the output density rasters will be placed.
#' @param output_location The name of the files to be output. Typically these will be a vector of the names of your list of layers.
#' @param output_name Name of the output file
#'
#' @details This function generates a euclidean distance grid with the \code{distance} function.
#'
#' @importFrom terra rast writeRaster mask distance
#'
#' @return SpatRaster
#' @export
#'
#' @examples
#' ## Load example data
#' ref_grid <- terra::rast(system.file("extdata/fuel.tif",package = "BurnP3.HelpR"))
#' data("interest_raster")
#' interest_raster <- terra::unwrap(interest_raster)
#' temp_dir <- tempdir()
#' distance_test <- distance_ign(reference_grid = ref_grid,
#'                             rasters_list = interest_raster,
#'                             output_location = temp_dir,
#'                             output_name = "distance_test")
#'
#' terra::plot(distance_test)
#'
#' unlink(temp_dir)
distance_ign <- function(reference_grid, rasters_list,output_location,output_name){

  if ( dir.exists(output_location) == F ) { dir.create(output_location)}
  if ( grepl("SpatRaster", class(reference_grid)) ) { grast <- reference_grid }
  if ( grepl("character", class(reference_grid)) ) { grast <- terra::rast(reference_grid) }
  if ( !grepl("SpatRaster|character", class(reference_grid)) ) { message("Reference Grid must be the directory of the raster or a raster object.") }

  distance.r <- terra::distance(rasters_list)
  terra::writeRaster(terra::mask(distance.r,grast),
              paste0(output_location,"/",output_name,"_distance.tif"),
              wopt = list(filetype = "GTiff",
                          datatype = "FLT4S",
                          gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")),
              overwrite = T)

}
