#' Euclidean Distance Calculator
#'
#' Generate euclidean distance layers from a list of rasters and write the rasters out
#'
#'
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.Can be either the location of the raster or a raster object.
#' @param rasters_list    The location the output density rasters will be placed.
#' @param output_location The name of the files to be output. Typically these will be a vector of the names of your list of layers.
#' @param output_name
#'
#' @details This function generates a euclidean distance grid with the \code{distance} function.
#'
#' @return RasterLayer
#' @export
#'
#' @examples
#' ## Load example data
#' ref_grid <- raster(system.file("extdata/fuel.tif",package = "BurnP3.HelpR"))
#' load(system.file("data/spatdat_rast_out.rda",package = "BurnP3.HelpR"))
#' temp_dir <- tempdir()
#' distance_test <- distance_ign(reference_grid = ref_grid,
#'                             rasters_list = raster_list$rasters,
#'                             output_location = temp_dir,
#'                             output_name = "distance_test")
#'
#' plot(distance_test)
#'
#' unlink(temp_dir,recursive = T)
distance_ign <- function(reference_grid, rasters_list,output_location,output_name){

  if ( dir.exists(output_location) == F ) { dir.create(output_location)}
  if ( grepl("RasterLayer", class(reference_grid)) ) { grast <- reference_grid }
  if ( grepl("character", class(reference_grid)) ) { grast <- raster(reference_grid) }
  if ( !grepl("RasterLayer|character", class(reference_grid)) ) { message("Reference Grid must be the directory of the raster or a raster object.") }

  distance.r <- distance(rasters_list)
  writeRaster(mask(distance.r,grast),
              paste0(output_location,"/",output_name,"_distance.tif"),
              format = "GTiff",
              overwrite = T)

}
