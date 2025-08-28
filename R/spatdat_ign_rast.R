#' Spatial Data Rasterizer
#'
#' A function to create a point layer and a raster layer out of spatial inputs (point, line, polygon) for use during ignition grid generation. These layers are intended for use in the density \code{density_ign} and euclidean distance \code{distance_ign} functions.
#'
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.
#' @param layers_list A list containing at least one spatial layer from the \code{spatdat_ign_layer} function to be rasterized.
#'
#' @importFrom terra rast cells classify setValues vect
#' @importFrom sf st_read st_crs st_transform st_centroid st_as_sf
#'
#' @return List containing points and raster layers.
#' @export
#'
#' @seealso \code{\link[BurnP3.HelpR]{spatdat_ign_layer}}
#'
#' @examples
#'
#' ## Load example data
#' ref_grid <- terra::rast(system.file("extdata/fuel.tif",package = "BurnP3.HelpR"))
#' layers_list <- list(sf::st_read(dsn=system.file("extdata/extdata.gpkg",
#'                             package = "BurnP3.HelpR"),layer="layers_list"))
#' out <- spatdat_ign_rast(reference_grid = ref_grid,
#'                         layers_list = layers_list)
#'
spatdat_ign_rast <- function(reference_grid, layers_list){

  if ( grepl("SpatRaster", class(reference_grid)) ) {grast <- reference_grid}
  if ( grepl("character", class(reference_grid)) ) {grast <- terra::rast(reference_grid)}
  if ( !grepl("SpatRaster|character", class(reference_grid)) ) {message("Reference Grid must be the directory of the raster or a SpatRaster object.")}

  rasters_list <- lapply(X = layers_list,
                         FUN = function(x) {
                           print(x)
                           out.r = terra::setValues(grast,0)
                           if (grepl("line",unique(st_geometry_type(x)),ignore.case = T)) {
                             cells_in <- terra::cells(x = terra::setValues(grast,0),
                                            y = vect(x))[,"cell"]
                           }
                           if (grepl("polygon",unique(st_geometry_type(x)),ignore.case = T)) {
                             cells_in <-  terra::cells(x = terra::setValues(grast,0),
                                                       y = vect(x))[,"cell"]
                             if (is.null(cells_in)) x <- sf::st_as_sf(sf::st_centroid(x),proj4string = sf::st_crs(grast))
                           }

                           if (grepl("point",unique(st_geometry_type(x)),ignore.case = T)) {
                             cells_in <- terra::cellFromXY(terra::setValues(grast,0), vect(st_transform(x,crs = st_crs(grast))))
                           }
                           out.r[cells_in] <- 1
                           out.r <- mask(out.r,grast)
                         }
  )

  if (length(rasters_list) == 1) {
    rasters.out <- terra::classify(rasters_list[[1]],rcl = matrix(ncol=3,c(-Inf,0,NA)), right=T)
  } else{
    rasters.out <- terra::classify(
      sum(
        rasters_list,
        na.rm = T),
      rcl = matrix(ncol=3,c(-Inf,0,NA)),
      right=T
    )
  }

  points.out <- sf::st_as_sf(terra::as.points(rasters.out))


  return(list(points = points.out,
              rasters = rasters.out)
  )
}
