#' Spatial Data Rasterizer
#'
#' A function to create a point layer and a raster layer out of spatial inputs (point, line, polygon) for use during ignition grid generation. These layers are intended for use in the density \code{density_ign} and euclidean distance \code{distance_ign} functions.
#'
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.
#' @param layer A list containing at least one spatial layer from the \code{spatdat_ign_layer} function to be rasterized.
#'
#' @importFrom raster raster cellFromLine cellFromPolygon reclassify rasterToPoints setValues stack
#' @importFrom sf SpatialPointsDataFrame proj4string
#' @importFrom rgoes gCentroid
#'
#' @return List containing points and raster layers.
#' @export
#'
#' @seealso \code{\link[BurnP3]{spatdat_ign_layer}}
#'
#' @examples
#'
#' ## Load example data
#' ref_grid <- raster(system.file("extdata/fuel.tif",package = "BurnP3"))
#' layers_list <- list(readOGR(dsn=system.file("extdata/extdata.gpkg",package = "BurnP3"),layer="layers_list"))
#' out <- spatdat_ign_rast(reference_grid = ref_grid,
#'                         layers_list = layers_list)
#'
spatdat_ign_rast <- function(reference_grid, layers_list){

  if ( grepl("RasterLayer", class(reference_grid)) ) {grast <- reference_grid}
  if ( grepl("character", class(reference_grid)) ) {grast <- raster(reference_grid)}
  if ( !grepl("RasterLayer|character", class(reference_grid)) ) {message("Reference Grid must be the directory of the raster or a raster object.")}

  rasters_list <- lapply(X = layers_list,
                         FUN = function(x) {
                           print(x)
                           out.r = setValues(grast,0)
                           if (grepl("line",class(x)[1],ignore.case = T)) {
                             cells_in <- unlist(
                               cellFromLine(object = setValues(grast,0),
                                            lns = x)
                             )
                           }
                           if (grepl("polygon",class(x)[1],ignore.case = T)) {
                             cells_in <- unlist(
                               cellFromPolygon(object = setValues(grast,0),
                                               p = x,
                                               weights = F)
                             )
                             if (is.null(cells_in)) x <- SpatialPoints(gCentroid(x),proj4string = CRS(proj4string(grast)))
                           }

                           if (grepl("point",class(x)[1],ignore.case = T)) {
                             cells_in <- cellFromXY(setValues(grast,0), spTransform(x,CRSobj = proj4string(grast)))
                           }
                           out.r[cells_in] <- 1
                           out.r <- mask(out.r,grast)
                         }
  )

  if (length(rasters_list) == 1) {
    rasters.out <- reclassify(rasters_list[[1]],rcl = c(-Inf,0,NA))
  } else{
    rasters.out <- reclassify(
      sum(
        stack(rasters_list),
        na.rm = T),
      rcl = c(-Inf,0,NA)
    )
  }

  points.out <- SpatialPointsDataFrame(rasterToPoints(rasters.out)[,c("x","y")],
                                       data = data.frame(rasterToPoints(rasters.out)),
                                       proj4string = CRS(proj4string(grast))
  )

  return(list(points = points.out,
              rasters = rasters.out)
  )
}
