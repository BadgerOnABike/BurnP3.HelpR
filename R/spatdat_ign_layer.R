#' Spatial Data Importer
#'
#' Import spatial data for rasterization in the ignition grid development process.
#'
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.
#' @param layer Names of the input layers for import, allows the system to function if there are multiple shapes at a single data source name.
#' @param dsn Locations of the shapefiles to be loaded in and operated upon. Also accepts a list containing sf objects.
#'
#' @details This tool imports the spatial data for use within the spatial data rasterizer, it can perform on points, lines and polygons.
#'
#' @importFrom terra rast crop
#' @importFrom sf  st_transform st_geometry_type st_is_empty st_zm st_make_valid st_crs
#' @importFrom methods as
#'
#' @return List
#' @export
#'
#'
#' @examples
#'
#' ## Load example data
#' ref_grid <- terra::rast(system.file("extdata/fuel.tif",package = "BurnP3.HelpR"))
#'
#' out <- spatdat_ign_layer(reference_grid = ref_grid,
#'                          layer = "road",
#'                          dsn = system.file("extdata/extdata.gpkg",package="BurnP3.HelpR"))
#'
spatdat_ign_layer <- function(reference_grid,layer,dsn){

  if ( grepl("SpatRaster", class(reference_grid)) ) {grast <- reference_grid}
  if ( grepl("character", class(reference_grid)) ) {grast <- terra::rast(reference_grid)}
  if ( !grepl("SpatRaster|character", class(reference_grid)) ) {message("Reference Grid must be the directory of the raster or a spatraster object.")}


  if (!is.character(layer)) {
    x <- sf::st_transform(layer,crs = st_crs(grast))
    y <- sf::st_geometry_type(x)
    if (length(unique(y)) > 1 & gregexpr("SURFACE",unique(y))[[1]][1] == -1) {
      x <- x[-which(y == "MULTISURFACE"),]
    }
    if (length(sf::st_is_empty(x)) > 0) {
      x <- x[!sf::st_is_empty(x),]
    }
    layers_list <- as(x,"Spatial")
  }else{

    if (!grepl("gdb|gpkg",dsn,ignore.case = T)) {

      x <- sf::st_transform(sf::st_read(x),
                        crs = sf::st_crs(grast))

      x <- sf::st_zm(x)
      y <- sf::st_geometry_type(x)
      if (length(unique(y)) > 1 & gregexpr("SURFACE",unique(y))[[1]][1] == -1) {
        x <- x[-which(y == "MULTISURFACE"),]
      }
      if (length(sf::st_is_empty(x)) > 0) {
        x <- x[!sf::st_is_empty(x),]
      }
      x <- as(x,"Spatial")
      x <- terra::crop(x,grast)
      layers_list <- x

    }

    if (grepl("gdb|gpkg",dsn,ignore.case = T)) {
    layers_list <- lapply(layer,
                          FUN = function(x){
                            print(x)
                            x <- sf::st_transform(sf::st_read(dsn = dsn,
                                                      layer = x),
                                              crs = st_crs(grast))

                            x <- sf::st_zm(x)
                            y <- sf::st_geometry_type(x)

                            if (length(unique(sf::st_is_valid(x))) > 1 || unique(sf::st_is_valid(x)) == F) {
                              x <- x[!is.na(sf::st_is_valid(x)),]
                              x <- sf::st_make_valid(x)
                            }

                            if (length(unique(y)) > 1 & gregexpr("SURFACE",unique(y))[[1]][1] == 1) {
                              x <- x[-which(y == "MULTISURFACE"),]
                            }
                            if (length(sf::st_is_empty(x)) > 0) {
                              x <- x[!sf::st_is_empty(x),]
                            }
                            x <- sf::st_crop(x,st_bbox(grast))
                          }
    )
    }
  }
  return(layers_list)
}
