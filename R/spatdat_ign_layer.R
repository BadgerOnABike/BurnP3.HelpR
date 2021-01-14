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
#' @importFrom raster raster crop
#' @importFrom sf st_read st_transform st_geometry_type st_is_empty st_zm st_make_valid
#' @importFrom rgdal ogrListLayers
#' 
#' @return List
#' @export
#'
#'
#' @examples
#'
#' ## Load example data
#' ref_grid <- raster(system.file("extdata/fuel.tif",package = "BurnP3.HelpR"))
#'
#' out <- spatdat_ign_layer(reference_grid = ref_grid,
#'                          layer = "road",
#'                          dsn = system.file("extdata",package="BurnP3.HelpR"))
#'
spatdat_ign_layer <- function(reference_grid,layer,dsn){

  if ( grepl("RasterLayer", class(reference_grid)) ) {grast <- reference_grid}
  if ( grepl("character", class(reference_grid)) ) {grast <- raster::raster(reference_grid)}
  if ( !grepl("RasterLayer|character", class(reference_grid)) ) {message("Reference Grid must be the directory of the raster or a raster object.")}


  if (class(layer) != "character") {
    x <- sf::st_transform(layer,crs = sp::CRS(sp::proj4string(grast)))
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

      x <- sf::st_transform(sf::read_sf(x),
                        crs = sp::CRS(sp::proj4string(grast)))

      x <- sf::st_zm(x)
      y <- sf::st_geometry_type(x)
      if (length(unique(y)) > 1 & gregexpr("SURFACE",unique(y))[[1]][1] == -1) {
        x <- x[-which(y == "MULTISURFACE"),]
      }
      if (length(sf::st_is_empty(x)) > 0) {
        x <- x[!sf::st_is_empty(x),]
      }
      x <- as(x,"Spatial")
      x <- raster::crop(x,grast)
      layers_list <- x

    }

    if (grepl("gdb|gpkg",dsn,ignore.case = T)) {
    layers_list <- lapply(raster::ogrListLayers(dsn = dsn)[grep(layer,
                                                        raster::ogrListLayers(dsn = dsn),
                                                        ignore.case = T)],
                          FUN = function(x){
                            print(x)
                            x <- sf::st_transform(sf::read_sf(dsn = dsn,
                                                      layer = x),
                                              crs = sp::CRS(sp::proj4string(grast)))

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
                            x <- as(x,"Spatial")
                            x <- raster::crop(x,grast)
                          }
    )
    }
  }
  return(layers_list)
}
